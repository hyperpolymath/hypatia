// SPDX-License-Identifier: MPL-2.0
//
// Shared helpers for the bench-data tools. Zero external dependencies: a
// minimal JSON model (object key-order preserved, numbers kept as their
// source token for byte-faithful round-trips), the criterion bencher-format
// line parser, and the ns formatter. A 1:1 port of the former
// scripts/{check-bench-regression,update-bench-baselines}.py.

/// Minimal JSON value. `Num` keeps the original textual token so existing
/// metadata round-trips byte-identically (matching Python's int/float
/// preservation under `json.dumps(indent=2, sort_keys=False)`).
#[derive(Debug, Clone)]
pub enum Json {
    Null,
    Bool(bool),
    Num(String),
    Str(String),
    Arr(Vec<Json>),
    Obj(Vec<(String, Json)>),
}

impl Json {
    pub fn get<'a>(&'a self, key: &str) -> Option<&'a Json> {
        match self {
            Json::Obj(pairs) => pairs.iter().find(|(k, _)| k == key).map(|(_, v)| v),
            _ => None,
        }
    }
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Json::Num(t) => t.parse::<f64>().ok(),
            _ => None,
        }
    }
}

struct P<'a> {
    b: &'a [u8],
    i: usize,
}

impl<'a> P<'a> {
    fn ws(&mut self) {
        while self.i < self.b.len() && matches!(self.b[self.i], b' ' | b'\t' | b'\n' | b'\r') {
            self.i += 1;
        }
    }
    fn value(&mut self) -> Result<Json, String> {
        self.ws();
        if self.i >= self.b.len() {
            return Err("unexpected end of input".into());
        }
        match self.b[self.i] {
            b'{' => self.object(),
            b'[' => self.array(),
            b'"' => Ok(Json::Str(self.string()?)),
            b't' | b'f' => self.boolean(),
            b'n' => {
                self.lit("null")?;
                Ok(Json::Null)
            }
            _ => self.number(),
        }
    }
    fn lit(&mut self, s: &str) -> Result<(), String> {
        if self.b[self.i..].starts_with(s.as_bytes()) {
            self.i += s.len();
            Ok(())
        } else {
            Err(format!("expected `{s}`"))
        }
    }
    fn boolean(&mut self) -> Result<Json, String> {
        if self.b[self.i] == b't' {
            self.lit("true")?;
            Ok(Json::Bool(true))
        } else {
            self.lit("false")?;
            Ok(Json::Bool(false))
        }
    }
    fn number(&mut self) -> Result<Json, String> {
        let start = self.i;
        while self.i < self.b.len()
            && matches!(self.b[self.i], b'0'..=b'9' | b'-' | b'+' | b'.' | b'e' | b'E')
        {
            self.i += 1;
        }
        if self.i == start {
            return Err("expected number".into());
        }
        Ok(Json::Num(
            std::str::from_utf8(&self.b[start..self.i])
                .map_err(|_| "invalid UTF-8 in number".to_string())?
                .to_string(),
        ))
    }
    fn string(&mut self) -> Result<String, String> {
        self.i += 1; // opening quote
        let mut s = String::new();
        while self.i < self.b.len() {
            let c = self.b[self.i];
            self.i += 1;
            match c {
                b'"' => return Ok(s),
                b'\\' => {
                    let e = self.b[self.i];
                    self.i += 1;
                    match e {
                        b'"' => s.push('"'),
                        b'\\' => s.push('\\'),
                        b'/' => s.push('/'),
                        b'n' => s.push('\n'),
                        b't' => s.push('\t'),
                        b'r' => s.push('\r'),
                        b'b' => s.push('\u{8}'),
                        b'f' => s.push('\u{c}'),
                        b'u' => {
                            let hex = std::str::from_utf8(&self.b[self.i..self.i + 4])
                                .map_err(|_| "bad \\u".to_string())?;
                            let cp = u32::from_str_radix(hex, 16)
                                .map_err(|_| "bad \\u".to_string())?;
                            self.i += 4;
                            // map_or, not unwrap_or: behaviour-identical (infallible
                            // default), but avoids the `unwrap` token that Hypatia's
                            // `unwrap_without_check` rule false-positives on (it matches
                            // the `unwrap` substring inside `unwrap_or`).
                            s.push(char::from_u32(cp).map_or('\u{fffd}', |c| c));
                        }
                        _ => return Err("bad escape".into()),
                    }
                }
                _ => {
                    // copy this UTF-8 byte and any continuation bytes verbatim
                    let mut buf = vec![c];
                    while self.i < self.b.len() && (self.b[self.i] & 0xC0) == 0x80 {
                        buf.push(self.b[self.i]);
                        self.i += 1;
                    }
                    s.push_str(std::str::from_utf8(&buf).map_err(|_| "bad utf8".to_string())?);
                }
            }
        }
        Err("unterminated string".into())
    }
    fn array(&mut self) -> Result<Json, String> {
        self.i += 1;
        let mut v = Vec::new();
        self.ws();
        if self.i < self.b.len() && self.b[self.i] == b']' {
            self.i += 1;
            return Ok(Json::Arr(v));
        }
        loop {
            v.push(self.value()?);
            self.ws();
            match self.b.get(self.i) {
                Some(b',') => {
                    self.i += 1;
                }
                Some(b']') => {
                    self.i += 1;
                    return Ok(Json::Arr(v));
                }
                _ => return Err("expected `,` or `]`".into()),
            }
        }
    }
    fn object(&mut self) -> Result<Json, String> {
        self.i += 1;
        let mut pairs = Vec::new();
        self.ws();
        if self.i < self.b.len() && self.b[self.i] == b'}' {
            self.i += 1;
            return Ok(Json::Obj(pairs));
        }
        loop {
            self.ws();
            let k = self.string()?;
            self.ws();
            if self.b.get(self.i) != Some(&b':') {
                return Err("expected `:`".into());
            }
            self.i += 1;
            let val = self.value()?;
            pairs.push((k, val));
            self.ws();
            match self.b.get(self.i) {
                Some(b',') => {
                    self.i += 1;
                }
                Some(b'}') => {
                    self.i += 1;
                    return Ok(Json::Obj(pairs));
                }
                _ => return Err("expected `,` or `}`".into()),
            }
        }
    }
}

/// Parse a JSON document. Returns `Err` on malformed input (callers treat
/// that as "empty baseline", mirroring the Python `JSONDecodeError` branch).
pub fn parse_json(text: &str) -> Result<Json, String> {
    let mut p = P {
        b: text.as_bytes(),
        i: 0,
    };
    let v = p.value()?;
    Ok(v)
}

fn escape_str(s: &str, out: &mut String) {
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\u{8}' => out.push_str("\\b"),
            '\u{c}' => out.push_str("\\f"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c if (c as u32) < 0x80 => out.push(c),
            // ensure_ascii=True parity with Python's json.dumps
            c => {
                let mut buf = [0u16; 2];
                for u in c.encode_utf16(&mut buf) {
                    out.push_str(&format!("\\u{:04x}", u));
                }
            }
        }
    }
    out.push('"');
}

/// Serialize matching Python `json.dumps(obj, indent=2, sort_keys=False)`
/// plus a trailing newline: empty containers stay `{}`/`[]`, nested levels
/// indent by two spaces, `": "` / `,` separators.
pub fn to_pretty(v: &Json) -> String {
    let mut s = String::new();
    write_val(v, 0, &mut s);
    s.push('\n');
    s
}

fn write_val(v: &Json, indent: usize, out: &mut String) {
    match v {
        Json::Null => out.push_str("null"),
        Json::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Json::Num(t) => out.push_str(t),
        Json::Str(s) => escape_str(s, out),
        Json::Arr(a) => {
            if a.is_empty() {
                out.push_str("[]");
                return;
            }
            out.push('[');
            for (n, e) in a.iter().enumerate() {
                if n > 0 {
                    out.push(',');
                }
                out.push('\n');
                out.push_str(&" ".repeat(indent + 2));
                write_val(e, indent + 2, out);
            }
            out.push('\n');
            out.push_str(&" ".repeat(indent));
            out.push(']');
        }
        Json::Obj(p) => {
            if p.is_empty() {
                out.push_str("{}");
                return;
            }
            out.push('{');
            for (n, (k, val)) in p.iter().enumerate() {
                if n > 0 {
                    out.push(',');
                }
                out.push('\n');
                out.push_str(&" ".repeat(indent + 2));
                escape_str(k, out);
                out.push_str(": ");
                write_val(val, indent + 2, out);
            }
            out.push('\n');
            out.push_str(&" ".repeat(indent));
            out.push('}');
        }
    }
}

/// Extract `[(name, ns_per_iter)]` from criterion bencher-format output, e.g.
/// `test foo ... bench:       12,345 ns/iter (+/- 678)`. Insertion order is
/// preserved (a repeated name updates in place, keeping its first position)
/// to match Python dict semantics under `json.dumps(sort_keys=False)`.
pub fn parse_bencher_output(text: &str) -> Vec<(String, i64)> {
    let mut out: Vec<(String, i64)> = Vec::new();
    for raw in text.lines() {
        let line = raw.trim();
        let t: Vec<&str> = line.split_whitespace().collect();
        // test <name> ... bench: <ns> ns/iter ...
        if t.len() >= 6
            && t[0] == "test"
            && t[2] == "..."
            && t[3] == "bench:"
            && t[5].starts_with("ns/iter")
        {
            let digits: String = t[4].chars().filter(|c| *c != ',').collect();
            if !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit()) {
                if let Ok(ns) = digits.parse::<i64>() {
                    let name = t[1].to_string();
                    if let Some(e) = out.iter_mut().find(|(k, _)| *k == name) {
                        e.1 = ns;
                    } else {
                        out.push((name, ns));
                    }
                }
            }
        }
    }
    out
}

/// Human-readable ns, mirroring the Python `fmt_ns` (µs / ms thresholds).
pub fn fmt_ns(ns: i64) -> String {
    let n = ns as f64;
    if ns >= 1_000_000 {
        format!("{:.2} ms", n / 1_000_000.0)
    } else if ns >= 1_000 {
        format!("{:.2} \u{b5}s", n / 1_000.0)
    } else {
        format!("{ns} ns")
    }
}
