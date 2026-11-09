// SPDX-License-Identifier: MPL-2.0
// Dump kyq's parse of each file in the canonical form Hypatia.KyamlDump
// writes, one JSON line per file, for the kyaml-conformance differential:
//
//   bun test/fixtures/kyaml/kyq_dump.mjs FILE...
//
// kyq is read from a checkout at `.kyq` in the repository root, where the
// kyaml-conformance job puts it (a symlink works locally). The imports are
// static, so the script never loads code named by its input.
//
// node   = ["map", line, col, [[key, value], ...]] | ["seq", line, col, [node, ...]]
//        | [kind, line, col, value]   (int: decimal string; float: its source text)
// result = {"file": FILE, "docs": [node, ...]} | {"file": FILE, "error": [code, line, col, path]}
// Aliases are seen through. A document over kyq's node budget is ["budget"].
import { readFileSync } from "node:fs";
import { assertNodeBudget } from "../../../.kyq/conformance/src/bridge.js";
import { NODE_BUDGET, parseKyaml } from "../../../.kyq/conformance/src/kyaml.js";

const files = process.argv.slice(2);

const dump = (n) =>
  n.kind === "map"
    ? ["map", n.loc.line, n.loc.column, n.entries.map((e) => [dump(e.key), dump(e.value)])]
    : n.kind === "seq"
      ? ["seq", n.loc.line, n.loc.column, n.items.map(dump)]
      : [n.kind, n.loc.line, n.loc.column, n.kind === "int" ? n.value.toString() : n.kind === "float" ? n.raw : n.value];

for (const file of files) {
  let out;
  try {
    const docs = parseKyaml(readFileSync(file), file).map((doc) => {
      try {
        assertNodeBudget(doc, NODE_BUDGET);
        return dump(doc);
      } catch {
        return ["budget"];
      }
    });
    out = { file, docs };
  } catch (e) {
    if (!e.code) throw e;
    out = { file, error: [e.code, e.line ?? null, e.column ?? null, e.path ?? ""] };
  }
  console.log(JSON.stringify(out));
}
