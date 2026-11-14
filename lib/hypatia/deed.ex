# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Deed do
  @moduledoc """
  Reader for DEED documents (`metadatastician/standards`
  `1-formats/deed/spec/abnf/deed.abnf` v2.0.0) and for the `(egress …)` clause
  profile (`1-formats/deed/spec/abnf/clauses/egress.abnf` v1.0.0, ADR-009).

  This is a *reader*, not the conformance gate. `deed_lint.js` in standards is
  the gate, and runs in every repo's `deed-conformance` lane. The reader
  produces the same tree the gate does, so a consumer can read a clause:

      {head, items}
      item  = {:field, keyword, value} | {:field, keyword, value, :bang}
            | {:clause, symbol, [item]}
      value = {:string, s} | {:symbol, s} | {:integer, n} | {:boolean, b}
            | {:uuid5, s} | {:uuid8, s} | {:uuid, s} | {:quoted, value} | {:list, [value]}

  `{:field, "id", value, :bang}` is a rule marked blocking (A7 J17): the only
  place the grammar's bang token may stand.

  It accepts a deed only when both the normative grammar and the gate's core
  check (`deed_lint.js FILE`) would. That is `deed.abnf`, plus the gate's
  semantic checks: one string `:schema-version`; a non-blank SPDX payload; a
  `#u8` domain; no line over 998 octets (v1.3.0); `!` only after the `:id` of
  a `(rule …)` clause at the top level of a contractile-deed (v1.5.0); no `#u5`,
  and no `#uuid` other than version 8, in an identity field (owner ruling Q2,
  CHORA-SPEC 7.1), read lexically as the gate reads it; and, given a file
  name (`parse/2`), the filename dispatch to a doc-head (`dispatch/1`). Where
  the grammar and the gate differ, the stricter reading wins, so a deed the
  reader refuses only ever declares nothing. It reports the reason. At
  standards b2aaeb0 this reader and `deed_lint.js FILE` gave the same verdict
  on all 343 standards fixtures and on 4,000 mutants of the core ones.

  The clause profiles the gate runs under its own flags (`--anchor`,
  `--contractiles`, `--edict`, `--verb-descriptile`, `--ecosystem`, `--state`,
  `--rsr-scorecard`, `--mustfile-json`, `--proof-receipt-json`) are not
  checked here; `(egress …)` is the one profile this module reads
  (`egress_declarations/1`). `test/deed_conformance_test.exs` runs the
  standards core fixtures against it.
  """

  # deed.abnf v2.0.0 doc-head. v1.4.0 added contractile-deed and
  # descriptile-deed (A7 J14); v2.0.0 removed anchor-deed (A7 S1, J24).
  @doc_heads ~w(estate-deed repo-deed estate-atlas-deed praxis-deed contractile-deed descriptile-deed)

  # A7 J14: exact, case-sensitive file names. A file in capitals says how it
  # is; a Capitalised file says how it must be (A7 YL4).
  @contractile_files MapSet.new(~w(Intent Must Trust Adjust Dust Bust), &"#{&1}file.deed")
  @descriptile_files MapSet.new(
                       ~w(META STATE ECOSYSTEM AGENTIC NEUROSYM PLAYBOOK CLADE INTENT MUST TRUST ADJUST DUST BUST),
                       &"#{&1}.deed"
                     )

  # Identity fields (CHORA-SPEC 7.1), the gate's IDENTITY_FIELDS and the field
  # names scripts/check-uuid-v8.sh reads.
  @identity_fields ~w(repo-uuid estate-uuid forge-uuid beholding-chora of-chora uuid estate base forge repo)

  @max_line_octets 998

  @doc """
  Parse deed source text into `{:ok, {head, items}}` or `{:error, reason}`.

  Given `filename` (a path or a base name), the doc-head must also be the one
  that name dispatches to (`dispatch/1`), as the gate checks every file it is
  given. Without one, only the text is judged.
  """
  def parse(text, filename \\ nil)

  def parse(text, filename) when is_binary(text) do
    with true <- String.valid?(text) || {:error, "a deed is UTF-8"},
         :ok <- line_lengths(text),
         {:ok, rest} <- header(text),
         {:ok, {head, items}, rest} <- form(skip(rest)),
         "" <- skip(rest),
         :ok <- schema_version(items),
         :ok <- identity_lexical(text),
         :ok <- dispatch_matches(filename, head) do
      {:ok, {head, items}}
    else
      {:error, _} = err -> err
      rest when is_binary(rest) -> {:error, "trailing content after the form"}
    end
  end

  @doc """
  The doc-head a deed file name dispatches to (deed.abnf `deed-filename`):
  `{:ok, head}` or `{:error, reason}`. Exact names first, so
  `estate_chora.deed` is the estate deed and never a repo deed; then the J14
  contractile and descriptile names, which are case-sensitive; then the
  `_praxis.deed` and `_chora.deed` suffixes, split on the final suffix so a
  stem may hold dots. Either stem must be non-empty and hold only ASCII
  letters, digits, `.`, `_` and `-`.
  """
  def dispatch(filename) when is_binary(filename) do
    base = Path.basename(filename)

    cond do
      base == "estate_chora.deed" -> {:ok, "estate-deed"}
      base == "ATLAS.deed" -> {:ok, "estate-atlas-deed"}
      base == "0-AI-MANIFEST.deed" -> {:ok, "repo-deed"}
      MapSet.member?(@contractile_files, base) -> {:ok, "contractile-deed"}
      MapSet.member?(@descriptile_files, base) -> {:ok, "descriptile-deed"}
      String.ends_with?(base, "_praxis.deed") -> stem(base, "_praxis.deed", "praxis-deed")
      String.ends_with?(base, "_chora.deed") -> stem(base, "_chora.deed", "repo-deed")
      true -> {:error, "filename #{inspect(base)} matches no deed dispatch pattern"}
    end
  end

  # praxis-file = stem "_praxis.deed"; repo-file = stem "_chora.deed";
  # stem = 1*( ALPHA / DIGIT / "-" / "." / "_" ). Both suffixes take the same
  # stem rule: non-empty, ASCII letters, digits, ".", "_" and "-" only.
  defp stem(base, suffix, head) do
    stem = binary_part(base, 0, byte_size(base) - byte_size(suffix))

    if Regex.match?(~r/\A[A-Za-z0-9._-]+\z/, stem),
      do: {:ok, head},
      else: {:error, "illegal deed filename stem: #{base}"}
  end

  defp dispatch_matches(nil, _head), do: :ok

  defp dispatch_matches(filename, head) do
    case dispatch(filename) do
      {:ok, ^head} ->
        :ok

      {:ok, want} ->
        {:error,
         "doc-head/filename mismatch: #{Path.basename(filename)} dispatches to #{want} " <>
           "but parses as #{head}"}

      err ->
        err
    end
  end

  # SEMANTIC (deed.abnf v1.3.0): no line is longer than 998 octets, counted in
  # UTF-8 bytes before its line-end (CRLF or LF). The last line counts too.
  defp line_lengths(text) do
    text
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.find_value(:ok, fn {line, n} ->
      octets = byte_size(String.trim_trailing(line, "\r"))

      if octets > @max_line_octets,
        do: {:error, "line #{n} is #{octets} octets; a deed line is at most 998"}
    end)
  end

  # form: exactly one :schema-version field, with a string value.
  defp schema_version(items) do
    case for({:field, "schema-version", v} <- items, do: v) do
      [{:string, _}] ->
        :ok

      [_] ->
        {:error, ":schema-version must be a string"}

      found ->
        {:error, "the form must carry exactly one :schema-version (found #{length(found)})"}
    end
  end

  # The identity rule read lexically, as the gate and check-uuid-v8.sh read it
  # (owner rulings Q2 and "by name", 2026-11-14): an identity field name, then
  # whitespace, then #u5"…" or a #uuid"…" that is not version 8, is an error
  # wherever it stands, comments and strings included. Two views are read, the
  # raw text and the text with comments blanked, so a comment between a field
  # and its value cannot hide the value.
  @identity_alt Enum.join(@identity_fields, "|")
  @u5_lexical Regex.compile!(":(#{@identity_alt})\\s+#u5\"[^\"]*\"", "u")
  @uuid_lexical Regex.compile!(
                  ":(#{@identity_alt})\\s+#uuid\"[0-9a-f]{8}-[0-9a-f]{4}-([0-9a-f])[0-9a-f]{3}-[0-9a-f]{4}-[0-9a-f]{12}\"",
                  "u"
                )

  defp identity_lexical(text) do
    Enum.find_value([text, blank_comments(text)], :ok, fn view ->
      cond do
        match = Regex.run(@u5_lexical, view) ->
          {:error, "identity field :#{Enum.at(match, 1)} carries a #u5 literal"}

        version = Enum.find(Regex.scan(@uuid_lexical, view), &(Enum.at(&1, 2) != "8")) ->
          {:error,
           "identity field :#{Enum.at(version, 1)} carries a version #{Enum.at(version, 2)} id; " <>
             "an identity field carries version 8"}

        true ->
          nil
      end
    end)
  end

  # Each ";" comment outside a string becomes spaces, to its line-end.
  defp blank_comments(text), do: blank(text, false, [])

  defp blank("", _in_string, acc), do: acc |> Enum.reverse() |> IO.iodata_to_binary()

  defp blank(<<?\\, c::utf8, rest::binary>>, true, acc),
    do: blank(rest, true, [<<c::utf8>>, ?\\ | acc])

  defp blank(<<?", rest::binary>>, in_string, acc), do: blank(rest, not in_string, [?" | acc])

  defp blank(<<?;, rest::binary>>, false, acc) do
    {comment, rest} =
      case :binary.split(rest, "\n") do
        [comment, tail] -> {comment, "\n" <> tail}
        [comment] -> {comment, ""}
      end

    blank(rest, false, [String.duplicate(" ", String.length(comment) + 1) | acc])
  end

  defp blank(<<c::utf8, rest::binary>>, in_string, acc),
    do: blank(rest, in_string, [<<c::utf8>> | acc])

  @doc """
  The repo deed at the root of `repo_path`: `{:ok, path}`, `:none`, or
  `{:error, reason}` when there is more than one (one deed per repo).
  `estate_chora.deed` is the estate deed, never a repo deed.
  """
  def repo_deed_path(repo_path) do
    case File.ls(repo_path) do
      {:ok, names} ->
        names
        |> Enum.filter(&(String.ends_with?(&1, "_chora.deed") and &1 != "estate_chora.deed"))
        |> Enum.sort()
        |> case do
          [] ->
            :none

          [one] ->
            # stem = 1*( ALPHA / DIGIT / "-" / "." / "_" ), as deed.abnf has it.
            if Regex.match?(~r/\A[A-Za-z0-9._-]+_chora\.deed\z/, one),
              do: {:ok, Path.join(repo_path, one)},
              else: {:error, "illegal deed filename stem: #{one}"}

          many ->
            {:error, "more than one repo deed: #{Enum.join(many, ", ")}"}
        end

      {:error, _} ->
        :none
    end
  end

  @doc """
  The jobs declared in the repo deed's `(egress …)` clause.

  Returns `{:ok, deed_file, jobs}` where each job is
  `%{workflow: path, job: id, secrets: MapSet, endpoints: [string], enforcement: name}`, `:none` when
  the repo has no deed, or `{:error, deed_file, reason}` when the deed or the clause cannot be
  read. A deed without the clause declares nothing: `{:ok, deed_file, []}`.
  """
  def egress_declarations(repo_path) do
    case repo_deed_path(repo_path) do
      :none ->
        :none

      {:error, reason} ->
        {:error, nil, reason}

      {:ok, path} ->
        rel = Path.relative_to(path, repo_path)

        with {:ok, text} <- File.read(path),
             # Filename dispatch: <stem>_chora.deed parses as a repo-deed.
             {:ok, {head, items}} <- parse(text, path),
             {:ok, jobs} <- egress_jobs(head, items) do
          {:ok, rel, jobs}
        else
          {:error, reason} when is_binary(reason) -> {:error, rel, reason}
          {:error, posix} -> {:error, rel, "cannot read: #{posix}"}
        end
    end
  end

  # ─── (egress …) profile ─────────────────────────────────────────────

  @required ~w(workflow job secrets endpoints enforcement)
  @allowed MapSet.new(["reason" | @required])
  @enforcement ~w(none harden-runner-audit harden-runner-block)
  # Every "." follows a non-empty name segment, as workflow-path in egress.abnf.
  @workflow ~r/\A\.github\/workflows\/(?:[A-Za-z0-9_-]+\.)+ya?ml\z/
  @job_id ~r/\A[A-Za-z_][A-Za-z0-9_-]*\z/
  # The stored form: GitHub keeps secret and variable names uppercase and
  # matches references to them without regard to case.
  @secret ~r/\A[A-Z_][A-Z0-9_]*\z/
  # Labels start and end with a letter or digit (RFC 1123 §2.1).
  @host_port ~r/\A(\*\.)?[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?(\.[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?)*(?::(?<port>[0-9]{1,5}))?\z/

  defp egress_jobs(head, items) do
    # The clause counts only at the top level; one nested deeper would declare
    # nothing without saying so, so it is refused.
    nested? =
      Enum.any?(items, fn item ->
        not match?({:clause, "egress", _}, item) and egress_below?(item)
      end)

    case for({:clause, "egress", members} <- items, do: members) do
      _ when nested? -> {:error, "(egress …) nested inside another clause"}
      [] -> {:ok, []}
      [_ | [_ | _]] -> {:error, "more than one (egress …) clause"}
      [_] when head != "repo-deed" -> {:error, "(egress …) outside a repo-deed"}
      [members] -> egress_members(members)
    end
  end

  defp egress_below?({:clause, _, inner}),
    do: Enum.any?(inner, &(match?({:clause, "egress", _}, &1) or egress_below?(&1)))

  defp egress_below?(_), do: false

  defp egress_members(members) do
    if Enum.count(members, &match?({:field, "policy", _}, &1)) > 1,
      do: {:error, "(egress …) repeats :policy"},
      else: egress_jobs_of(members)
  end

  defp egress_jobs_of(members) do
    members
    |> Enum.reduce_while({:ok, []}, fn
      {:field, "policy", {:string, _}}, acc -> {:cont, acc}
      {:clause, "job", fields}, {:ok, jobs} -> egress_job(fields, jobs)
      other, _ -> {:halt, {:error, "(egress …) cannot hold #{describe(other)}"}}
    end)
    |> case do
      {:ok, jobs} ->
        keys = Enum.map(jobs, &{&1.workflow, &1.job})

        if length(Enum.uniq(keys)) == length(keys),
          do: {:ok, Enum.reverse(jobs)},
          else: {:error, "(egress …) lists the same workflow job twice"}

      err ->
        err
    end
  end

  defp egress_job(fields, jobs) do
    by_key = for {:field, k, v} <- fields, into: %{}, do: {k, v}

    cond do
      Enum.any?(fields, &(not match?({:field, _, _}, &1))) ->
        {:halt, {:error, "(job …) holds a nested clause"}}

      length(fields) != map_size(by_key) ->
        {:halt, {:error, "(job …) repeats a field"}}

      (unknown = Enum.reject(Map.keys(by_key), &MapSet.member?(@allowed, &1))) != [] ->
        {:halt, {:error, "(job …) has no field :#{hd(unknown)}"}}

      (missing = Enum.reject(@required, &Map.has_key?(by_key, &1))) != [] ->
        {:halt, {:error, "(job …) is missing :#{hd(missing)}"}}

      true ->
        with {:string, workflow} <- by_key["workflow"],
             true <- Regex.match?(@workflow, workflow),
             {:string, job} <- by_key["job"],
             true <- Regex.match?(@job_id, job),
             {:ok, secrets} <- strings(by_key["secrets"]),
             true <- valid_secrets?(secrets),
             {:ok, endpoints} <- strings(by_key["endpoints"]),
             true <- Enum.all?(endpoints, &valid_endpoint?(&1, secrets)),
             {:symbol, enf} when enf in @enforcement <- by_key["enforcement"],
             true <- reason_ok?(by_key["reason"]) do
          job = %{
            workflow: workflow,
            job: job,
            secrets: MapSet.new(secrets),
            endpoints: endpoints,
            enforcement: enf
          }

          {:cont, {:ok, [job | jobs]}}
        else
          _ ->
            {:halt, {:error, "(job …) has a field of the wrong shape (standards egress.abnf)"}}
        end
    end
  end

  defp valid_secrets?(secrets) do
    secrets != [] and "GITHUB_TOKEN" not in secrets and
      Enum.all?(secrets, &Regex.match?(@secret, &1)) and
      length(Enum.uniq(secrets)) == length(secrets)
  end

  defp valid_endpoint?("secret:" <> name, secrets), do: name in secrets
  defp valid_endpoint?("var:" <> name, _secrets), do: Regex.match?(@secret, name)

  defp valid_endpoint?(endpoint, _secrets) do
    case Regex.named_captures(@host_port, endpoint) do
      nil -> false
      %{"port" => port} -> port_ok?(port)
    end
  end

  defp port_ok?(port) when port in [nil, ""], do: true
  defp port_ok?(port), do: String.to_integer(port) in 1..65_535

  defp reason_ok?(nil), do: true
  defp reason_ok?({:string, _}), do: true
  defp reason_ok?(_), do: false

  defp strings({:list, values}) do
    if Enum.all?(values, &match?({:string, _}, &1)),
      do: {:ok, Enum.map(values, &elem(&1, 1))},
      else: :error
  end

  defp strings(_), do: :error

  defp describe({:field, k, _}), do: ":#{k}"
  defp describe({:clause, s, _}), do: "(#{s} …)"

  # ─── Lexical layer ──────────────────────────────────────────────────

  # text-char = VCHAR / SP / UTF8-2 / UTF8-3 / UTF8-4; line-end = CRLF / LF.
  # The input is valid UTF-8 (checked in parse/1), so a code point at or above
  # U+0080 is one of the UTF8-n forms.
  @text_chars "[\\x{20}-\\x{7E}\\x{80}-\\x{10FFFF}]"
  @spdx_line Regex.compile!("\\A;; SPDX-(#{@text_chars}+)(?:\\r\\n|\\n)", "u")
  @comment_tail Regex.compile!("\\A#{@text_chars}*(?:\\r\\n|\\n)", "u")

  # header = 1*spdx-line; spdx-line = ";;" SP "SPDX-" 1*text-char line-end.
  # The gate also refuses a payload that is only spaces.
  defp header(text), do: spdx_lines(text, 0)

  defp spdx_lines(text, n) do
    case Regex.run(@spdx_line, text) do
      [line, payload] ->
        if String.trim(payload) == "",
          do: {:error, "SPDX header line is empty after ';; SPDX-'"},
          else: spdx_lines(drop(text, line), n + 1)

      nil when n == 0 ->
        {:error, "no ';; SPDX-' header line"}

      nil ->
        {:ok, text}
    end
  end

  # token-sep = 1*(SP / line-end / comment); comment = ";" *text-char line-end.
  # A bare CR, a tab, or a comment without its line-end is not a separator;
  # skip/1 stops there and the caller reports what it cannot read.
  defp skip(" " <> rest), do: skip(rest)
  defp skip("\n" <> rest), do: skip(rest)
  defp skip("\r\n" <> rest), do: skip(rest)

  defp skip(";" <> rest = text) do
    case Regex.run(@comment_tail, rest) do
      [tail] -> skip(drop(rest, tail))
      nil -> text
    end
  end

  defp skip(rest), do: rest

  defp drop(text, prefix),
    do: binary_part(text, byte_size(prefix), byte_size(text) - byte_size(prefix))

  defp form("(" <> rest) do
    with {:ok, head, rest} <- symbol(rest),
         true <- head in @doc_heads || :bad_head,
         {:ok, items, rest} <- items(rest, {head, []}, []) do
      {:ok, {head, items}, rest}
    else
      :bad_head -> {:error, "invalid doc-head"}
      {:error, _} = err -> err
    end
  end

  defp form(_), do: {:error, "a deed form must start with '('"}

  # *(token-sep (field / clause)) [token-sep] ")" — every item is preceded by
  # a separator; only the closing paren may follow without one. `ctx` is the
  # doc-head and the open clause heads, innermost first: the bang needs both.
  defp items(text, ctx, acc) do
    case skip(text) do
      ")" <> rest -> {:ok, Enum.reverse(acc), rest}
      "" -> {:error, "unbalanced parens"}
      ^text -> {:error, "expected a separator before the next field or clause"}
      next -> with {:ok, item, rest} <- item(next, ctx), do: items(rest, ctx, [item | acc])
    end
  end

  # field = keyword token-sep value [token-sep bang]. As the gate does, a
  # field's value may not be a boolean spelled as a word: booleans are #t and #f.
  defp item(":" <> rest, ctx) do
    with {:ok, kw, rest} <- symbol(rest),
         {:ok, rest} <- separator(rest, ":#{kw}"),
         {:ok, value, rest} <- value(rest) do
      case value do
        {:symbol, word} when word in ~w(true false yes no) ->
          {:error, ":#{kw} #{word} is not a boolean: write #t or #f"}

        _ ->
          case bang(rest, kw, ctx) do
            :none -> {:ok, {:field, kw, value}, rest}
            {:ok, rest} -> {:ok, {:field, kw, value, :bang}, rest}
            {:error, _} = err -> err
          end
      end
    end
  end

  defp item("(" <> rest, {doc, stack}) do
    with {:ok, head, rest} <- symbol(rest),
         {:ok, inner, rest} <- items(rest, {doc, [head | stack]}, []) do
      {:ok, {:clause, head, inner}, rest}
    end
  end

  defp item(_, _ctx), do: {:error, "expected a field or a clause"}

  # bang = "!" (deed.abnf v1.5.0, A7 J17), after a separator. "!" may continue
  # a symbol, so it is the bang only when it stands alone: a separator, ")" or
  # the end follows it. SEMANTIC: it marks a blocking rule, and stands only
  # after the :id of a (rule …) clause at the top level of a contractile-deed.
  defp bang(rest, kw, ctx) do
    with "!" <> after_bang <- skip(rest),
         false <- rest == skip(rest),
         true <- bang_end?(after_bang) do
      if bang_allowed?(ctx, kw),
        do: {:ok, after_bang},
        else:
          {:error,
           "'!' is allowed only directly after a rule's :id value in a contractile-deed (A7 J17)"}
    else
      _ -> :none
    end
  end

  defp bang_end?(""), do: true
  defp bang_end?(<<c, _::binary>>), do: c in [?\s, ?\n, ?\r, ?;, ?)]

  defp bang_allowed?({"contractile-deed", ["rule"]}, "id"), do: true
  defp bang_allowed?(_ctx, _kw), do: false

  defp separator(text, after_what) do
    case skip(text) do
      ^text -> {:error, "#{after_what} must be followed by a separator"}
      rest -> {:ok, rest}
    end
  end

  defp value("\"" <> rest), do: string(rest, [])
  defp value("#u5\"" <> rest), do: tagged(:uuid5, string(rest, []))
  defp value("#u8\"" <> rest), do: uuid8(string(rest, []))
  defp value("#uuid\"" <> rest), do: uuid(string(rest, []))
  defp value("#t" <> rest), do: {:ok, {:boolean, true}, rest}
  defp value("#f" <> rest), do: {:ok, {:boolean, false}, rest}
  defp value("(" <> rest), do: list_open(rest)

  # quoted = "'" ( symbol / list ): no separator after the quote, and nothing
  # else may be quoted.
  defp value("'" <> rest) do
    case value(rest) do
      {:ok, {kind, _} = v, rest} when kind in [:symbol, :list] -> {:ok, {:quoted, v}, rest}
      {:ok, _, _} -> {:error, "only a symbol or a list may be quoted"}
      err -> err
    end
  end

  defp value(text) do
    case Regex.run(~r/\A-?[0-9]+/, text) do
      [digits] ->
        {:ok, {:integer, String.to_integer(digits)},
         binary_part(text, byte_size(digits), byte_size(text) - byte_size(digits))}

      nil ->
        case symbol(text) do
          {:ok, s, rest} -> {:ok, {:symbol, s}, rest}
          err -> err
        end
    end
  end

  defp tagged(tag, {:ok, {:string, s}, rest}), do: {:ok, {tag, s}, rest}
  defp tagged(_tag, err), do: err

  # uuid8 body: domain ":" name, the domain (before the first ":") non-empty
  # printable ASCII (ADR-008 profile C), as the gate checks.
  defp uuid8({:ok, {:string, s}, _} = ok) do
    if Regex.match?(~r/\A[\x21-\x39\x3B-\x7E]+:/, s),
      do: tagged(:uuid8, ok),
      else: {:error, "a #u8 body must be domain:name with a printable ASCII domain"}
  end

  defp uuid8(err), do: err

  # uuid body: an opaque id, lowercase 8-4-4-4-12 hex, version 5 or 8, variant
  # 10 (deed.abnf v1.3.0 `uuid-hex`; all of it is grammar). The reader keeps it
  # as written. Which version an identity field may carry is identity_lexical/1's.
  @uuid_hex ~r/\A[0-9a-f]{8}-[0-9a-f]{4}-[58][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}\z/
  defp uuid({:ok, {:string, s}, _} = ok) do
    if Regex.match?(@uuid_hex, s),
      do: tagged(:uuid, ok),
      else:
        {:error,
         "a #uuid body must be a lowercase 8-4-4-4-12 hex id of version 5 or 8, variant 10"}
  end

  defp uuid(err), do: err

  # list = "(" [token-sep] [value *(token-sep value)] [token-sep] ")", as
  # v1.2.1 has it: a multi-line list may start on the line after "(", as the
  # estate template writes them.
  defp list_open(text) do
    case skip(text) do
      ")" <> rest -> {:ok, {:list, []}, rest}
      next -> list(next, [])
    end
  end

  defp list(text, acc) do
    with {:ok, v, rest} <- value(text) do
      case skip(rest) do
        ")" <> after_list -> {:ok, {:list, Enum.reverse([v | acc])}, after_list}
        ^rest -> {:error, "list values must be separated"}
        next -> list(next, [v | acc])
      end
    end
  end

  # string = DQUOTE *(str-char / escape) DQUOTE; exactly four escapes.
  defp string("\"" <> rest, acc),
    do: {:ok, {:string, IO.iodata_to_binary(Enum.reverse(acc))}, rest}

  defp string("\\\"" <> rest, acc), do: string(rest, ["\"" | acc])
  defp string("\\\\" <> rest, acc), do: string(rest, ["\\" | acc])
  defp string("\\n" <> rest, acc), do: string(rest, ["\n" | acc])
  defp string("\\t" <> rest, acc), do: string(rest, ["\t" | acc])
  defp string("\\" <> _, _acc), do: {:error, "illegal string escape"}
  defp string("", _acc), do: {:error, "unterminated string"}
  # str-char = %x20-21 / %x23-5B / %x5D-7E / UTF8-2 / UTF8-3 / UTF8-4
  defp string(<<c::utf8, rest::binary>>, acc) when c in 0x20..0x7E or c >= 0x80,
    do: string(rest, [<<c::utf8>> | acc])

  defp string(<<c::utf8, _::binary>>, _acc),
    do:
      {:error, "raw control character U+#{Integer.to_string(c, 16)} in a string (use \\n or \\t)"}

  defp string(_, _acc), do: {:error, "invalid UTF-8 in a string"}

  @symbol ~r/\A[A-Za-z][A-Za-z0-9.*\/<>=!?+_-]*/
  defp symbol(text) do
    case Regex.run(@symbol, text) do
      [s] -> {:ok, s, binary_part(text, byte_size(s), byte_size(text) - byte_size(s))}
      nil -> {:error, "expected a symbol"}
    end
  end
end
