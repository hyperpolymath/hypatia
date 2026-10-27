# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Deed do
  @moduledoc """
  Reader for DEED documents (`metadatastician/standards`
  `1-formats/deed/spec/abnf/deed.abnf` v1.2.1) and for the `(egress …)` clause
  profile (`1-formats/deed/spec/abnf/clauses/egress.abnf` v1.0.0, ADR-009).

  This is a *reader*, not the conformance gate. `deed_lint.js` in standards is
  the gate, and runs in every repo's `deed-conformance` lane. The reader
  produces the same tree the gate does, so a consumer can read a clause:

      {head, items}
      item  = {:field, keyword, value} | {:clause, symbol, [item]}
      value = {:string, s} | {:symbol, s} | {:integer, n} | {:boolean, b}
            | {:uuid5, s} | {:uuid8, s} | {:quoted, value} | {:list, [value]}

  It accepts a deed only when both the normative grammar and the gate would:
  `deed.abnf`, plus the gate's semantic checks (one string `:schema-version`,
  a non-blank SPDX payload, a `#u8` domain). Where the two differ, the
  stricter reading wins, so a deed the reader refuses only ever declares
  nothing. It reports the reason. v1.2.1 (metadatastician/standards#1359)
  made the grammar and the gate agree; an apg-js run of `deed.abnf` and this
  reader agree on 20,043 mutated deeds, semantic checks apart.
  """

  @doc "Parse deed source text into `{:ok, {head, items}}` or `{:error, reason}`."
  def parse(text) when is_binary(text) do
    with true <- String.valid?(text) || {:error, "a deed is UTF-8"},
         {:ok, rest} <- header(text),
         {:ok, {head, items}, rest} <- form(skip(rest)),
         "" <- skip(rest),
         :ok <- schema_version(items) do
      {:ok, {head, items}}
    else
      {:error, _} = err -> err
      rest when is_binary(rest) -> {:error, "trailing content after the form"}
    end
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
  `%{workflow: path, job: id, secrets: MapSet}`, `:none` when the repo has no
  deed, or `{:error, deed_file, reason}` when the deed or the clause cannot be
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
             {:ok, {head, items}} <- parse(text),
             # Filename dispatch: <stem>_chora.deed parses as a repo-deed.
             true <-
               head == "repo-deed" ||
                 {:error, "doc-head/filename mismatch: #{rel} must be a repo-deed, not #{head}"},
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
          job = %{workflow: workflow, job: job, secrets: MapSet.new(secrets)}
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
         true <- head in ~w(estate-deed repo-deed estate-atlas-deed praxis-deed) || :bad_head,
         {:ok, items, rest} <- items(rest, []) do
      {:ok, {head, items}, rest}
    else
      :bad_head -> {:error, "invalid doc-head"}
      {:error, _} = err -> err
    end
  end

  defp form(_), do: {:error, "a deed form must start with '('"}

  # *(token-sep (field / clause)) [token-sep] ")" — every item is preceded by
  # a separator; only the closing paren may follow without one.
  defp items(text, acc) do
    case skip(text) do
      ")" <> rest -> {:ok, Enum.reverse(acc), rest}
      "" -> {:error, "unbalanced parens"}
      ^text -> {:error, "expected a separator before the next field or clause"}
      next -> with {:ok, item, rest} <- item(next), do: items(rest, [item | acc])
    end
  end

  # field = keyword token-sep value. As the gate does, a field's value may not
  # be a boolean spelled as a word: booleans are #t and #f.
  defp item(":" <> rest) do
    with {:ok, kw, rest} <- symbol(rest),
         {:ok, rest} <- separator(rest, ":#{kw}"),
         {:ok, value, rest} <- value(rest) do
      case value do
        {:symbol, word} when word in ~w(true false yes no) ->
          {:error, ":#{kw} #{word} is not a boolean: write #t or #f"}

        _ ->
          {:ok, {:field, kw, value}, rest}
      end
    end
  end

  defp item("(" <> rest) do
    with {:ok, head, rest} <- symbol(rest),
         {:ok, inner, rest} <- items(rest, []) do
      {:ok, {:clause, head, inner}, rest}
    end
  end

  defp item(_), do: {:error, "expected a field or a clause"}

  defp separator(text, after_what) do
    case skip(text) do
      ^text -> {:error, "#{after_what} must be followed by a separator"}
      rest -> {:ok, rest}
    end
  end

  defp value("\"" <> rest), do: string(rest, [])
  defp value("#u5\"" <> rest), do: tagged(:uuid5, string(rest, []))
  defp value("#u8\"" <> rest), do: uuid8(string(rest, []))
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
