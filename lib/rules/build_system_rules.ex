# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.BuildSystemRules do
  @moduledoc """
  Build-tool configuration anti-patterns that block estate CI but slip past
  language-level scanners.

  Originating incident: affinescript#361 — three independent baselines red
  on every PR for repo-wide reasons. Two of the three (dune-test-alias
  inline field, npm @org-scoped unpublished dependency) are cross-cutting:
  they can happen in any estate repo that uses dune or npm-package
  scaffolding, and the existing language rules in `CodeSafety` /
  `CicdRules` do not catch them.

  Rules detect at file-content level. Caller is `Hypatia.Rules.scan_file/3`.
  """

  @doc """
  Scan a `dune` build-config file for the dune 3.x deprecated
  `(test ... (alias <name>))` inline-field anti-pattern.

  Under dune ≥3.0 the `(test ...)` stanza does not accept `(alias <name>)`
  as a child field; the build aborts with `Error: Unknown field "alias"`.
  The canonical replacement is to use `(executable ...)` plus a dedicated
  `(rule (alias N) (action ...))` stanza. See affinescript#361 (PR) for
  the worked example.

  The regex tolerates whitespace / newlines / comments between the `(test`
  opener and the `(alias` field. It will NOT match a `(rule (alias N))`
  stanza because the outer keyword is `rule`, not `test`. It MAY match a
  `(test ... (rule ... (alias)))` pseudo-nest, but that's not a valid
  dune shape so the false-positive cost is zero in practice.
  """
  def scan_dune_test_alias_inline_field(content) do
    if Regex.match?(~r/\(test\b[^()]*(?:\([^()]*\)[^()]*)*?\(alias\b/s, content) do
      [%{rule: "dune_test_alias_inline_field", severity: :high,
         description:
           "(test ...) stanza has (alias ...) inline field — dune 3.x rejects with " <>
           "`Error: Unknown field \"alias\"`. Replace with (executable ...) + " <>
           "separate (rule (alias <name>) (action ...)). See affinescript#361."}]
    else
      []
    end
  end

  @doc """
  Scan a `package.json` for an @org-scoped package listed under
  `"dependencies"` (NOT `"optionalDependencies"`, `"devDependencies"`, or
  `"peerDependencies"`) where the org matches one of the estate's
  pre-publish-hazard prefixes.

  Why this is a rule, not just a 404: estate-scoped packages
  (e.g. `@hyperpolymath/*`) are often added to `dependencies` BEFORE
  they've been published to npm. Every CI install then 404s and blocks
  the entire PR queue (affinescript was stuck for ~4 days on this
  pattern with `@hyperpolymath/affine-vscode`). Moving the dep to
  `optionalDependencies` makes `npm install` tolerate the 404 while
  letting real installs pick the package up automatically once it lands.

  Heuristic. A regex on a JSON file is imperfect — it will misfire on
  pathological multi-line `dependencies` blocks with nested objects (none
  exist in real package.json files but possible in principle). It will
  miss `dependencies` blocks where `@hyperpolymath/` appears split across
  lines in a way the lookahead can't capture. Acceptable for a WARNING-
  severity finding.

  Org prefixes are sourced from `estate_pre_publish_org_prefixes/0` so
  future estate orgs can be added in one place.
  """
  def scan_package_json_org_scoped_pre_publish(content) do
    Enum.flat_map(estate_pre_publish_org_prefixes(), fn org_prefix ->
      # Match the `"dependencies"` block (not optionalDependencies etc.) and
      # look for `"<org>/` inside it. The negative lookbehind `(?<!optional)`
      # / `(?<!dev)` / `(?<!peer)` excludes the other three dep buckets.
      regex =
        ~r/(?<![a-zA-Z])"dependencies"\s*:\s*\{[^}]*"#{Regex.escape(org_prefix)}\//s

      if Regex.match?(regex, content) do
        [%{rule: "npm_org_scoped_pre_publish_dependency", severity: :medium,
           description:
             "#{org_prefix}* package in \"dependencies\" (not \"optionalDependencies\"). " <>
             "Pre-publish hazard: if the package isn't on npm yet, `npm install` will " <>
             "404 and block every CI run. Move to optionalDependencies until published, " <>
             "or verify the package exists on the registry. See affinescript#361."}]
      else
        []
      end
    end)
  end

  @doc """
  Org prefixes whose pre-publish 404 has blocked estate CI in the past.
  Add new estate scopes here as they're introduced.
  """
  def estate_pre_publish_org_prefixes,
    do: ["@hyperpolymath"]

  @doc """
  Dispatch by filename — called from `Hypatia.Rules.scan_file/3`. Returns
  a list of findings (empty when the file is not a build-system file or
  no anti-pattern matches).
  """
  def scan(content, file_path) do
    case Path.basename(file_path) do
      "dune" -> scan_dune_test_alias_inline_field(content)
      "package.json" -> scan_package_json_org_scoped_pre_publish(content)
      _ -> []
    end
  end
end
