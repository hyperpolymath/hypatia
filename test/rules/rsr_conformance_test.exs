# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.RsrConformanceTest do
  use ExUnit.Case, async: true

  alias Hypatia.A2ml.RecordDialect
  alias Hypatia.Rules.RsrConformance
  alias Hypatia.Rules.RsrConformance.Scorecard
  alias Hypatia.Rules.RsrCriteria

  @ssot Path.expand("../fixtures/a2ml/rsr-criteria-v2.a2ml", __DIR__)

  # A minimal catalogue whose every criterion has a built-in detector, so the
  # non-provisional path (automatable_coverage == 1.0) is testable — the real
  # 74-criterion SSOT cannot reach it in this increment, by design.
  @synthetic """
  [meta]
  version = "2.0.0-test"
  status = "draft"

  [tiers]
  bronze = 50
  silver = 75
  gold = 100
  rhodium = 100

  [[category]]
  id = 1
  key = "docs"
  name = "Docs"
  weight = 10
  criteria = [
    { id = "2.1.1", name = "readme-adoc", desc = "README.adoc", tier = "bronze", gate = "universal", detect = "root_hygiene/readme", template_ref = "README.adoc" },
    { id = "2.1.3", name = "security-md", desc = "SECURITY.md", tier = "bronze", gate = "universal", detect = "root_hygiene/security", template_ref = "SECURITY.md" },
  ]

  [[category]]
  id = 2
  key = "hygiene"
  name = "Hygiene"
  weight = 20
  criteria = [
    { id = "2.1.10", name = "gitignore", desc = ".gitignore + .gitattributes", tier = "bronze", gate = "universal", detect = "manual", template_ref = "-" },
    { id = "1.2.3", name = "container-rootless", desc = "rootless container", tier = "gold", gate = "container", detect = "manual", template_ref = "container/" },
  ]
  """

  defp mk!(dir, rel, content \\ "x") do
    path = Path.join(dir, rel)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, content)
  end

  defp synthetic_catalogue do
    {:ok, cat} = RsrCriteria.load_text(@synthetic)
    cat
  end

  describe "scoring against the synthetic (fully-detectable) catalogue" do
    @describetag :tmp_dir

    test "empty repo: universal criteria applicable and failing; gated one :na", %{
      tmp_dir: tmp
    } do
      {:ok, %Scorecard{} = sc} = RsrConformance.score(synthetic_catalogue(), tmp)

      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})
      assert verdicts["2.1.1"] == :fail
      assert verdicts["2.1.3"] == :fail
      assert verdicts["2.1.10"] == :fail
      # container-gated with no rsr-profile declared -> not applicable
      assert verdicts["1.2.3"] == :na

      assert sc.profile_present == false
      assert sc.score == 0.0
      assert sc.tier == "none"
      # every applicable criterion has a detector here -> full coverage
      assert sc.automatable_coverage == 1.0
      assert sc.provisional == false
    end

    test "weights: category weight split per criterion; partial = half", %{tmp_dir: tmp} do
      mk!(tmp, "README.adoc")
      # only one of the .gitignore/.gitattributes pair -> :partial
      mk!(tmp, ".gitignore")

      {:ok, sc} = RsrConformance.score(synthetic_catalogue(), tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})
      assert verdicts["2.1.1"] == :pass
      assert verdicts["2.1.10"] == :partial

      # applicable: 2.1.1 (5.0) + 2.1.3 (5.0) + 2.1.10 (10.0) = 20.0
      assert sc.applicable_weight == 20.0
      # passed: 5.0 (pass) + 10.0 / 2 (partial) = 10.0 -> 50%
      assert sc.passed_weight == 10.0
      assert sc.score == 50.0
      # bronze threshold in the synthetic tiers is 50
      assert sc.tier == "bronze"
      assert sc.provisional == false
    end

    test "declared capabilities activate gated criteria", %{tmp_dir: tmp} do
      mk!(tmp, ".machine_readable/rsr-profile.a2ml", """
      [rsr-profile]
      capabilities = ["container", "rust"]
      """)

      {:ok, sc} = RsrConformance.score(synthetic_catalogue(), tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})

      assert sc.profile_present == true
      assert "container" in sc.capabilities
      # now applicable, but no built-in detector for 1.2.3 -> reported, not assumed
      assert verdicts["1.2.3"] == :unverified
      # an unverified applicable criterion lowers coverage -> provisional
      assert sc.automatable_coverage < 1.0
      assert sc.provisional == true
    end

    @profile_deed_head """
    ;; SPDX-License-Identifier: MPL-2.0
    (repo-deed
      :schema-version "1.0.0"
      :canonical-name "x"
      :repo-uuid #u5"github.com/o/x"
      :beholding-chora #u5"estate/chora"
    """

    test "a deed-era repo declares its capabilities in (rsr-profile …)", %{tmp_dir: tmp} do
      mk!(
        tmp,
        "x_chora.deed",
        @profile_deed_head <>
          ~s|  (rsr-profile :version "1.0.0" :capabilities ("container" rust)))\n|
      )

      {:ok, sc} = RsrConformance.score(synthetic_catalogue(), tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})

      assert sc.profile_present == true
      assert sc.capabilities == ["container", "rust"]
      assert verdicts["1.2.3"] == :unverified
    end

    test "the deed clause wins over a legacy .a2ml beside it", %{tmp_dir: tmp} do
      mk!(tmp, ".machine_readable/rsr-profile.a2ml", """
      [rsr-profile]
      capabilities = ["container"]
      """)

      mk!(
        tmp,
        "x_chora.deed",
        @profile_deed_head <> ~s|  (rsr-profile :version "1.0.0" :capabilities ("bash")))\n|
      )

      {:ok, sc} = RsrConformance.score(synthetic_catalogue(), tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})

      assert sc.capabilities == ["bash"]
      assert verdicts["1.2.3"] == :na
    end

    test "two (rsr-profile …) clauses, or one nested, declare nothing", %{tmp_dir: tmp} do
      mk!(
        tmp,
        "x_chora.deed",
        @profile_deed_head <>
          ~s|  (rsr-profile :capabilities ("container"))\n  (rsr-profile :capabilities ("rust")))\n|
      )

      {:ok, sc} = RsrConformance.score(synthetic_catalogue(), tmp)
      assert sc.profile_present == false

      File.write!(
        Path.join(tmp, "x_chora.deed"),
        @profile_deed_head <> ~s|  (notes (rsr-profile :capabilities ("container"))))\n|
      )

      {:ok, sc} = RsrConformance.score(synthetic_catalogue(), tmp)
      assert sc.profile_present == false
    end
  end

  describe "scoring against the real RSR v2.0 SSOT" do
    @describetag :tmp_dir

    test "loads, scores an empty repo, and stays provisional", %{tmp_dir: tmp} do
      {:ok, catalogue} = RsrCriteria.load(@ssot)
      {:ok, %Scorecard{} = sc} = RsrConformance.score(catalogue, tmp)

      assert sc.spec_version == "2.0.0-draft"
      # capability-gated criteria are :na without a profile
      assert Enum.any?(sc.results, &(&1.verdict == :na))
      # detector tranche is partial by design -> provisional, always
      assert sc.provisional == true
      assert sc.tier == "none"
      # not zero: prohibition criteria pass on an empty tree — emptiness
      # satisfies a ban (no Makefile, no Python/Go/V/npm). Everything requiring
      # a file present fails. Score stays well below the bronze floor (75).
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})
      assert verdicts["1.1.3"] == :pass
      assert verdicts["5.1.1"] == :pass
      assert verdicts["5.1.5"] == :pass
      assert sc.score < 75.0
    end

    test "a well-formed universal tree scores above zero and detects descriptiles", %{
      tmp_dir: tmp
    } do
      for f <-
            ~w(README.adoc SECURITY.md CODE_OF_CONDUCT.md CONTRIBUTING.md CHANGELOG.md MAINTAINERS.adoc GOVERNANCE.adoc LICENSE Justfile .editorconfig .tool-versions .pre-commit-config.yaml .gitignore .gitattributes 0-AI-MANIFEST.a2ml FUNDING.yml) do
        mk!(tmp, f)
      end

      File.mkdir_p!(Path.join(tmp, "LICENSES"))
      for f <- ~w(security.txt ai.txt humans.txt), do: mk!(tmp, ".well-known/#{f}")
      mk!(tmp, ".github/workflows/governance.yml")
      mk!(tmp, ".github/workflows/hypatia-scan.yml")

      for d <- ~w(STATE META ECOSYSTEM AGENTIC NEUROSYM PLAYBOOK ANCHOR CLADE) do
        mk!(tmp, ".machine_readable/descriptiles/#{d}.a2ml", """
        [metadata]
        version = "0.1.0"
        """)
      end

      {:ok, catalogue} = RsrCriteria.load(@ssot)
      {:ok, sc} = RsrConformance.score(catalogue, tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})

      assert verdicts["2.1.1"] == :pass
      assert verdicts["3.1.1"] == :pass
      assert verdicts["3.1.2"] == :pass
      # all descriptiles parse as record dialect
      assert verdicts["3.2.1"] == :pass
      # no rsr-profile.a2ml in this tree -> that universal criterion fails
      assert verdicts["3.2.2"] == :fail

      assert sc.score > 50.0
      assert sc.provisional == true
    end

    test "3.2.2 is satisfied by the repo deed's (rsr-profile …) clause", %{tmp_dir: tmp} do
      mk!(tmp, "x_chora.deed", """
      ;; SPDX-License-Identifier: MPL-2.0
      (repo-deed
        :schema-version "1.0.0"
        :canonical-name "x"
        :repo-uuid #u5"github.com/o/x"
        :beholding-chora #u5"estate/chora"
        (rsr-profile :version "1.0.0" :capabilities ("bash")))
      """)

      {:ok, catalogue} = RsrCriteria.load(@ssot)
      {:ok, sc} = RsrConformance.score(catalogue, tmp)
      assert Enum.find(sc.results, &(&1.id == "3.2.2")).verdict == :pass
      assert sc.profile_present == true
    end

    test "a malformed descriptile fails 3.2.1 (parse gate, not just presence)", %{
      tmp_dir: tmp
    } do
      mk!(tmp, ".machine_readable/descriptiles/STATE.a2ml", """
      [metadata]
      version = unquoted-bare-scalar
      """)

      {:ok, catalogue} = RsrCriteria.load(@ssot)
      {:ok, sc} = RsrConformance.score(catalogue, tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})
      assert verdicts["3.2.1"] == :fail
    end

    test "3.2.1 ignores a markup-dialect manifest — only record files are validated", %{
      tmp_dir: tmp
    } do
      # A well-formed record-dialect descriptile ...
      mk!(tmp, ".machine_readable/descriptiles/STATE.a2ml", """
      [metadata]
      version = "0.1.0"
      """)

      # ... alongside a MARKUP-dialect manifest (opens with @directive + prose),
      # which the record-dialect reader cannot parse. It must NOT fail 3.2.1 —
      # it is a different A2ML surface, not malformed record dialect.
      mk!(tmp, ".machine_readable/descriptiles/0-AI-MANIFEST.a2ml", """
      # 0-AI-MANIFEST.a2ml
      @manifest(version="2.0.0"):
      This manifest declares the AI-assistant context for the repo.
      @end
      """)

      {:ok, catalogue} = RsrCriteria.load(@ssot)
      {:ok, sc} = RsrConformance.score(catalogue, tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})
      assert verdicts["3.2.1"] == :pass
    end
  end

  describe "descriptiles carried by the repo deed (3.1.5-3.1.7)" do
    @describetag :tmp_dir

    @deed_head """
    ;; SPDX-License-Identifier: MPL-2.0
    (repo-deed
      :schema-version "1.0.0"
      :canonical-name "x"
      :repo-uuid #u5"github.com/o/x"
      :beholding-chora #u5"estate/chora"
    """

    defp agentic_verdicts(tmp) do
      {:ok, catalogue} = RsrCriteria.load(@ssot)
      {:ok, sc} = RsrConformance.score(catalogue, tmp)
      sc.results |> Map.new(&{&1.id, &1.verdict}) |> Map.take(~w(3.1.5 3.1.6 3.1.7))
    end

    test "top-level (agentic) (neurosym) (playbook) clauses satisfy them with no .a2ml", %{
      tmp_dir: tmp
    } do
      mk!(
        tmp,
        "x_chora.deed",
        @deed_head <>
          """
            (agentic :version "0.1.0")
            (neurosym :version "0.1.0")
            (playbook :version "0.1.0" (deployment)))
          """
      )

      assert agentic_verdicts(tmp) == %{"3.1.5" => :pass, "3.1.6" => :pass, "3.1.7" => :pass}
    end

    test "the legacy .a2ml still satisfies them without a deed", %{tmp_dir: tmp} do
      for d <- ~w(AGENTIC NEUROSYM PLAYBOOK) do
        mk!(tmp, ".machine_readable/descriptiles/#{d}.a2ml", "[metadata]\nversion = \"0.1.0\"\n")
      end

      assert agentic_verdicts(tmp) == %{"3.1.5" => :pass, "3.1.6" => :pass, "3.1.7" => :pass}
    end

    test "a deed without the clauses, or with them nested, does not", %{tmp_dir: tmp} do
      mk!(
        tmp,
        "x_chora.deed",
        @deed_head <>
          """
            (notes (agentic :version "0.1.0") (neurosym) (playbook)))
          """
      )

      assert agentic_verdicts(tmp) == %{"3.1.5" => :fail, "3.1.6" => :fail, "3.1.7" => :fail}
    end

    test "a deed that does not parse declares nothing", %{tmp_dir: tmp} do
      # unbalanced: the closing paren is inside the comment
      mk!(tmp, "x_chora.deed", @deed_head <> "  (agentic) (neurosym) (playbook) ; )\n")

      assert agentic_verdicts(tmp) == %{"3.1.5" => :fail, "3.1.6" => :fail, "3.1.7" => :fail}
    end
  end

  describe "scorecard serialization" do
    @describetag :tmp_dir

    test "round-trips through the record-dialect reader", %{tmp_dir: tmp} do
      mk!(tmp, "README.adoc")
      {:ok, sc} = RsrConformance.score(synthetic_catalogue(), tmp)

      text = RsrConformance.to_record_dialect(sc)
      assert {:ok, tree} = RecordDialect.parse(text)

      card = tree["scorecard"]
      assert card["spec"] == "rsr-criteria-v2"
      assert card["provisional"] == false
      assert card["tier"] == sc.tier
      assert is_number(card["score"])

      results = tree["result"]
      assert is_list(results)
      assert length(results) == length(sc.results)
      assert Enum.any?(results, &(&1["id"] == "2.1.1" and &1["verdict"] == "pass"))
    end
  end

  describe "delegation to live scanner rules" do
    @describetag :tmp_dir

    test "a planted .py fails 5.1.1 via the carve-out-aware scanner", %{tmp_dir: tmp} do
      mk!(tmp, "src/bad.py", "print('x')\n")
      {:ok, catalogue} = RsrCriteria.load(@ssot)
      {:ok, sc} = RsrConformance.score(catalogue, tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})
      assert verdicts["5.1.1"] == :fail
    end

    test "an unpinned action fails 4.1.3 (sha-pinning delegated to workflow_audit)", %{
      tmp_dir: tmp
    } do
      mk!(tmp, ".github/workflows/w.yml", """
      name: x
      jobs:
        a:
          steps:
            - uses: actions/checkout@v4
      """)

      {:ok, catalogue} = RsrCriteria.load(@ssot)
      {:ok, sc} = RsrConformance.score(catalogue, tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})
      assert verdicts["4.1.3"] == :fail
    end

    test "a clean tree passes the delegated language bans", %{tmp_dir: tmp} do
      mk!(tmp, "README.adoc")
      {:ok, catalogue} = RsrCriteria.load(@ssot)
      {:ok, sc} = RsrConformance.score(catalogue, tmp)
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})
      # no banned files -> the scanner emits nothing -> bans pass
      assert verdicts["5.1.1"] == :pass
      assert verdicts["5.1.2"] == :pass
      assert verdicts["5.1.5"] == :pass
    end
  end

  describe "self-application smoke" do
    test "scoring the hypatia repo itself returns a scorecard without raising" do
      {:ok, catalogue} = RsrCriteria.load(@ssot)
      repo_root = Path.expand("../..", __DIR__)

      assert {:ok, %Scorecard{} = sc} = RsrConformance.score(catalogue, repo_root)
      # hypatia predates the descriptiles migration (still 6a2/) — the oracle
      # must report that honestly rather than crash or invent a pass.
      verdicts = Map.new(sc.results, &{&1.id, &1.verdict})
      assert verdicts["3.1.1"] in [:pass, :fail]
      assert sc.provisional == true
    end
  end
end
