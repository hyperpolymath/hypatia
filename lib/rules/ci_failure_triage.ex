# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.CiFailureTriage do
  @moduledoc """
  Symbolic triage of a failed CI run: which layer failed, what to do, and
  when to stop.

  Hypatia's other rules read a repository. These rules read what a run left
  behind (a job log, its exit code and conclusion, the gate's check states)
  and answer one question: is this red the code's fault? Most estate reds in
  November 2026 were not: a rate-limited setup step, a merge ref that vanished
  when the PR merged, a cancelled run, an unreachable git host, a lockfile pin
  the runner refuses. Treating those as code failures sends a bot to "fix"
  code that is fine, and loops.

  ## Rule ids

  Every id is `hypatia/ci_failure_triage/<name>` (standards A5: `<domain>/<name>`,
  no wildcard ids). `rules/0` lists them with their layer, action and budget.

  ## Precedence (first layer wins)

  A log often shows several symptoms; the earliest layer is the cause:

      1. :platform   the runner or a service failed (rate limit, merge-ref race,
                     cancellation, unreachable host)
      2. :config     the run was set up wrongly (lockfile pin, secret or
                     variable, App key format, canon lockstep)
      3. :toolchain  the tool crashed (SIGSEGV)
      4. :code       the code or its tests failed (tests, bench regression)

  Within a layer, the rule listed first in `@rules` wins. `classify/2`
  returns every match, and `primary` is the first match by
  `{layer rank, rule order}`.

  ## Actions and loop limit

  Each rule names one action:

    * `:none`    not a failure of this change (cancelled; a stale merge ref
                 for a PR that already merged). Nothing to do.
    * `:rerun`   transient. Rerun, at most `retries` times for the same
                 `{repo, workflow, sha, rule}`.
    * `:fix`     a known recipe fixes it (`fix` names it). Never a rerun.
    * `:owner`   needs a person: a judgement, a credential, or a setting this
                 system may not change (required checks, rulesets).
    * `:report`  a code failure, for the code's owner or the fixer bots.
    * `:upstream` a rollup job that only reports other jobs' failures. Triage
                 those jobs instead.

  `decide/2` applies the loop limit. A transient rule whose budget is spent
  becomes `:owner` with `persistent: true`, so a rerun loop always ends: the
  attempt count only rises, and every budget is at most 2.

  ## What a fix may never do

  A decision from these rules never bypasses a gate. It never drops or
  renames a required check, edits a ruleset, uses an admin merge, touches
  licence or SPDX text, or loosens a canon/spine gate to make a pin pass.
  `forbidden_actions/0` lists these so a dispatcher can check before it acts.
  """

  @domain "hypatia/ci_failure_triage"

  @layers [:platform, :config, :toolchain, :code]

  # Each rule: name, layer, patterns (any matches), action, retries, fix,
  # escalation, reason. Order inside a layer is precedence.
  @rules [
    %{
      name: "runner_cancelled",
      layer: :platform,
      patterns: [
        # Runner-authored lines only. A bare "The operation was canceled." is
        # also ordinary exception text (.NET TaskCanceledException) and must
        # not hide a real failure; `conclusion: cancelled` is authoritative.
        ~r/##\[error\]The operation was canceled/,
        ~r/##\[error\]The run was canceled by/,
        # A verdict job reporting that the job it reads was cancelled by a
        # newer push (rsr-template-repo#337: `Fork governance failed
        # (deed=cancelled)`). Superseded, not a defect.
        ~r/\bgovernance failed \(deed=cancelled\)/i
      ],
      action: :none,
      retries: 0,
      fix: nil,
      escalation: :none,
      reason:
        "The run was cancelled (a newer push, concurrency, or a person). A cancelled " <>
          "job is not a failure of the change. If a later job turned red because " <>
          "an upstream job was cancelled, that job runs with `if: always()`: see WF029."
    },
    %{
      # CASE-043: estate gates that read GitHub state (rulesets, the workflow
      # catalogue, a callee workflow at a pinned SHA) and refuse to pass when
      # the read fails. Three signatures in one 3-minute window on
      # metadatastician/standards#1514/#1518 and its main: one API event.
      name: "github_state_unreadable",
      layer: :platform,
      patterns: [
        ~r/could not read branch rulesets/,
        ~r/could not read the workflow catalogue/,
        ~r/could not be read at [0-9a-f]{7,40}\S* \(offline with no cache entry, or the API refused\)/
      ],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "A gate could not read GitHub state (rulesets, the workflow catalogue or a " <>
          "pinned callee) and correctly refused to pass. Not the change. Rerun once; " <>
          "if it fails again, the token the gate uses lacks read access or is rate " <>
          "limited, and only a person can fix that."
    },
    %{
      name: "api_rate_limit",
      layer: :platform,
      patterns: [
        ~r/API rate limit exceeded/i,
        ~r/exceeded a secondary rate limit/i
      ],
      # GitHub's own throttle only: an app's API test that logs the same words
      # is the code under test, not the platform.
      context: [~r/api\.github\.com|\bgh api\b|x-github-request-id|installation ID \d+/i],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "GitHub API rate limit (often in a setup step such as setup-bun fetching a " <>
          "release list). Not a code failure. Rerun after the limit resets; if it " <>
          "persists, the token or App installation is shared by too many jobs."
    },
    %{
      name: "merge_ref_race",
      layer: :platform,
      patterns: [~r/couldn't find remote ref refs\/pull\/\d+\/merge/],
      # Agreed with cicd-squabbler (SQ-NOI-02): a merged or closed PR is a
      # stale run (noise); an OPEN PR with no merge ref has a conflict or a
      # deleted ref, which is referred, never rerun.
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "The PR's merge ref was gone when checkout ran. For a merged or closed PR " <>
          "the run is stale: nothing to do (SQ-NOI-02). For an open PR the merge " <>
          "ref is missing because of a conflict or a deleted ref: refer, do not rerun."
    },
    %{
      name: "external_host_unreachable",
      layer: :platform,
      patterns: [
        ~r/fatal: unable to access '[^']+': (Could not resolve host|Failed to connect|Connection timed out|Operation timed out|Recv failure|gnutls_handshake|The requested URL returned error: 5\d\d)/,
        # A dotted (external) name only: `backend` or `db` is a service under test.
        ~r/Could not resolve host:\s*([A-Za-z0-9-]+\.[A-Za-z0-9.-]+)/,
        # Not the app under test: a loopback probe that fails is a code failure.
        ~r/Failed to connect to (?!localhost\b|127\.|0\.0\.0\.0\b|\[?::1\]?\b)(\S+) port \d+/
      ],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "A git or download host outside the repo did not answer (for example " <>
          "git.sr.ht in an Idris2 pack build, exit 128). Rerun later; if a host is " <>
          "often down, the owner decides whether to mirror or vendor it."
    },
    %{
      name: "api_server_error",
      layer: :platform,
      patterns: [
        ~r/\bgh: HTTP 50[234]\b/,
        # A bare `HTTP 502: ...` is any client's; only the `gh` CLI and the
        # api.github.com host tie it to the GitHub API.
        ~r/\bgh(?: api)?\b[^\n]*\bHTTP 50[234]: /,
        ~r/api\.github\.com[^\n]*\b50[234]\b/,
        ~r/\bgh\b[^\n]*Server Error \(HTTP 50[234]\)/
      ],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "The GitHub API answered 502/503/504. Transient, or a query too heavy for the " <>
          "API (a timeout shows as 502). Rerun; if it persists, split the query."
    },
    %{
      name: "scan_range_missing_commit",
      layer: :platform,
      patterns: [
        # Scanner prefix required: a test running git on a bad range emits the
        # same message.
        ~r/(?:\[git\]|gitleaks|trufflehog)[^\n]*fatal: ambiguous argument '[0-9a-f]{7,40}\^?\.\.[0-9a-f]{7,40}': unknown revision/i,
        # `bad object` alone is any git caller's (a test with a bad SHA); only
        # a range scanner's prefix ties it to this cause.
        ~r/(?:\[git\]|gitleaks|trufflehog)[^\n]*fatal: bad object [0-9a-f]{40}/i
      ],
      action: :rerun,
      retries: 1,
      fix: nil,
      escalation: :owner,
      reason:
        "A diff-range scan (gitleaks, trufflehog) named a commit that is not in the " <>
          "checkout: the base moved (rebase, force-push, base update) or the clone is too " <>
          "shallow. Rerun once; if it repeats, fetch the base commit before scanning."
    },
    %{
      name: "token_invalid",
      layer: :config,
      patterns: [
        # The GitHub client forms only; an app's own login test may log the
        # bare phrase.
        ~r/\bgh(?: api)?\b[^\n]*Bad credentials/,
        # gh's own error form names the API host; a bare "HTTP 401: Bad
        # credentials" from an app's HTTP test is not a GitHub token.
        ~r/HTTP 401: Bad credentials \(https:\/\/api\.github\.com/,
        # git over HTTPS: the server answered and refused the credential.
        ~r/fatal: unable to access '[^']+': The requested URL returned error: 40[13]/
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "The token the job uses was refused (HTTP 401): expired, revoked or wrong. " <>
          "Rotate it. Only a person can do this; every job on that token fails until then."
    },
    %{
      name: "actions_lock_invalid_pin",
      layer: :config,
      patterns: [~r/invalid lockfile pin/i],
      action: :fix,
      retries: 0,
      fix: "regenerate-actions-lock-leaf",
      escalation: :owner,
      reason:
        "The Actions runner refused an actions.lock leaf at `Set up job`. The known " <>
          "cause is a leaf whose `ref` repeats its own SHA (ActionsLock " <>
          "`self_sha_leaf_ref`). Regenerate the lock with the current gh-actions-lock; " <>
          "if the bad leaf comes from a standards commit, fix it there (solutions at source)."
    },
    %{
      name: "actions_lock_missing_ref",
      layer: :config,
      patterns: [
        ~r/refs missing from the lockfile/i,
        # The runner's wording for the same gap (Protoctist.jl pages build).
        ~r/lockfile missing pin for /
      ],
      action: :fix,
      retries: 0,
      fix: "regenerate-actions-lock",
      escalation: :owner,
      reason:
        "A workflow uses an action ref that actions.lock does not record (the " <>
          "lock-sync gate). Regenerate the lock with gh-actions-lock against the real " <>
          "ref; never hand-write a leaf."
    },
    %{
      name: "repository_map_stale",
      layer: :config,
      patterns: [~r/committed REPOSITORY-MAP\.adoc is stale/],
      action: :fix,
      retries: 0,
      # cicd-squabbler SQ-GEN-01: the generator is named in the rule, never
      # read from the log.
      fix: "bash scripts/gen-repo-map.sh . && commit (SQ-GEN-01)",
      escalation: :owner,
      reason:
        "REPOSITORY-MAP.adoc no longer matches the tree (dictask). Regenerate it " <>
          "with the repo's generator and commit the result."
    },
    %{
      name: "generated_file_stale",
      layer: :config,
      patterns: [~r/committed (?!REPOSITORY-MAP\.adoc)\S+ is stale\s*[—-]+\s*run: \S/],
      # Referral: a bot must not run a command it read from a log. Each new
      # generated file becomes its own first-line rule, generator named.
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A committed generated file no longer matches its source. Its generator " <>
          "is not yet a named first-line rule; refer, and add a rule for it."
    },
    %{
      name: "workflow_startup_failure",
      layer: :config,
      patterns: [
        ~r/conclusion: startup_failure/,
        ~r/This run likely failed because of a workflow file issue/
      ],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "The workflow failed before any job started, so there is no log. Seen " <>
          "causes: an actions.lock record with a stale owner_id after a repo " <>
          "transfer, a stale cross-repo workflow_call caller, an Actions policy " <>
          "that does not allow the called reusable. Read the run page's banner; " <>
          "never rerun blind."
    },
    %{
      name: "dependabot_ecosystem_mismatch",
      layer: :config,
      patterns: [
        ~r/has a bun\.lock file but no package-lock\.json/,
        # cicd-squabbler SQ-TOOL-02 signature (run 43225380901).
        ~r/Dependabot detected that Bun is misconfigured/
      ],
      action: :fix,
      retries: 0,
      fix: "dependabot.yml: package-ecosystem npm -> bun for that directory",
      escalation: :owner,
      reason:
        "Dependabot is configured for npm on a directory that uses bun " <>
          "(MetaManifold-WebUI#330)."
    },
    %{
      name: "codeql_no_source",
      layer: :config,
      patterns: [~r/CodeQL did not detect any code written in languages supported/],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "Code scanning default setup lists a language the repo has no code in. " <>
          "Removing it is a security setting: the owner changes it."
    },
    %{
      name: "codeql_setup_conflict",
      layer: :config,
      patterns: [~r/CodeQL job status was configuration error/],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "CodeQL default setup and an advanced codeql.yml both run. Only the owner " <>
          "can turn default setup off."
    },
    %{
      name: "actions_lock_unresolvable_pin",
      layer: :config,
      patterns: [
        ~r/lockfile verification did not produce a result for this action/,
        ~r/Unable to resolve action `[^`]+`, unable to find version/
      ],
      action: :fix,
      retries: 0,
      fix: "regenerate-actions-lock",
      escalation: :owner,
      reason:
        "actions.lock pins an action to a commit GitHub cannot resolve (a stale or " <>
          "deleted pin; DEBT-REGISTER CI-1). Regenerate the lock against live refs."
    },
    %{
      name: "app_key_wrong_format",
      layer: :config,
      patterns: [
        ~r/BEGIN OPENSSH PRIVATE KEY/,
        ~r/DECODER routines::unsupported/,
        ~r/error:1E08010C/,
        ~r/Invalid keyData/
      ],
      # The decoder errors are any key parser's; only a GitHub App token step
      # elsewhere in the same log ties them to the App's private key.
      context: [~r/create-github-app-token/],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "The GitHub App private key is not a PEM key the App token action can read " <>
          "(an OpenSSH key was pasted). Generate a private key from the App's settings " <>
          "page and store that .pem file's contents in the secret. Only a person can do this."
    },
    %{
      name: "secret_or_variable_missing",
      layer: :config,
      patterns: [
        ~r/appId option is required/,
        ~r/Input required and not supplied:\s*(app[-_]id|private[-_]key|client[-_]id)/i,
        ~r/\[@octokit\/auth-app\].*required/
      ],
      context: [~r/create-github-app-token|@octokit\/auth-app|app[-_]token/i],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A required App id or key reached the step empty. Common cause: a value read " <>
          "as `vars.X` was stored as a secret (or the reverse), so the expression is " <>
          "empty. Store it where the workflow reads it. Only a person can do this."
    },
    %{
      name: "ssh_key_secret_empty",
      layer: :config,
      patterns: [~r/The ssh-private-key argument is empty/],
      context: [~r/ssh-agent|webfactory|SSH agent/i],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "An SSH key secret (webfactory/ssh-agent `ssh-private-key`) reached the step " <>
          "empty. Seen on forge mirrors whose `*_MIRROR_ENABLED` variable is on while " <>
          "the forge's key secret was never set, so every push to main fails. Add the " <>
          "secret or turn the variable off. Only a person can do this; never rerun."
    },
    %{
      name: "security_findings_reported",
      layer: :code,
      patterns: [~r/Hypatia reported \d+ (critical|high) finding/i],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A security scan reported findings at gate severity. The owner decides; " <>
          "never autofix, close or baseline a security finding to turn the check green."
    },
    %{
      name: "dependency_advisory",
      layer: :code,
      patterns: [~r/\bRUSTSEC-\d{4}-\d{4}\b/, ~r/error: \d+ vulnerabilit(y|ies) found/],
      # An advisory id in a test or fixture is not an audit failure.
      context: [~r/\d+ vulnerabilit(y|ies) found|cargo[- ]audit|cargo[- ]deny|Crate:\s+\S+/i],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A dependency has a published advisory (cargo audit). A person bumps or " <>
          "replaces it; never ignore the advisory to pass."
    },
    %{
      name: "review_rate_limited",
      layer: :platform,
      patterns: [~r/Review limit reached/],
      # An app's own review feature may log the phrase; CodeRabbit's comment
      # carries its own marker.
      context: [~r/coderabbit/i],
      action: :none,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "CodeRabbit is out of reviews for now. Wait; the minutes count changes " <>
          "on every edit, so key on the marker."
    },
    %{
      name: "review_quota_exhausted",
      layer: :platform,
      patterns: [
        ~r/exhausted its Codex allowance/,
        ~r/reached your Codex usage limits/
      ],
      # Owner ruling R7 covers bot REVIEWS only; a required generation or agent
      # step out of quota stays actionable.
      context: [~r/\breview\b/i],
      # Owner ruling R7: bot reviews are advisory, never a gate.
      action: :none,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "The Codex paying account has no credits, so no Codex review runs. Bot " <>
          "reviews are advisory (owner ruling R7): nothing waits on it."
    },
    %{
      name: "ruleset_blocks_push",
      layer: :config,
      patterns: [~r/Repository rule violations found/],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A repository ruleset refused a push from the workflow (Apodeixis.jl badge " <>
          "branch: 'Code coverage checks require a pull request', HTTP 409). A rerun " <>
          "cannot pass; rulesets are the owner's. No bot edits or bypasses them."
    },
    %{
      name: "uuid_conformance",
      layer: :config,
      patterns: [~r/non-v8 UUID literal/],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "An identifier is not ESTATE-UUID-V8 (Apodeixis.jl Project.toml). Existing " <>
          "ids are not re-minted (A7); a Julia package UUID needs the owner's call."
    },
    %{
      name: "kyaml_format",
      layer: :config,
      patterns: [~r/kyaml-format: \S+ is not KYAML/],
      action: :fix,
      retries: 0,
      # Named generator, never a command read from the log.
      fix: "scripts/kyaml-format.sh <file> with Go yq (mikefarah), then commit",
      escalation: :owner,
      reason:
        "A YAML file is not in KYAML form (quintessence compose.yaml). Reformat " <>
          "with the repo's kyaml-format.sh using Go yq; Python yq corrupts it."
    },
    %{
      name: "sonar_not_authorized",
      layer: :config,
      patterns: [~r/Not authorized or organization not found/],
      # An app may log the phrase; only a Sonar scanner run ties it to Sonar.
      context: [~r/sonar-scanner|SonarQube|SonarCloud/i],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "The Sonar scanner's token or project binding is not valid for this " <>
          "organization (lithoglyph). The owner fixes the setting."
    },
    %{
      name: "pages_not_enabled",
      layer: :config,
      patterns: [
        ~r/Get Pages site failed/,
        ~r/verify that the repository has Pages enabled/
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "GitHub Pages is not enabled for the repository (configure-pages cannot find " <>
          "a site). Enabling it is a repository setting: escalate, never fix in code."
    },
    %{
      name: "canon_lockstep",
      layer: :config,
      patterns: [
        ~r/the canon and the spine are not in lockstep/i,
        ~r/canon[^\n]{0,40}lockstep[^\n]{0,40}(FAIL|mismatch|broken)/i,
        # dictask: the pin needs rsr-profile.a2ml, which the estate forbids.
        # A gate/rule contradiction: owner ruling, never a generated file.
        ~r/cannot declare a canon pin/
      ],
      # Split by cause with cicd-squabbler: an old INLINE copy of the
      # lockstep job is SQ-CANON-01 (first-line, adopt-canon-lockstep.sh); a
      # pin that must change is the owner's, since A2ML and canon pins are
      # never edited automatically.
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "The repo is out of lockstep with the canon standards publishes. If the " <>
          "job is an old inline copy, that is SQ-CANON-01 (first-line). If the pin " <>
          "itself must change, the owner bumps it. Owner ruling R10: the gate " <>
          "accepts the deed form, so a missing rsr-profile.a2ml is fixed in the " <>
          "gate, never by adding the file. Never loosen the spine gate to pass."
    },
    %{
      name: "toolchain_segfault",
      layer: :toolchain,
      # Only when the crashing command is a known runtime or compiler. A
      # segfault in a program the change built is a code failure
      # (`process_segfault`).
      patterns: [
        ~r/Segmentation fault[^\n]*\b(mix|elixir|erl|beam\.smp|rustc|cargo|ghc|idris2|zig|pack)\b/,
        # hypatia-cli.sh runs a pure-Elixir escript: a segfault there is the
        # BEAM (OTP-19951 on OTP 28, hypatia#1062), not Hypatia's code.
        ~r/Segmentation fault[^\n]*hypatia-cli\.sh/,
        ~r/\b(beam\.smp|rustc|idris2|ghc)\b[^\n]*\b(SIGSEGV|signal 11)\b/
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A tool crashed with SIGSEGV (exit 139), not a test verdict. Do not rerun: " <>
          "the estate case was deterministic (OTP 28.5.0.8 miscompiling PinSites in " <>
          "`mix hypatia.rsr_score`, hypatia#1062). Record the runtime version and " <>
          "escalate. Owner ruling R2: the interim estate pin is OTP 27.3.4.19 until " <>
          "hypatia#1062 merges; never edit code to dodge it."
    },
    %{
      name: "bench_regression",
      layer: :code,
      patterns: [
        ~r/regress\w*[^\n]{0,80}\b\d+(\.\d+)?x\b/i,
        ~r/\b\d+(\.\d+)?x\b[^\n]{0,80}(baseline|threshold|limit)/i
      ],
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      reason:
        "A benchmark exceeded its baseline. Whether to fix the code or accept a new " <>
          "baseline is a judgement for the owner; a bot may not rebaseline."
    },
    %{
      name: "format_diff",
      layer: :code,
      patterns: [~r/(^|\s)Diff in \S+:\d+:/],
      action: :fix,
      retries: 0,
      fix: "run-formatter",
      escalation: :code_owner,
      reason: "A formatter check found unformatted code. Run the formatter and commit."
    },
    %{
      name: "compile_error",
      layer: :code,
      patterns: [
        ~r/error\[E\d{4}\]/,
        ~r/error: could not compile/,
        ~r/== Compilation error in file/
      ],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :code_owner,
      reason: "The code did not compile. Report to the change's author."
    },
    %{
      name: "runtime_exception",
      layer: :code,
      patterns: [~r/\*\* \((?:\w+\.)*\w+(Error|Exception)\)/, ~r/thread '[^']+' panicked at/],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :code_owner,
      reason: "A program raised or panicked. Report to its owner with the evidence line."
    },
    %{
      name: "process_segfault",
      layer: :code,
      patterns: [~r/Segmentation fault/, ~r/exit code 139\b/, ~r/\bSIGSEGV\b/],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :code_owner,
      reason:
        "A process crashed with SIGSEGV (exit 139) and the log does not name a known " <>
          "runtime or compiler, so treat it as the program's own crash and report it."
    },
    %{
      # Never matched by a pattern: added only when a nonzero exit code has no
      # known signature, so a failed job is never classified as clean.
      name: "unrecognized_failure",
      layer: :code,
      patterns: [],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :code_owner,
      reason:
        "The job exited nonzero and no known signature matched. Report it with the " <>
          "log tail; if it recurs, it is a candidate for a new signature."
    },
    %{
      name: "test_failure",
      layer: :code,
      patterns: [
        ~r/\b\d+ tests?, [1-9]\d* failures?/,
        # A doctest-only ExUnit suite summarises as "N doctests, M failures".
        ~r/\b\d+ doctests?, (?:\d+ tests?, )?[1-9]\d* failures?/,
        ~r/test result: FAILED/,
        ~r/(^|\s)FAILED\s+\S+::/
      ],
      action: :report,
      retries: 0,
      fix: nil,
      escalation: :code_owner,
      reason: "Tests failed. This is a code failure; report it to the change's author."
    },
    %{
      name: "upstream_artifact_missing",
      layer: :code,
      patterns: [
        ~r/Artifact not found for name/,
        ~r/Unable to download artifact\(s\)/,
        ~r/No artifacts found|Unable to find any artifacts/
      ],
      action: :upstream,
      retries: 0,
      fix: nil,
      escalation: :none,
      reason:
        "An artifact a job expects was never uploaded: the job that makes it failed, " <>
          "crashed (for example a scanner SIGSEGV) or was cancelled. Triage that job."
    },
    %{
      name: "rollup_cascade",
      layer: :code,
      patterns: [~r/\d+ of \d+ [\w -]+ (did not report success|did not complete)/],
      action: :upstream,
      retries: 0,
      fix: nil,
      escalation: :none,
      reason:
        "A status rollup is red because jobs it waits on are red or missing. Triage " <>
          "those jobs, not this one; fixing the rollup would hide them."
    }
  ]

  @forbidden [
    :admin_merge,
    :drop_required_check,
    :rename_required_check,
    :edit_ruleset,
    :edit_licence_or_spdx,
    :loosen_canon_gate,
    :skip_or_quarantine_test,
    # A security finding (Hypatia critical, a Sonar security rating) stays
    # open until a person resolves it; no rule closes, baselines or hides it.
    :close_or_baseline_security_finding
  ]

  @doc "The rule id for a rule name."
  def id(name), do: "#{@domain}/#{name}"

  # Rules that are not log patterns: they take check states, workflow sets or
  # workflow files. Listed so `rules/0` is the whole inventory.
  @supplemental [
    %{
      name: "required_check_never_reported",
      layer: :config,
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      input: :check_states,
      reason: "A required check never reported (check_gate/2)."
    },
    %{
      name: "scanner_pin_drift",
      layer: :config,
      action: :owner,
      retries: 0,
      fix: nil,
      escalation: :owner,
      input: :workflow_set,
      reason: "panic-attack pinned at different commits (check_scanner_pin_drift/1)."
    },
    %{
      name: "always_after_cancel",
      layer: :config,
      action: :fix,
      retries: 0,
      fix: "always-to-not-cancelled",
      escalation: :owner,
      input: :workflow,
      reason: "if: always() job that downloads its needs' artifacts (WorkflowAudit WF029)."
    }
  ]

  @doc """
  Every rule, in precedence order, without its patterns. Each carries its
  `route` under cicd-squabbler ADR 0003: `first-line` (the squabbler acts),
  `referral` (Hypatia answers), `owner-only` or `noise`.
  """
  def rules do
    (Enum.map(@rules, &Map.put(&1, :input, :log)) ++ @supplemental)
    |> Enum.sort_by(&rank/1)
    |> Enum.map(fn r ->
      r
      |> Map.drop([:patterns, :context])
      |> Map.put(:id, id(r.name))
      |> Map.put(:route, route(r.action))
    end)
  end

  @doc """
  The ADR 0003 route for an action. Hypatia never reruns or fixes on its own:
  a `rerun` or `fix` decision is advice to the first-line fixer.
  """
  def route(:rerun), do: "first-line"
  def route(:fix), do: "first-line"
  def route(:owner), do: "owner-only"
  def route(:none), do: "noise"
  def route(_), do: "referral"

  @doc "Actions a decision from these rules may never take."
  def forbidden_actions, do: @forbidden

  @doc """
  Classifies a failed job from its log text and context.

  Context keys (all optional): `:exit_code`, `:conclusion` (GitHub's job
  conclusion, e.g. "cancelled"), `:pr_state` (`:open`, `:merged`, `:closed`).

  Returns `%{primary: finding | nil, matches: [finding]}`. A finding carries
  `rule`, `layer`, `action`, `retries`, `fix`, `escalation`, `reason`,
  `evidence` (the first matching line) and `line`.
  """
  def classify(log, ctx \\ %{}) when is_binary(log) do
    lines = log |> String.split("\n") |> Enum.with_index(1)

    matches =
      @rules
      |> Enum.flat_map(fn rule ->
        case first_hit(rule, lines, log) do
          nil -> []
          {text, n} -> [finding(rule, text, n)]
        end
      end)
      |> add_conclusion(ctx)
      |> refine(ctx)
      |> Enum.sort_by(&{rank(&1), order(&1)})

    %{primary: List.first(matches), matches: matches}
  end

  @doc """
  Applies the loop limit. `attempts` is how many reruns this
  `{repo, workflow, sha, rule}` has already had. Returns the finding with its
  final `action`, and `persistent: true` once a transient budget is spent.
  """
  def decide(nil, _attempts), do: nil

  # A negative count would read as budget left; refuse rather than allow
  # extra reruns.
  def decide(_finding, attempts) when is_integer(attempts) and attempts < 0,
    do: raise(ArgumentError, "attempts must be 0 or more, got #{attempts}")

  def decide(%{action: :rerun, retries: budget} = finding, attempts)
      when is_integer(attempts) and attempts >= budget do
    %{finding | action: finding.escalation}
    |> Map.put(:route, route(finding.escalation))
    |> Map.put(:persistent, true)
  end

  def decide(finding, _attempts), do: Map.put(finding, :persistent, false)

  @doc """
  Required checks that never reported ("Expected — Waiting for status to be
  reported"). `required` and `reported` are check context names. A reported
  name that differs only in case or surrounding space is named as the likely
  intended check (name drift); otherwise the workflow never ran for this
  event (a path filter, an event filter, or a deleted workflow).

  Both outcomes go to the owner: required checks are settings this system
  never changes.
  """
  def check_gate(required, reported) when is_list(required) and is_list(reported) do
    norm = fn s -> s |> String.trim() |> String.downcase() end
    reported_set = MapSet.new(reported)
    by_norm = Map.new(reported, &{norm.(&1), &1})

    for name <- required, not MapSet.member?(reported_set, name) do
      near = Map.get(by_norm, norm.(name))

      %{
        rule: id("required_check_never_reported"),
        layer: :config,
        action: :owner,
        route: route(:owner),
        retries: 0,
        fix: nil,
        escalation: :owner,
        check: name,
        near_match: near,
        reason:
          if near do
            "Required check #{inspect(name)} never reported, but #{inspect(near)} did. " <>
              "The required name and the job name have drifted apart."
          else
            "Required check #{inspect(name)} never reported. Its workflow did not run " <>
              "for this event (path or event filter, or the workflow is gone)."
          end
      }
    end
  end

  @scanner_pin ~r/^\s*["']?(PANIC_ATTACK_SHA|PA_SHA|panic[-_]attack[-_]sha)["']?\s*:\s*["']?([0-9a-f]{40})/im
  # A reusable workflow's input (`panic-ref`, `panic-rev`, `panic-attack-sha`)
  # whose `default:` is the pin.
  # Every line between the key and `default:` must be indented deeper than
  # the key, so the match cannot cross into a sibling input's block.
  @scanner_input ~r/^([ \t]*)["']?(panic(?:[-_]attack)?[-_](?:sha|ref|rev|commit))["']?[ \t]*:[ \t]*\n(?:\1[ \t]+(?!["']?default["']?[ \t]*:)[^\n]*\n){0,6}?\1[ \t]+["']?default["']?[ \t]*:[ \t]*["']?([0-9a-f]{40})/im

  # Drop YAML comments (a `#` at line start or after whitespace, outside
  # quotes) so a kept "old pin" comment is not read as a second pin, while
  # `{NOTE: "use # carefully", PA_SHA: <sha>}` keeps its pin.
  defp strip_yaml_comments(content) do
    content
    |> String.split("\n")
    |> Enum.map_join("\n", &strip_line_comment(String.to_charlist(&1), nil, []))
  end

  defp strip_line_comment([], _q, acc), do: acc |> Enum.reverse() |> List.to_string()

  defp strip_line_comment([?# | _], nil, acc) when acc == [] or hd(acc) in [?\s, ?\t],
    do: acc |> Enum.reverse() |> List.to_string()

  defp strip_line_comment([c | rest], nil, acc) when c in [?", ?'],
    do: strip_line_comment(rest, c, [c | acc])

  defp strip_line_comment([?\\, c | rest], ?", acc),
    do: strip_line_comment(rest, ?", [c, ?\\ | acc])

  defp strip_line_comment([q | rest], q, acc), do: strip_line_comment(rest, nil, [q | acc])
  defp strip_line_comment([c | rest], q, acc), do: strip_line_comment(rest, q, [c | acc])

  @doc """
  Scanner pin drift: panic-attack pinned at different commits across a set of
  workflow files (`%{label => content}`, labels may name several repos).
  Returns one finding per pin site when more than one commit is pinned, and
  `[]` otherwise. Which commit is canonical is the owner's call.
  """
  # YAML flow forms: `env: {PA_SHA: <sha>}` and `panic-ref: {default: <sha>}`.
  @scanner_pin_flow ~r/[{,]\s*["']?(PANIC_ATTACK_SHA|PA_SHA|panic[-_]attack[-_]sha)["']?\s*:\s*["']?([0-9a-f]{40})/i
  @scanner_input_flow ~r/["']?(panic(?:[-_]attack)?[-_](?:sha|ref|rev|commit))["']?\s*:\s*\{[^}]*\bdefault["']?\s*:\s*["']?([0-9a-f]{40})/i

  def check_scanner_pin_drift(contents) when is_map(contents) do
    sites =
      for {label, content} <- contents,
          content = content |> String.replace("\r\n", "\n") |> strip_yaml_comments(),
          re <- [@scanner_pin, @scanner_input, @scanner_pin_flow, @scanner_input_flow],
          match <- Regex.scan(re, content),
          [key, sha] = Enum.take(match, -2) do
        %{file: label, key: key, sha: String.downcase(sha)}
      end
      |> Enum.uniq()

    shas = sites |> Enum.map(& &1.sha) |> Enum.uniq() |> Enum.sort()

    if length(shas) > 1 do
      for site <- Enum.sort_by(sites, &{&1.file, &1.key}) do
        %{
          rule: id("scanner_pin_drift"),
          layer: :config,
          action: :owner,
          route: route(:owner),
          retries: 0,
          fix: nil,
          escalation: :owner,
          file: site.file,
          key: site.key,
          sha: site.sha,
          pinned: shas,
          reason:
            "panic-attack is pinned at #{length(shas)} different commits " <>
              "(#{Enum.map_join(shas, ", ", &String.slice(&1, 0, 8))}); findings " <>
              "differ by lane. Pick one commit and pin it everywhere."
        }
      end
    else
      []
    end
  end

  # ── internals ─────────────────────────────────────────────────────────

  defp first_hit(rule, lines, log) do
    if in_context?(rule, log), do: scan_lines(rule, lines)
  end

  # A rule with `:context` fires only when one of those patterns appears
  # somewhere in the whole log (the signature line alone is ambiguous).
  defp in_context?(%{context: ctx}, log), do: Enum.any?(ctx, &Regex.match?(&1, log))
  defp in_context?(_rule, _log), do: true

  defp scan_lines(rule, lines) do
    Enum.find_value(lines, fn {text, n} ->
      if Enum.any?(rule.patterns, &Regex.match?(&1, text)), do: {String.trim(text), n}
    end)
  end

  defp finding(rule, text, n) do
    %{
      rule: id(rule.name),
      layer: rule.layer,
      action: rule.action,
      retries: rule.retries,
      fix: rule.fix,
      escalation: rule.escalation,
      reason: rule.reason,
      route: route(rule.action),
      evidence: text,
      line: n
    }
  end

  # A conclusion of "cancelled" is authoritative even when the log says nothing.
  defp add_conclusion(matches, %{conclusion: c}) when c in ["cancelled", :cancelled] do
    cancelled = id("runner_cancelled")

    if Enum.any?(matches, &(&1.rule == cancelled)) do
      matches
    else
      rule = Enum.find(@rules, &(&1.name == "runner_cancelled"))
      [finding(rule, "conclusion: cancelled", 0) | matches]
    end
  end

  defp add_conclusion(matches, %{conclusion: c})
       when c in ["startup_failure", :startup_failure] do
    startup = id("workflow_startup_failure")

    if Enum.any?(matches, &(&1.rule == startup)) do
      matches
    else
      rule = Enum.find(@rules, &(&1.name == "workflow_startup_failure"))
      [finding(rule, "conclusion: startup_failure", 0) | matches]
    end
  end

  defp add_conclusion(matches, _ctx), do: matches

  # Context that changes a rule's action.
  defp refine(matches, ctx) do
    race = id("merge_ref_race")

    # A merge ref that vanished because the PR merged or closed is a stale
    # run, not a failure.
    Enum.map(matches, fn
      %{rule: ^race} = f when is_map_key(ctx, :pr_state) ->
        if ctx.pr_state in [:merged, :closed],
          do: %{f | action: :none, retries: 0, route: route(:none)},
          else: f

      f ->
        f
    end)
    |> add_exit_code(ctx)
  end

  defp add_exit_code(matches, %{exit_code: 139}) do
    segv = [id("toolchain_segfault"), id("process_segfault")]

    if Enum.any?(matches, &(&1.rule in segv)) do
      matches
    else
      rule = Enum.find(@rules, &(&1.name == "process_segfault"))
      [finding(rule, "exit code 139", 0) | matches]
    end
  end

  defp add_exit_code([], %{exit_code: code}) when is_integer(code) and code != 0 do
    rule = Enum.find(@rules, &(&1.name == "unrecognized_failure"))
    [finding(rule, "exit code #{code}", 0)]
  end

  defp add_exit_code(matches, _ctx), do: matches

  defp rank(%{layer: layer}), do: Enum.find_index(@layers, &(&1 == layer))

  defp order(%{rule: rule_id}) do
    Enum.find_index(@rules, &(id(&1.name) == rule_id)) || length(@rules)
  end
end
