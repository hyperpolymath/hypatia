;; SPDX-License-Identifier: AGPL-3.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for cicd-hyper-a
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "cicd-hyper-a")
  (type "automation-engine")
  (purpose "Neurosymbolic CI/CD automation with Logtalk rule engine for cross-repo security and quality enforcement")

  (position-in-ecosystem
    (category "devops-automation")
    (subcategory "ci-cd-security")
    (unique-value
      ("Logtalk-based rule engine for CI/CD")
      ("Neurosymbolic: neural learning + symbolic execution")
      ("Cross-repo hook wave propagation")
      ("Pan-forge support: GitHub, GitLab, Bitbucket, Codeberg")))

  (related-projects
    ;; Core infrastructure
    (project "ambientops"
      (relationship "parent")
      (description "Umbrella project for ambient computing operations")
      (integration "cicd-hyper-a is the CI/CD automation layer of ambientops"))

    (project "git-dispatcher"
      (relationship "coordinator")
      (description "Central coordination for git operations across forges")
      (integration "cicd-hyper-a receives dispatched events for CI/CD automation")
      (update-protocol "When committing, notify git-dispatcher for documentation updates"))

    (project "gitvisor"
      (relationship "sibling")
      (description "Git repository supervision and monitoring")
      (integration "gitvisor monitors repos, cicd-hyper-a automates fixes"))

    (project ".git-private-farm"
      (relationship "infrastructure")
      (description "Private git hook and template distribution")
      (integration "cicd-hyper-a generates hooks, .git-private-farm distributes them"))

    ;; Satellites
    (project "repo-slm-augmentor"
      (relationship "component")
      (description "SLM augmentation for security knowledge")
      (integration "Provides security_errors.lgt knowledge base"))

    (project "echidnabot"
      (relationship "satellite")
      (description "Discord/notification bot")
      (integration "Reports CI/CD issues from cicd-hyper-a"))

    (project "rhodibot"
      (relationship "satellite")
      (description "Repository health dashboard bot")
      (integration "Displays cicd-hyper-a metrics"))

    ;; Planned satellites (via ambientops)
    (project "system-freeze-ejector"
      (relationship "planned-satellite")
      (description "Off-machine kernel dump for system recovery")
      (integration "Preserves state during system crashes"))

    (project "system-flare"
      (relationship "planned-satellite")
      (description "Rapid system halt with state preservation")
      (integration "Emergency stop with data safety"))

    ;; External dependencies
    (project "OpenSSF Scorecard"
      (relationship "external-standard")
      (description "Security scoring for open source")
      (integration "cicd-hyper-a enforces Scorecard compliance"))

    (project "Virtuoso Open Source"
      (relationship "planned-database")
      (description "RDF/SPARQL database for rule storage")
      (integration "Will store learned rules and error patterns")))

  (what-this-is
    ("Logtalk rule engine for CI/CD automation")
    ("Error catalog with severity classification")
    ("Auto-fix pattern generator")
    ("Hook wave propagation system")
    ("Cross-repo security enforcer")
    ("Neurosymbolic learning system"))

  (what-this-is-not
    ("Not a CI/CD platform replacement - works alongside GitHub Actions")
    ("Not a monitoring tool - gitvisor handles monitoring")
    ("Not a secrets manager - uses existing secret stores")
    ("Not a deployment pipeline - focuses on security/quality")))
