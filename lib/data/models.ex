# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Data.Models do
  @moduledoc """
  Data models for the Hypatia intelligence layer.

  These models map to ArangoDB document and edge collections in the
  federated architecture. verisimdb-data (git) remains the canonical
  source of truth; ArangoDB provides graph query, trust traversal,
  and neural state persistence alongside it.

  Entity-Relationship Model:

      Repo ──scans──> Finding ──matches──> Pattern
        │                │                    │
        │           dispatched_to         has_recipe
        │                │                    │
        │                v                    v
        ├──depends_on──> Bot ──executes──> Recipe
        │                │                    │
        │           produces             has_outcome
        │                │                    │
        v                v                    v
    Contributor     DispatchEntry        Outcome
                         │                    │
                    generates            tracked_in
                         │                    │
                         v                    v
                      Anomaly         ConfidenceHistory
  """

  # --- Document Collections ---

  defmodule Repo do
    @moduledoc "Repository under Hypatia management"
    defstruct [
      :_key, :name, :forge, :owner, :url,
      :languages, :scorecard_score, :health_score,
      :trust_score, :last_scanned, :last_dispatched,
      :finding_count, :fixed_count, :weak_point_count,
      enrolled: true, status: :active
    ]
  end

  defmodule Finding do
    @moduledoc "Individual finding from a scan"
    defstruct [
      :_key, :repo, :pattern_id, :severity, :category,
      :location, :message, :confidence, :scan_timestamp,
      :status,  # :open, :dispatched, :fixed, :false_positive, :wont_fix
      :fix_recipe_id, :dispatched_to_bot, :verisim_id
    ]
  end

  defmodule Pattern do
    @moduledoc "Canonical deduplicated pattern (PA001-PA020+)"
    defstruct [
      :_key, :pattern_id, :name, :description,
      :severity, :category, :frequency,
      :first_seen, :last_seen, :repo_count,
      :auto_fixable, :recipe_id
    ]
  end

  defmodule Bot do
    @moduledoc "Fleet bot (rhodibot, echidnabot, etc.)"
    defstruct [
      :_key, :name, :bot_id, :capabilities,
      :trust_score, :success_rate, :total_dispatches,
      :consecutive_failures, :last_active,
      quarantined: false, quarantine_reason: nil,
      quarantine_until: nil
    ]
  end

  defmodule Recipe do
    @moduledoc "Fix recipe with confidence tracking"
    defstruct [
      :_key, :recipe_id, :name, :description,
      :strategy,  # :eliminate, :substitute, :control
      :language, :fix_type,  # :replace, :inject, :delete, :command, :pr
      :confidence, :confidence_floor,
      :success_count, :failure_count, :fp_count,
      :created, :last_applied, :proven_module
    ]
  end

  defmodule Outcome do
    @moduledoc "Fix outcome record (success/failure/false_positive)"
    defstruct [
      :_key, :recipe_id, :repo, :bot, :file,
      :outcome,  # :success, :failure, :false_positive
      :timestamp, :confidence_before, :confidence_after,
      :batch_id, :details, :verisim_proof_id
    ]
  end

  defmodule Contributor do
    @moduledoc "Human or bot contributor to fixes"
    defstruct [
      :_key, :name, :type,  # :human, :bot
      :trust_score, :fixes_submitted, :fixes_accepted,
      :fixes_rejected, :false_positives_reported,
      :domains, :last_active
    ]
  end

  defmodule ConfidenceHistory do
    @moduledoc "Time-series record for recipe confidence trajectory"
    defstruct [
      :_key, :recipe_id, :timestamp,
      :confidence, :event_type,  # :outcome, :decay, :manual, :learning_cycle
      :delta, :trigger  # What caused this change
    ]
  end

  defmodule Anomaly do
    @moduledoc "Detected anomaly from LSM temporal analysis"
    defstruct [
      :_key, :event, :deviation, :timestamp,
      :source_network,  # :lsm, :esn, :moe, :rbf, :trust
      :severity,  # :info, :warning, :critical
      :resolved, :resolution, :resolution_timestamp
    ]
  end

  defmodule DispatchBatch do
    @moduledoc "Batch of dispatches for rollback capability"
    defstruct [
      :_key, :batch_id, :timestamp,
      :dispatch_count, :strategy,
      :status,  # :pending, :executing, :completed, :rolled_back
      :rollback_reason, :rollback_timestamp
    ]
  end

  defmodule NeuralState do
    @moduledoc "Persisted state of a neural network subsystem"
    defstruct [
      :_key, :network,  # :trust, :moe, :lsm, :esn, :rbf
      :state_data,  # Serialized network state
      :cycle_count, :last_updated,
      :training_samples, :accuracy_metrics, :verisim_tensor_id
    ]
  end

  # --- Edge Collections ---

  defmodule Edge do
    @moduledoc "Generic edge for graph relationships"
    defstruct [:_from, :_to, :weight, :metadata, :created, :updated]
  end

  # Named edge types for clarity
  defmodule TrustEdge do
    @moduledoc "Trust relationship between entities"
    defstruct [:_from, :_to, :trust_score, :evidence_count, :last_updated, :decay_rate]
  end

  defmodule DependencyEdge do
    @moduledoc "Dependency between repos"
    defstruct [:_from, :_to, :dependency_type, :version_constraint, :critical]
  end
end
