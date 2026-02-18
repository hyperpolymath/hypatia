-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Hypatia ABI — GraphQL Schema Definitions
-- Defines the GraphQL operations as dependent types.
-- The Zig FFI layer generates the actual GraphQL schema from these types.

module Hypatia.ABI.GraphQL

import Hypatia.ABI.Types

%default total

||| GraphQL query operations (read-only)
public export
data Query : Type -> Type where
  ||| Get health status of all Hypatia components
  HealthQuery : Query (ApiResponse (List (String, HealthStatus)))

  ||| Get scan results for a specific repository
  ScanQuery : (repo : String) -> Query (ApiResponse ScanResult)

  ||| List all scanned repositories with pagination
  ReposQuery : (offset : Nat) -> (limit : Nat) -> Query (ApiResponse (List ScanResult))

  ||| Get a specific recipe by ID
  RecipeQuery : (recipeId : String) -> Query (ApiResponse Recipe)

  ||| List all recipes with optional confidence filter
  RecipesQuery : (minConfidence : Double) -> Query (ApiResponse (List Recipe))

  ||| Get outcomes for a recipe
  OutcomesQuery : (recipeId : String) -> Query (ApiResponse (List OutcomeRecord))

  ||| Get patterns by severity
  PatternsBySeverity : (severity : Severity) -> Query (ApiResponse (List Pattern))

  ||| Search patterns by keyword
  SearchPatterns : (keyword : String) -> Query (ApiResponse (List Pattern))

||| GraphQL mutation operations (write)
public export
data Mutation : Type -> Type where
  ||| Trigger a scan for a repository
  TriggerScan : (repo : String) -> Mutation (ApiResponse ScanResult)

  ||| Dispatch a finding to a fleet bot
  DispatchFinding : (entry : DispatchEntry) -> Mutation (ApiResponse String)

  ||| Record a fix outcome (feeds the learning loop)
  RecordOutcome : (record : OutcomeRecord) -> Mutation (ApiResponse String)

  ||| Force a learning cycle (normally automatic every 5 min)
  ForceLearningCycle : Mutation (ApiResponse Nat)

  ||| Update recipe confidence manually
  UpdateConfidence : (recipeId : String) -> Mutation (ApiResponse Double)

||| GraphQL subscription operations (real-time)
public export
data Subscription : Type -> Type where
  ||| Stream new scan results as they arrive
  OnScanComplete : Subscription ScanResult

  ||| Stream outcome recordings (for monitoring)
  OnOutcomeRecorded : Subscription OutcomeRecord

  ||| Stream confidence changes (for drift monitoring)
  OnConfidenceChange : (recipeId : String) -> Subscription (String, Double)

  ||| Stream health status changes
  OnHealthChange : Subscription (String, HealthStatus)

||| Proof that all queries return ApiResponse-wrapped types
public export
queryReturnsApiResponse : (q : Query a) -> (b : Type ** a = ApiResponse b)
queryReturnsApiResponse HealthQuery = (_ ** Refl)
queryReturnsApiResponse (ScanQuery _) = (_ ** Refl)
queryReturnsApiResponse (ReposQuery _ _) = (_ ** Refl)
queryReturnsApiResponse (RecipeQuery _) = (_ ** Refl)
queryReturnsApiResponse (RecipesQuery _) = (_ ** Refl)
queryReturnsApiResponse (OutcomesQuery _) = (_ ** Refl)
queryReturnsApiResponse (PatternsBySeverity _) = (_ ** Refl)
queryReturnsApiResponse (SearchPatterns _) = (_ ** Refl)

||| Proof that all mutations return ApiResponse-wrapped types
public export
mutationReturnsApiResponse : (m : Mutation a) -> (b : Type ** a = ApiResponse b)
mutationReturnsApiResponse (TriggerScan _) = (_ ** Refl)
mutationReturnsApiResponse (DispatchFinding _) = (_ ** Refl)
mutationReturnsApiResponse (RecordOutcome _) = (_ ** Refl)
mutationReturnsApiResponse ForceLearningCycle = (_ ** Refl)
mutationReturnsApiResponse (UpdateConfidence _) = (_ ** Refl)
