-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Hypatia ABI — Core Types
-- Defines the type-safe interface for all Hypatia API operations.
-- These types are the single source of truth; all API protocols
-- (GraphQL, gRPC, REST) derive from these definitions.

module Hypatia.ABI.Types

%default total

||| Severity levels for findings, ordered by criticality
public export
data Severity = Critical | High | Medium | Low | Info

public export
Eq Severity where
  Critical == Critical = True
  High == High = True
  Medium == Medium = True
  Low == Low = True
  Info == Info = True
  _ == _ = False

public export
Ord Severity where
  compare Critical Critical = EQ
  compare Critical _ = LT
  compare _ Critical = GT
  compare High High = EQ
  compare High _ = LT
  compare _ High = GT
  compare Medium Medium = EQ
  compare Medium _ = LT
  compare _ Medium = GT
  compare Low Low = EQ
  compare Low _ = LT
  compare _ Low = GT
  compare Info Info = EQ

||| Safety triangle tiers (Eliminate > Substitute > Control)
public export
data TriangleTier = Eliminate | Substitute | Control

||| Dispatch strategy based on confidence level
public export
data DispatchStrategy = AutoExecute | Review | ReportOnly

||| Fix outcome from bot execution
public export
data Outcome = Success | Failure | FalsePositive

||| Bot identifiers in the fleet
public export
data BotId
  = Rhodibot
  | Echidnabot
  | Sustainabot
  | Glambot
  | Seambot
  | Cipherbot
  | Finishbot
  | Accessibilitybot
  | RobotRepoAutomaton

||| Confidence score — bounded between 0.0 and 1.0
||| The Nat parameter is confidence * 10000 for type-level precision
public export
record Confidence where
  constructor MkConfidence
  value : Double
  {auto prf : So (value >= 0.0 && value <= 1.0)}

||| A recipe for fixing a specific weakness pattern
public export
record Recipe where
  constructor MkRecipe
  id : String
  description : String
  confidence : Double
  autoFixable : Bool
  provenModule : Maybe String

||| A canonical weakness pattern
public export
record Pattern where
  constructor MkPattern
  patternId : String
  description : String
  severity : Severity
  affectedRepos : List String
  file : String
  line : Nat
  cwe : Maybe String

||| Health check status for a single component
public export
data HealthStatus = Pass | Warn | Fail

||| API response wrapper with typed content
public export
record ApiResponse (a : Type) where
  constructor MkApiResponse
  success : Bool
  data : Maybe a
  error : Maybe String
  timestamp : String

||| Scan result for a repository
public export
record ScanResult where
  constructor MkScanResult
  repo : String
  weakPoints : Nat
  patterns : List Pattern
  scannedAt : String

||| Dispatch manifest entry
public export
record DispatchEntry where
  constructor MkDispatchEntry
  bot : BotId
  repo : String
  file : String
  recipeId : String
  tier : TriangleTier
  strategy : DispatchStrategy

||| Outcome record for the learning loop
public export
record OutcomeRecord where
  constructor MkOutcomeRecord
  recipeId : String
  repo : String
  file : String
  outcome : Outcome
  timestamp : String
  bot : String
