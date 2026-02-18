-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- FFI function type signatures for the Zig C ABI bridge.
-- Each constructor in FFIFunction declares a typed foreign function.
-- Links to REST endpoint definitions for type-level verification.

module Hypatia.ABI.FFI

import Hypatia.ABI.Types
import Hypatia.ABI.REST

%default total

||| C ABI return code: 0 = success, non-zero = error
public export
CResult : Type
CResult = Int

||| Foreign function declaration linking Zig export name to Idris2 types.
||| Each constructor corresponds to an `export fn` in ffi/zig/src/main.zig.
public export
data FFIFunction : Type where
  ||| Health check — reads verisimdb-data dirs, counts files, returns JSON.
  ||| Corresponds to: GET /health, GET /status
  HealthCheck : FFIFunction

  ||| Scan repo — reads scans/{repo}.json, returns scan result.
  ||| Corresponds to: GET /api/v1/scans/:repo
  ScanRepo : (repo : String) -> FFIFunction

  ||| Dispatch finding — appends to dispatch/pending.jsonl.
  ||| Corresponds to: POST /api/v1/dispatch
  Dispatch : (entry : DispatchEntry) -> FFIFunction

  ||| Record outcome — appends to outcomes/YYYY-MM.jsonl.
  ||| Corresponds to: POST /api/v1/outcomes
  RecordOutcome : (record : OutcomeRecord) -> FFIFunction

  ||| Force learning cycle — writes .force-learning signal file.
  ||| Corresponds to: POST /api/v1/learning/force
  ForceLearningCycle : FFIFunction

  ||| Get confidence — reads recipes/recipe-*.json, extracts confidence.
  ||| Corresponds to: GET /api/v1/recipes/:id (confidence field)
  GetConfidence : (recipeId : String) -> FFIFunction

||| Return type of each FFI function
public export
ffiReturnType : FFIFunction -> Type
ffiReturnType HealthCheck = ApiResponse HealthStatus
ffiReturnType (ScanRepo _) = ApiResponse ScanResult
ffiReturnType (Dispatch _) = ApiResponse ()
ffiReturnType (RecordOutcome _) = ApiResponse ()
ffiReturnType ForceLearningCycle = ApiResponse ()
ffiReturnType (GetConfidence _) = ApiResponse Confidence

||| Proof that every FFI function returns an ApiResponse-wrapped type
public export
ffiReturnsApiResponse : (f : FFIFunction) -> (a : Type ** ffiReturnType f = ApiResponse a)
ffiReturnsApiResponse HealthCheck = (HealthStatus ** Refl)
ffiReturnsApiResponse (ScanRepo _) = (ScanResult ** Refl)
ffiReturnsApiResponse (Dispatch _) = (() ** Refl)
ffiReturnsApiResponse (RecordOutcome _) = (() ** Refl)
ffiReturnsApiResponse ForceLearningCycle = (() ** Refl)
ffiReturnsApiResponse (GetConfidence _) = (Confidence ** Refl)
