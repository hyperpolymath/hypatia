-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Hypatia ABI — gRPC Service Definitions
-- Defines the gRPC service contract as dependent types.
-- Maps to protobuf service definitions via the FFI layer.

module Hypatia.ABI.GRPC

import Hypatia.ABI.Types

%default total

||| gRPC method types
public export
data MethodType = Unary | ServerStream | ClientStream | BidiStream

||| A gRPC method with typed request and response
public export
record GrpcMethod where
  constructor MkGrpcMethod
  name : String
  methodType : MethodType
  requestType : Type
  responseType : Type

||| Hypatia Scanner Service — unary RPCs for scanning and querying
public export
scannerService : List GrpcMethod
scannerService =
  [ MkGrpcMethod "ScanRepo" Unary String (ApiResponse ScanResult)
  , MkGrpcMethod "GetScanResult" Unary String (ApiResponse ScanResult)
  , MkGrpcMethod "ListScans" Unary (Nat, Nat) (ApiResponse (List ScanResult))
  , MkGrpcMethod "SearchPatterns" Unary String (ApiResponse (List Pattern))
  ]

||| Hypatia Dispatch Service — RPCs for fleet coordination
public export
dispatchService : List GrpcMethod
dispatchService =
  [ MkGrpcMethod "DispatchFinding" Unary DispatchEntry (ApiResponse String)
  , MkGrpcMethod "GetRecipe" Unary String (ApiResponse Recipe)
  , MkGrpcMethod "ListRecipes" Unary Double (ApiResponse (List Recipe))
  , MkGrpcMethod "RecordOutcome" Unary OutcomeRecord (ApiResponse String)
  , MkGrpcMethod "ForceLearningCycle" Unary () (ApiResponse Nat)
  ]

||| Hypatia Stream Service — server-streaming RPCs for monitoring
public export
streamService : List GrpcMethod
streamService =
  [ MkGrpcMethod "StreamScans" ServerStream () ScanResult
  , MkGrpcMethod "StreamOutcomes" ServerStream () OutcomeRecord
  , MkGrpcMethod "StreamHealthChanges" ServerStream () (String, HealthStatus)
  , MkGrpcMethod "StreamConfidenceChanges" ServerStream String (String, Double)
  ]

||| Hypatia Health Service — standard gRPC health check protocol
public export
healthService : List GrpcMethod
healthService =
  [ MkGrpcMethod "Check" Unary String (ApiResponse HealthStatus)
  , MkGrpcMethod "Watch" ServerStream String (ApiResponse HealthStatus)
  ]
