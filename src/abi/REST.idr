-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Hypatia ABI — REST API Definitions
-- Defines REST endpoints as dependent types.
-- All endpoints return ApiResponse-wrapped typed responses.

module Hypatia.ABI.REST

import Hypatia.ABI.Types

%default total

||| HTTP methods
public export
data HttpMethod = GET | POST | PUT | DELETE | PATCH

||| A REST endpoint with typed request body and response
public export
record Endpoint where
  constructor MkEndpoint
  method : HttpMethod
  path : String
  description : String
  requestBody : Type
  responseBody : Type
  requiresAuth : Bool

||| Health and status endpoints
public export
healthEndpoints : List Endpoint
healthEndpoints =
  [ MkEndpoint GET "/health" "Overall health status" () (ApiResponse (List (String, HealthStatus))) False
  , MkEndpoint GET "/status" "Detailed system status" () (ApiResponse (List (String, String))) False
  , MkEndpoint GET "/metrics" "Prometheus-compatible metrics" () String False
  ]

||| Scanning endpoints
public export
scanEndpoints : List Endpoint
scanEndpoints =
  [ MkEndpoint GET "/api/v1/scans/:repo" "Get scan results for a repo" () (ApiResponse ScanResult) False
  , MkEndpoint GET "/api/v1/scans" "List all scan results" () (ApiResponse (List ScanResult)) False
  , MkEndpoint POST "/api/v1/scans/:repo" "Trigger a new scan" () (ApiResponse ScanResult) True
  ]

||| Pattern endpoints
public export
patternEndpoints : List Endpoint
patternEndpoints =
  [ MkEndpoint GET "/api/v1/patterns" "List all patterns" () (ApiResponse (List Pattern)) False
  , MkEndpoint GET "/api/v1/patterns/:id" "Get pattern by ID" () (ApiResponse Pattern) False
  , MkEndpoint GET "/api/v1/patterns/severity/:level" "Patterns by severity" () (ApiResponse (List Pattern)) False
  , MkEndpoint GET "/api/v1/patterns/search" "Search patterns" () (ApiResponse (List Pattern)) False
  ]

||| Recipe endpoints
public export
recipeEndpoints : List Endpoint
recipeEndpoints =
  [ MkEndpoint GET "/api/v1/recipes" "List all recipes" () (ApiResponse (List Recipe)) False
  , MkEndpoint GET "/api/v1/recipes/:id" "Get recipe by ID" () (ApiResponse Recipe) False
  , MkEndpoint PUT "/api/v1/recipes/:id/confidence" "Update confidence" Double (ApiResponse Double) True
  ]

||| Dispatch endpoints
public export
dispatchEndpoints : List Endpoint
dispatchEndpoints =
  [ MkEndpoint POST "/api/v1/dispatch" "Dispatch finding to fleet" DispatchEntry (ApiResponse String) True
  , MkEndpoint GET "/api/v1/dispatch/pending" "List pending dispatches" () (ApiResponse (List DispatchEntry)) False
  , MkEndpoint GET "/api/v1/dispatch/history" "Dispatch history" () (ApiResponse (List DispatchEntry)) False
  ]

||| Outcome and learning endpoints
public export
outcomeEndpoints : List Endpoint
outcomeEndpoints =
  [ MkEndpoint POST "/api/v1/outcomes" "Record fix outcome" OutcomeRecord (ApiResponse String) True
  , MkEndpoint GET "/api/v1/outcomes/:recipe" "Outcomes for recipe" () (ApiResponse (List OutcomeRecord)) False
  , MkEndpoint POST "/api/v1/learning/cycle" "Force learning cycle" () (ApiResponse Nat) True
  , MkEndpoint GET "/api/v1/learning/status" "Learning scheduler status" () (ApiResponse (List (String, String))) False
  ]

||| All endpoints combined
public export
allEndpoints : List Endpoint
allEndpoints =
  healthEndpoints ++ scanEndpoints ++ patternEndpoints ++
  recipeEndpoints ++ dispatchEndpoints ++ outcomeEndpoints

||| Proof: total endpoint count
public export
totalEndpointCount : Nat
totalEndpointCount = length allEndpoints
