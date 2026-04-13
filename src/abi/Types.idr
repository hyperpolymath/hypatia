-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
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

-- ============================================================
-- Hexadeca-Connector — sixteen protocol adapters
-- ============================================================
--
-- Mirrors the Zig enum at `ffi/zig/src/hexadeca.zig` and the Rust
-- enum at `clients/rust/hypatia-client/src/connector.rs`. Wire
-- ordering is load-bearing — the integer value of each variant is
-- the C ABI id used by `hypatia_connector_name(id)`. Do not
-- renumber. The dependent-type proof `connectorWireIdInRange`
-- below pins the count at exactly 16.
--
-- Replaces the V-lang client at `api/v/hypatia.v` (deleted 2026-04-13).

||| The sixteen protocol connectors exposed by the Hypatia
||| hexadeca-connector surface. Order is the C ABI wire ordering;
||| see `Hypatia.ABI.Types.connectorWireId` for the mapping.
public export
data Connector
  = -- Core 12
    GRPC          -- 0
  | GraphQL       -- 1
  | REST          -- 2
  | FlatBuffers   -- 3
  | Bebop         -- 4
  | JsonRpc       -- 5
  | WebSocket     -- 6
  | MQTT          -- 7
  | TRPC          -- 8
  | CapnProto     -- 9
  | SOAP          -- 10
  | VeriSimDBRest -- 11
    -- Umoja-substrate 4
  | BSP           -- 12
  | SCIP          -- 13
  | IPFS          -- 14
  | ArrowFlight   -- 15

||| C ABI wire id of a connector. Must agree with the Zig enum.
public export
connectorWireId : Connector -> Nat
connectorWireId GRPC          = 0
connectorWireId GraphQL       = 1
connectorWireId REST          = 2
connectorWireId FlatBuffers   = 3
connectorWireId Bebop         = 4
connectorWireId JsonRpc       = 5
connectorWireId WebSocket     = 6
connectorWireId MQTT          = 7
connectorWireId TRPC          = 8
connectorWireId CapnProto     = 9
connectorWireId SOAP          = 10
connectorWireId VeriSimDBRest = 11
connectorWireId BSP           = 12
connectorWireId SCIP          = 13
connectorWireId IPFS          = 14
connectorWireId ArrowFlight   = 15

||| Canonical wire name of a connector. Must agree with
||| `Connector.name()` in `ffi/zig/src/hexadeca.zig`.
public export
connectorName : Connector -> String
connectorName GRPC          = "grpc"
connectorName GraphQL       = "graphql"
connectorName REST          = "rest"
connectorName FlatBuffers   = "flatbuffers"
connectorName Bebop         = "bebop"
connectorName JsonRpc       = "jsonrpc"
connectorName WebSocket     = "websocket"
connectorName MQTT          = "mqtt"
connectorName TRPC          = "trpc"
connectorName CapnProto     = "capnproto"
connectorName SOAP          = "soap"
connectorName VeriSimDBRest = "verisimdb-rest"
connectorName BSP           = "bsp"
connectorName SCIP          = "scip"
connectorName IPFS          = "ipfs"
connectorName ArrowFlight   = "arrow-flight"

||| All sixteen connectors in wire order.
public export
allConnectors : List Connector
allConnectors =
  [ GRPC, GraphQL, REST, FlatBuffers, Bebop, JsonRpc
  , WebSocket, MQTT, TRPC, CapnProto, SOAP, VeriSimDBRest
  , BSP, SCIP, IPFS, ArrowFlight
  ]

||| Proof that there are exactly sixteen connectors. Pins the
||| hexadeca invariant at the type level: any code that adds or
||| removes a connector must update this proof, which forces a
||| coordinated update of the Zig enum and the Rust client.
public export
connectorCount : length Hypatia.ABI.Types.allConnectors = 16
connectorCount = Refl

||| Bound port for a connector under a given base port. Layout
||| matches the V-lang reference (`base + id + 1`).
public export
connectorPort : (basePort : Nat) -> Connector -> Nat
connectorPort base c = base + connectorWireId c + 1
