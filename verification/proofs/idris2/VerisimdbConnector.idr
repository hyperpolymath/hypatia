-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia — VerisimdbConnector Proofs
--
-- Formal verification of the VeriSimDB connector data integrity:
--   1. Completeness:  all mandatory fields in the DB map to the ABI types
--   2. Soundness:  ill-formed data is rejected (no "unknown" fallbacks in the core)
--   3. Preservation:  findings are not corrupted during transit to Logtalk facts
--
-- Corresponds to: lib/verisimdb_connector.ex

module VerisimdbConnector

import Data.String
import Data.Maybe

%default total

------------------------------------------------------------------------
-- Section 0: ABI types (mirror of Hypatia.ABI.Types)
-- Defined locally so this proof obligation type-checks standalone:
-- verify-proofs.yml runs `idris2 --check` per file with no package path.
------------------------------------------------------------------------

||| Severity levels (mirrors Hypatia.ABI.Types.Severity).
public export
data Severity = Critical | High | Medium | Low | Info

||| A canonical weakness pattern (mirrors Hypatia.ABI.Types.Pattern).
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

------------------------------------------------------------------------
-- Section 1: Raw Data Representation
------------------------------------------------------------------------

||| A raw finding as it might appear in a VeriSimDB JSON/VCL response.
||| Fields are Maybe because the DB schema is more flexible than our ABI.
public export
record RawFinding where
  constructor MkRawFinding
  repo : Maybe String
  file : Maybe String
  category : Maybe String
  severity : Maybe String
  line : Maybe Nat
  cwe : Maybe String

||| A raw scan result.
public export
record RawScan where
  constructor MkRawScan
  repo : Maybe String
  weakPoints : List RawFinding

------------------------------------------------------------------------
-- Section 2: Conversion Correctness
------------------------------------------------------------------------

||| Convert a raw string severity to the typed Severity enum.
public export
parseSeverity : String -> Maybe Severity
parseSeverity "critical" = Just Critical
parseSeverity "high"     = Just High
parseSeverity "medium"   = Just Medium
parseSeverity "low"      = Just Low
parseSeverity "info"     = Just Info
parseSeverity _          = Nothing

||| Proof: parseSeverity is total — every input either maps to some
||| severity or to Nothing (there is no third "stuck" outcome).
public export
parseSeverityTotal : (s : String)
                  -> Either (sev : Severity ** parseSeverity s = Just sev)
                            (parseSeverity s = Nothing)
parseSeverityTotal s with (parseSeverity s)
  parseSeverityTotal s | Just sev = Left (sev ** Refl)
  parseSeverityTotal s | Nothing  = Right Refl

||| Convert a RawFinding to the clean Pattern type.
||| This function ensures that all mandatory fields are present.
public export
cleanFinding : RawFinding -> Maybe Pattern
cleanFinding raw = do
  r    <- raw.repo
  f    <- raw.file
  cat  <- raw.category
  sevS <- raw.severity
  sev  <- parseSeverity sevS
  ln   <- raw.line
  pure $ MkPattern "P-ID" cat sev [r] f ln raw.cwe

||| Proof: completeness — if all fields are present, cleanFinding succeeds.
public export
cleanFindingCompleteness : (r, f, cat, sevS : String) -> (sev : Severity) -> (ln : Nat) -> (c : Maybe String)
                        -> parseSeverity sevS = Just sev
                        -> cleanFinding (MkRawFinding (Just r) (Just f) (Just cat) (Just sevS) (Just ln) c)
                           = Just (MkPattern "P-ID" cat sev [r] f ln c)
cleanFindingCompleteness r f cat sevS sev ln c prf =
  rewrite prf in Refl

||| Proof: soundness — if any mandatory field is missing, cleanFinding fails.
public export
cleanFindingSoundness : (raw : RawFinding)
                     -> (Either (raw.repo = Nothing)
                         (Either (raw.file = Nothing)
                          (Either (raw.category = Nothing)
                           (Either (raw.severity = Nothing)
                            (raw.line = Nothing)))))
                     -> cleanFinding raw = Nothing
-- One of the first four mandatory fields is Nothing: the do-block
-- short-circuits before parseSeverity, so cleanFinding = Nothing directly.
cleanFindingSoundness (MkRawFinding Nothing _ _ _ _ _) _ = Refl
cleanFindingSoundness (MkRawFinding (Just _) Nothing _ _ _ _) _ = Refl
cleanFindingSoundness (MkRawFinding (Just _) (Just _) Nothing _ _ _) _ = Refl
cleanFindingSoundness (MkRawFinding (Just _) (Just _) (Just _) Nothing _ _) _ = Refl
-- severity is present but line is Nothing: parseSeverity runs first, so we
-- case on it — either branch then hits `Nothing` for the line bind.
cleanFindingSoundness (MkRawFinding (Just _) (Just _) (Just _) (Just sevS) Nothing _) _ with (parseSeverity sevS)
  cleanFindingSoundness (MkRawFinding (Just _) (Just _) (Just _) (Just sevS) Nothing _) _ | Nothing = Refl
  cleanFindingSoundness (MkRawFinding (Just _) (Just _) (Just _) (Just sevS) Nothing _) _ | Just _ = Refl
-- all mandatory fields present: if parseSeverity fails, cleanFinding = Nothing;
-- if it succeeds, cleanFinding = Just _, so the hypothesis (some field = Nothing)
-- is contradictory and is refuted.
cleanFindingSoundness (MkRawFinding (Just _) (Just _) (Just _) (Just sevS) (Just _) _) hyp with (parseSeverity sevS)
  cleanFindingSoundness (MkRawFinding (Just _) (Just _) (Just _) (Just sevS) (Just _) _) _ | Nothing = Refl
  cleanFindingSoundness (MkRawFinding (Just _) (Just _) (Just _) (Just sevS) (Just _) _) hyp | Just _ =
    case hyp of
      Left p => absurd p
      Right (Left p) => absurd p
      Right (Right (Left p)) => absurd p
      Right (Right (Right (Left p))) => absurd p
      Right (Right (Right (Right p))) => absurd p

------------------------------------------------------------------------
-- Section 3: Transit to Logtalk Facts
------------------------------------------------------------------------

||| Representation of a Logtalk fact.
public export
record LogtalkFact where
  constructor MkFact
  repo : String
  file : String
  category : String
  severity : Severity

||| Transform a clean Pattern into a Logtalk fact.
||| This models the logic in VerisimdbConnector.to_logtalk_facts.
public export
toFact : Pattern -> LogtalkFact
toFact p =
  let r = case p.affectedRepos of
            [] => "unknown"
            (h :: _) => h
  in MkFact r p.file p.description p.severity

||| Proof: data preservation — fields in the Fact match the Pattern exactly.
public export
factPreservesData : (p : Pattern) -> (r : String) -> p.affectedRepos = [r]
                 -> ( (toFact p).repo = r
                    , (toFact p).file = p.file
                    , (toFact p).category = p.description
                    , (toFact p).severity = p.severity )
factPreservesData p r prf =
  rewrite prf in (Refl, Refl, Refl, Refl)
