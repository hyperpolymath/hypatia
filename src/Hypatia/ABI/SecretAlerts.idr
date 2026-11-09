-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia ABI — the `hypatia secret-alerts` line contract
--
-- `hypatia secret-alerts --owner O --repo R` prints one JSON line and exits.
-- A bash/jq consumer (the private estate secret-scanning sweep) reads the line
-- and the exit code, so the pair is a cross-language wire contract: PROOF-POLICY
-- §1 assigns it to Idris2.  The Elixir producer is
-- `Hypatia.CLI.secret_alerts_output/3`; test/cli_test.exs ("secret-alerts")
-- exercises it.
--
-- The contract:
--   * status "ok": the alerts were read.  The line carries `total` and the
--     findings; `total` is the number of findings; exit code 0.
--   * status "error": the alerts could not be read (refused token, secret
--     scanning disabled, timeout, pagination cap).  The line carries a
--     non-empty reason; exit code 3.
--   * A sweep classifies a line as Clean (ok, no findings), WithAlerts (ok,
--     at least one finding) or Unread (error).  "Could not look" is never
--     "clean".
--   * Every finding is a pattern-supported HYP-01 verdict, so it is never
--     reported at level "sound".
--
-- What is proved here (%default total, no proof-bypass primitives):
--   * the exit code is 0 exactly for status Ok, and 3 for Error
--     (`exitZeroIsOk`, `errorExitsThree`), and the status can be recovered
--     from the exit code (`statusOfExitRoundTrip`);
--   * no exit code other than 0 reads as Ok (`nonZeroNotOk`), so a crash
--     (exit 1) or a usage error (exit 2) is never taken for a clean read;
--   * an error line is always Unread, and a Clean line has exit 0
--     (`errorIsUnread`, `cleanExitsZero`, `unreadIsError`);
--   * `total` equals the number of findings by construction (the findings
--     are a vector indexed by `total`; `totalIsLength`);
--   * every finding is pattern-supported and never "sound"
--     (`findingNeverSound`).
--
-- What is NOT proved here:
--   * that the Elixir producer emits exactly this shape: the ExUnit tests
--     check it on sample data (TESTED), and this module does not prove
--     runtime refinement (PROOF-POLICY §2.1: a model does not prove the
--     implementation);
--   * anything about GitHub's API: pagination, rate limits and token scope
--     are the producer's job.

module Hypatia.ABI.SecretAlerts

import Data.List
import Data.Vect
import Hypatia.ABI.Verdict

%default total

------------------------------------------------------------------------
-- Section 1: Status and exit code
------------------------------------------------------------------------

||| Whether the alerts were read.
public export
data Status = Ok | Error

public export
Uninhabited (Ok = Error) where
  uninhabited Refl impossible

public export
Uninhabited (Error = Ok) where
  uninhabited Refl impossible

||| The process exit code for each status.
public export
exitCode : Status -> Nat
exitCode Ok    = 0
exitCode Error = 3

||| Read a status back from an exit code.  Any code but 0 and 3 (a crash,
||| a usage error) is not a status line at all.
public export
statusOfExit : Nat -> Maybe Status
statusOfExit 0 = Just Ok
statusOfExit 3 = Just Error
statusOfExit _ = Nothing

||| The exit code determines the status.
public export
statusOfExitRoundTrip : (s : Status) -> statusOfExit (exitCode s) = Just s
statusOfExitRoundTrip Ok    = Refl
statusOfExitRoundTrip Error = Refl

||| Exit 0 means Ok.
public export
exitZeroIsOk : (s : Status) -> exitCode s = 0 -> s = Ok
exitZeroIsOk Ok    Refl = Refl
exitZeroIsOk Error prf  = absurd prf

||| An error exits 3, never 0.
public export
errorExitsThree : exitCode Error = 3
errorExitsThree = Refl

||| No non-zero exit code reads as Ok.
public export
nonZeroNotOk : (n : Nat) -> Not (n = 0) -> Not (statusOfExit n = Just Ok)
nonZeroNotOk 0 nz _ = nz Refl
nonZeroNotOk 1 _ prf = absurd prf
nonZeroNotOk 2 _ prf = absurd prf
nonZeroNotOk 3 _ prf = absurd (justInjective prf)
  where
    justInjective : Just Error = Just Ok -> Error = Ok
    justInjective Refl impossible
nonZeroNotOk (S (S (S (S k)))) _ prf = absurd prf

------------------------------------------------------------------------
-- Section 2: The line
------------------------------------------------------------------------

||| A secret-scanning finding: a pattern-supported verdict (rule SSA001 to
||| SSA004).  The rule cannot assert a level; it is derived (HYP-01).
public export
record SecretFinding where
  constructor MkSecretFinding
  verdict   : Verdict
  isPattern : support verdict = Pattern

||| A non-empty string, for an error reason.
public export
data NonEmptyString : String -> Type where
  MkNonEmptyString : {0 s : String} -> NonEmpty (unpack s) -> NonEmptyString s

||| One output line, indexed by its status.  An Ok line's findings are a
||| vector of length `count` (the wire `total`), so it cannot disagree with them.
public export
data AlertLine : Status -> Type where
  OkLine  : (owner, repo : String) -> (count : Nat)
         -> (findings : Vect count SecretFinding) -> AlertLine Ok
  ErrLine : (owner, repo : String) -> (reason : String)
         -> NonEmptyString reason -> AlertLine Error

||| The exit code a line is printed with.
public export
exitOf : {s : Status} -> AlertLine s -> Nat
exitOf {s} _ = exitCode s

||| `total` is the number of findings.
public export
totalIsLength : (owner, repo : String) -> (n : Nat) -> (fs : Vect n SecretFinding)
             -> length fs = n
totalIsLength _ _ n fs = lengthCorrect fs

------------------------------------------------------------------------
-- Section 3: How a sweep reads a line
------------------------------------------------------------------------

||| The three outcomes a sweep distinguishes.
public export
data Outcome = Clean | WithAlerts | Unread

public export
Uninhabited (Unread = Clean) where
  uninhabited Refl impossible

||| Classify a line.
public export
outcome : AlertLine s -> Outcome
outcome (OkLine _ _ 0 _)     = Clean
outcome (OkLine _ _ (S _) _) = WithAlerts
outcome (ErrLine _ _ _ _)    = Unread

||| An error line is always Unread: "could not look" is never "clean".
public export
errorIsUnread : (l : AlertLine Error) -> outcome l = Unread
errorIsUnread (ErrLine _ _ _ _) = Refl

||| An error line is never Clean.
public export
errorNeverClean : (l : AlertLine Error) -> Not (outcome l = Clean)
errorNeverClean l prf = absurd (trans (sym (errorIsUnread l)) prf)

||| A Clean line was printed with exit code 0.
public export
cleanExitsZero : {s : Status} -> (l : AlertLine s) -> outcome l = Clean -> exitOf l = 0
cleanExitsZero (OkLine _ _ 0 _)     Refl = Refl
cleanExitsZero (OkLine _ _ (S _) _) prf  = absurd prf
  where
    Uninhabited (WithAlerts = Clean) where
      uninhabited Refl impossible
cleanExitsZero (ErrLine _ _ _ _)    prf  = absurd prf

||| An Unread line has status Error.
public export
unreadIsError : {s : Status} -> (l : AlertLine s) -> outcome l = Unread -> s = Error
unreadIsError (OkLine _ _ 0 _)     prf = absurd prf
  where
    Uninhabited (Clean = Unread) where
      uninhabited Refl impossible
unreadIsError (OkLine _ _ (S _) _) prf = absurd prf
  where
    Uninhabited (WithAlerts = Unread) where
      uninhabited Refl impossible
unreadIsError (ErrLine _ _ _ _)    Refl = Refl

------------------------------------------------------------------------
-- Section 4: Findings are never "sound"
------------------------------------------------------------------------

||| No secret-scanning finding is reported at level "sound".
public export
findingNeverSound : (f : SecretFinding) -> Not (level (verdict f) = LvSound)
findingNeverSound f = patternNeverSoundLevel (verdict f) (isPattern f)
