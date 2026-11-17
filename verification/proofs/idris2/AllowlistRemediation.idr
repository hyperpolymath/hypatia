-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia — CI-health B-ALLOWLIST remediation is additive
--
-- scripts/ci-health/remediate.sh B-ALLOWLIST writes a repository's Actions
-- allow-list as its CURRENT patterns UNION the REQUIRED set (hyperpolymath/*
-- plus each line of scripts/ci-health/action-superset.txt as owner/repo@*),
-- and writes the github_owned_allowed / verified_allowed flags back exactly as
-- it read them. scripts/ci-health/detect.sh reports B-ALLOWLIST exactly when
-- some required pattern is missing. This module states that remediation and
-- proves:
--
--   1. `keepsCurrent`: every pattern allowed before is allowed after. The
--      remediation can never newly break a workflow that started before
--      (reachability is monotone under this additive change).
--   2. `coversRequired`: every required pattern is allowed after.
--   3. `onlyAdds`: everything allowed after was allowed before or is required.
--   4. `flagsKept`: both trust flags are unchanged, so no trust is widened.
--   5. `noWriteWhenCovered`: when nothing is missing, the list is unchanged.
--   6. `converges` / `idempotent`: after one remediation nothing is missing,
--      so detect.sh no longer fires and a second run changes nothing.
--   7. The four rows of the shared corpus (test/fixtures/ci-health/allowlist/)
--      are transcribed below with the patterns renamed to numbers. The counts
--      each reduces to are equations the type checker reduces. The corpus test
--      (test/ci-health-test.sh) runs the real remediate.sh and detect.sh on the
--      same fixtures, and checks the `-- corpus:` line and the equation under
--      it against each fixture's expected counts.
--
-- What this does NOT prove (recorded in verification/PROOF-STATUS.adoc):
--   * Runtime refinement. remediate.sh agrees with this model on the four
--     corpus rows only (TESTED), not in general.
--   * GitHub's semantics. That a pattern in patterns_allowed lets a matching
--     `uses:` start, and that the API stores what was PUT, are ASSUMED.
--   * Lists versus sets. The script deduplicates (jq `unique`); the model keeps
--     list order and states every property by membership (`Elem`), which is
--     the same under deduplication.

module AllowlistRemediation

import Data.List
import Data.List.Elem
import Decidable.Equality

%default total

------------------------------------------------------------------------
-- Section 1: The allow-list and the remediation
------------------------------------------------------------------------

||| A repository's selected-actions setting.
public export
record Allowlist a where
  constructor MkAllowlist
  githubOwned : Bool
  verified    : Bool
  patterns    : List a

||| The required patterns the current list does not hold, in required order.
||| detect.sh fires B-ALLOWLIST exactly when this is non-empty.
public export
missing : DecEq a => (current : List a) -> (required : List a) -> List a
missing cur [] = []
missing cur (r :: rs) with (isElem r cur)
  missing cur (r :: rs) | Yes _ = missing cur rs
  missing cur (r :: rs) | No _  = r :: missing cur rs

||| What remediate.sh writes: current UNION required, flags as read.
public export
remediate : DecEq a => (required : List a) -> Allowlist a -> Allowlist a
remediate req (MkAllowlist g v ps) = MkAllowlist g v (ps ++ missing ps req)

------------------------------------------------------------------------
-- Section 2: Membership lemmas
------------------------------------------------------------------------

appendLeft : Elem x xs -> Elem x (xs ++ ys)
appendLeft Here      = Here
appendLeft (There e) = There (appendLeft e)

appendRight : (xs : List a) -> Elem x ys -> Elem x (xs ++ ys)
appendRight []        e = e
appendRight (_ :: xs) e = There (appendRight xs e)

appendSplit : (xs : List a) -> Elem x (xs ++ ys) -> Either (Elem x xs) (Elem x ys)
appendSplit []        e         = Right e
appendSplit (_ :: xs) Here      = Left Here
appendSplit (_ :: xs) (There e) = case appendSplit xs e of
  Left l  => Left (There l)
  Right r => Right r

||| A required pattern is either already current or reported missing.
requiredCurrentOrMissing : DecEq a => (cur : List a) -> (req : List a)
                        -> Elem r req -> Either (Elem r cur) (Elem r (missing cur req))
requiredCurrentOrMissing cur (r :: rs) Here with (isElem r cur)
  requiredCurrentOrMissing cur (r :: rs) Here | Yes p = Left p
  requiredCurrentOrMissing cur (r :: rs) Here | No _  = Right Here
requiredCurrentOrMissing cur (q :: rs) (There e) with (isElem q cur)
  requiredCurrentOrMissing cur (q :: rs) (There e) | Yes _ = requiredCurrentOrMissing cur rs e
  requiredCurrentOrMissing cur (q :: rs) (There e) | No _  =
    case requiredCurrentOrMissing cur rs e of
      Left c  => Left c
      Right m => Right (There m)

||| Everything reported missing is required.
missingIsRequired : DecEq a => (cur : List a) -> (req : List a)
                 -> Elem x (missing cur req) -> Elem x req
missingIsRequired cur [] e impossible
missingIsRequired cur (r :: rs) e with (isElem r cur)
  missingIsRequired cur (r :: rs) e         | Yes _ = There (missingIsRequired cur rs e)
  missingIsRequired cur (r :: rs) Here      | No _  = Here
  missingIsRequired cur (r :: rs) (There e) | No _  = There (missingIsRequired cur rs e)

||| Nothing is missing from a list that holds every required pattern.
missingNilIfAllIn : DecEq a => (xs : List a) -> (req : List a)
                 -> ((r : a) -> Elem r req -> Elem r xs) -> missing xs req = []
missingNilIfAllIn xs [] _ = Refl
missingNilIfAllIn xs (r :: rs) all with (isElem r xs)
  missingNilIfAllIn xs (r :: rs) all | Yes _ =
    missingNilIfAllIn xs rs (\q, e => all q (There e))
  missingNilIfAllIn xs (r :: rs) all | No no = absurd (no (all r Here))

------------------------------------------------------------------------
-- Section 3: The properties
------------------------------------------------------------------------

||| 1. Monotone: nothing allowed before is lost.
export
keepsCurrent : DecEq a => (req : List a) -> (al : Allowlist a)
            -> Elem p (patterns al) -> Elem p (patterns (remediate req al))
keepsCurrent req (MkAllowlist g v ps) e = appendLeft e

||| 2. Covering: every required pattern is allowed after.
export
coversRequired : DecEq a => (req : List a) -> (al : Allowlist a)
              -> Elem r req -> Elem r (patterns (remediate req al))
coversRequired req (MkAllowlist g v ps) e = case requiredCurrentOrMissing ps req e of
  Left c  => appendLeft c
  Right m => appendRight ps m

||| 3. Sound: nothing is added beyond the required set.
export
onlyAdds : DecEq a => (req : List a) -> (al : Allowlist a)
        -> Elem p (patterns (remediate req al)) -> Either (Elem p (patterns al)) (Elem p req)
onlyAdds req (MkAllowlist g v ps) e = case appendSplit ps e of
  Left c  => Left c
  Right m => Right (missingIsRequired ps req m)

||| 4. No trust is widened: both flags are written back as read.
export
flagsKept : DecEq a => (req : List a) -> (al : Allowlist a)
         -> ( githubOwned (remediate req al) = githubOwned al
            , verified (remediate req al) = verified al )
flagsKept req (MkAllowlist g v ps) = (Refl, Refl)

||| 5. When nothing is missing the list is unchanged (remediate.sh skips the write).
export
noWriteWhenCovered : DecEq a => (req : List a) -> (al : Allowlist a)
                  -> missing (patterns al) req = []
                  -> patterns (remediate req al) = patterns al
noWriteWhenCovered req (MkAllowlist g v ps) prf =
  rewrite prf in appendNilRightNeutral ps

||| 6a. Converges: after one remediation detect.sh finds nothing missing.
export
converges : DecEq a => (req : List a) -> (al : Allowlist a)
         -> missing (patterns (remediate req al)) req = []
converges req al =
  missingNilIfAllIn (patterns (remediate req al)) req (\r, e => coversRequired req al e)

||| 6b. Idempotent: a second remediation changes nothing.
export
idempotent : DecEq a => (req : List a) -> (al : Allowlist a)
          -> patterns (remediate req (remediate req al)) = patterns (remediate req al)
idempotent req al = noWriteWhenCovered req (remediate req al) (converges req al)

------------------------------------------------------------------------
-- Section 4: The shared corpus (test/fixtures/ci-health/allowlist/)
------------------------------------------------------------------------
-- Patterns are renamed to numbers: the 131 distinct required patterns are
-- 0..130, with hyperpolymath/* as 0, and a repo-specific pattern outside the
-- required set is 1000. Each row reduces to (added, kept, result size). The
-- rows spell their lists out rather than reuse `req`, so each reduces on its
-- own literal.

||| The required set: 131 distinct patterns.
public export
req : List Integer
req = [0 .. 130]

||| (patterns added, patterns kept, size of the result).
public export
counts : List Integer -> (Nat, Nat, Nat)
counts cur = ( length (missing cur req)
             , length cur
             , length (patterns (remediate req (MkAllowlist True True cur))) )

-- corpus: covered added=0 kept=132 result=132
rowCovered : counts ([0 .. 130] ++ [1000]) = (0, 132, 132)
rowCovered = Refl

-- corpus: empty added=131 kept=0 result=131
rowEmpty : counts [] = (131, 0, 131)
rowEmpty = Refl

-- corpus: extras added=130 kept=2 result=132
rowExtras : counts [0, 1000] = (130, 2, 132)
rowExtras = Refl

-- corpus: partial added=13 kept=118 result=131
rowPartial : counts [0 .. 117] = (13, 118, 131)
rowPartial = Refl
