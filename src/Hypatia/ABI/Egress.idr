-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Egress declarations (standards ADR-009) and RE001's decision on them.
--
-- A job that reads a real secret is acceptable to RE001 in exactly two ways:
-- it runs `step-security/harden-runner` and declares nothing, or the repo deed
-- declares it, and the declaration is true of the workflow:
--
--   * it lists exactly the secrets the job reads (no key is computed at run
--     time, which no list can name);
--   * the enforcement it claims holds: `none` claims nothing,
--     `harden-runner-audit` needs a hardener in any mode, and
--     `harden-runner-block` needs one whose `egress-policy` is `block`.
--
-- `Clean job decl` is that acceptance, as a type: a value of it is the
-- evidence. The theorems below are the properties the Elixir rule
-- (lib/rules/research_extensions.ex, re001_missing_harden_runner/1) is built
-- to have; tests/reject/ holds the shapes the type must refuse, and
-- test/egress_decision_mirror_test.exs holds the Elixir rule to this decision
-- on every case of a small domain. Endpoint comparison is outside this model.

module Hypatia.ABI.Egress

-- Re-exported: the decision procedure below is public, and a caller can
-- evaluate it (tests/accept/DeclaredJobIsClean.idr does) only with the
-- membership and string-equality definitions it unfolds to in scope.
import public Data.List.Elem
import public Data.List.Quantifiers
import public Decidable.Equality

%default total

||| A harden-runner step's `egress-policy` (harden-runner's default is audit).
public export
data Policy = Audit | Block

||| The `:enforcement` a declaration claims (egress.abnf).
public export
data Enforcement = None | HardenRunnerAudit | HardenRunnerBlock

||| A job as RE001 reads it: the secrets it reads, whether a key is computed
||| at run time (`secrets[env.X]`, `toJSON(secrets)`), and its hardener.
public export
record Job where
  constructor MkJob
  secrets  : List String
  dynamic  : Bool
  hardener : Maybe Policy

||| A deed declaration for the job.
public export
record Decl where
  constructor MkDecl
  declared    : List String
  enforcement : Enforcement

||| Two lists name the same secrets.
public export
SameSecrets : List String -> List String -> Type
SameSecrets xs ys = (All (\x => Elem x ys) xs, All (\y => Elem y xs) ys)

||| A claimed enforcement holds of the job's hardener.
public export
data Holds : Enforcement -> Maybe Policy -> Type where
  ClaimsNothing : Holds None h
  AnyHardener   : Holds HardenRunnerAudit (Just p)
  Blocking      : Holds HardenRunnerBlock (Just Block)

||| RE001 accepts the job with this declaration (or with none).
public export
data Clean : Job -> Maybe Decl -> Type where
  ||| Hardened, and nothing declared.
  Hardened : Clean (MkJob s d (Just p)) Nothing
  ||| Declared, and the declaration is true.
  Declared : SameSecrets s ds -> Holds e h -> Clean (MkJob s False h) (Just (MkDecl ds e))

-- ─── Decidability: RE001 is a decision procedure ───────────────────────

public export
decElems : (xs, ys : List String) -> Dec (All (\x => Elem x ys) xs)
decElems xs ys = all (\x => isElem x ys) xs

public export
decSame : (xs, ys : List String) -> Dec (SameSecrets xs ys)
decSame xs ys with (decElems xs ys)
  _ | No no = No (\(a, _) => no a)
  _ | Yes a with (decElems ys xs)
    _ | Yes b = Yes (a, b)
    _ | No no = No (\(_, b) => no b)

public export
decHolds : (e : Enforcement) -> (h : Maybe Policy) -> Dec (Holds e h)
decHolds None h = Yes ClaimsNothing
decHolds HardenRunnerAudit (Just p) = Yes AnyHardener
decHolds HardenRunnerAudit Nothing = No (\case ClaimsNothing impossible)
decHolds HardenRunnerBlock (Just Block) = Yes Blocking
decHolds HardenRunnerBlock (Just Audit) = No (\case ClaimsNothing impossible)
decHolds HardenRunnerBlock Nothing = No (\case ClaimsNothing impossible)

||| RE001's verdict on a job and its declaration is decidable.
public export
decClean : (j : Job) -> (d : Maybe Decl) -> Dec (Clean j d)
decClean (MkJob s dyn (Just p)) Nothing = Yes Hardened
decClean (MkJob s dyn Nothing) Nothing = No (\case Hardened impossible)
decClean (MkJob s True h) (Just (MkDecl ds e)) = No (\case Hardened impossible)
decClean (MkJob s False h) (Just (MkDecl ds e)) with (decSame s ds)
  _ | No no = No (\(Declared same _) => no same)
  _ | Yes same with (decHolds e h)
    _ | Yes holds = Yes (Declared same holds)
    _ | No no = No (\(Declared _ holds) => no holds)

-- ─── The properties RE001 is built to have ─────────────────────────────

||| An audit-mode hardener never satisfies a block claim, whatever the
||| secrets (the #975 shape: a hardener that audits must not pass as one
||| that blocks).
export
auditNeverBlock : Clean (MkJob s dyn (Just Audit)) (Just (MkDecl ds HardenRunnerBlock)) -> Void
auditNeverBlock (Declared _ holds) = case holds of {}

||| Every secret the job reads is declared.
export
readSecretDeclared : Clean (MkJob s dyn h) (Just (MkDecl ds e)) -> Elem x s -> Elem x ds
readSecretDeclared (Declared (inDs, _) _) el = indexAll el inDs

||| Every declared secret is one the job reads (a declaration does not
||| over-claim).
export
declaredSecretRead : Clean (MkJob s dyn h) (Just (MkDecl ds e)) -> Elem y ds -> Elem y s
declaredSecretRead (Declared (_, inS) _) el = indexAll el inS

||| A key computed at run time is never covered by a declaration.
export
dynamicNeverDeclared : Clean (MkJob s True h) (Just d) -> Void
dynamicNeverDeclared Hardened impossible
dynamicNeverDeclared (Declared _ _) impossible

||| A secret-bearing job with neither a hardener nor a declaration fails.
export
bareJobFails : Clean (MkJob s dyn Nothing) Nothing -> Void
bareJobFails Hardened impossible

||| A claim of enforcement needs a hardener to hold.
export
claimNeedsHardener : Clean (MkJob s dyn Nothing) (Just (MkDecl ds e)) -> Not (e = None) -> Void
claimNeedsHardener (Declared _ ClaimsNothing) notNone = notNone Refl
