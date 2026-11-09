-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia — actions.lock closure model (standards#963, criterion 5)
--
-- An actions.lock is CLOSED when every ref it names, under `workflows:` or in
-- a record's nested `uses:`, has a top-level record of its own. This module
-- states that property and proves a decision procedure for it:
--
--   1. `danglingNilOfAll` / `allOfDanglingNil`: the dangling list is empty
--      exactly when the lock is closed (`closedIffNoDangling`).
--   2. `danglingSound`: every ref the dangling list reports is named by the
--      lock and has no record.
--   3. `danglingComplete`: every named ref without a record is reported.
--   4. `decClosed`: closure is decidable, by the dangling list.
--   5. The five rows of the shared closure corpus are evaluated here: the
--      verdict and the exact dangling refs of each row are equations the type
--      checker reduces, so this file fails to check if the model and
--      test/fixtures/actions_lock/closure/expected.tsv ever disagree.
--
-- Corresponds to: lib/rules/actions_lock.ex (validate_workflow_dependencies,
-- validate_transitive_dependencies) and, in hyperpolymath/standards,
-- scripts/close-lock.sh --check. Both are tested against the same corpus.
--
-- What this does NOT prove (recorded in verification/PROOF-STATUS.adoc):
--   * Runtime refinement. The Elixir and Bash implementations agree with this
--     model on the five corpus rows only (TESTED), not in general.
--   * The parse boundary. Refs are abstract values with decidable equality.
--     Folding owner/repo to lowercase while keeping the ref exact
--     (ActionsLock.normalise_ref/1) happens before this model, so the corpus
--     rows below are transcribed with the fold already applied (ASSUMED).

module ActionsLockClosure

import Data.List
import Data.List.Elem
import Data.List.Quantifiers
import Decidable.Equality

%default total

------------------------------------------------------------------------
-- Section 1: The lock and its closure predicate
------------------------------------------------------------------------

||| A lock after parsing: the refs listed under `workflows:` (all workflows
||| flattened together) and the top-level records, each a key paired with
||| the refs in its nested `uses:`.
public export
record Lock a where
  constructor MkLock
  workflowRefs : List a
  records      : List (a, List a)

||| The refs that have a top-level record.
public export
keys : Lock a -> List a
keys l = map fst (records l)

||| Every ref the lock names: the workflow refs, then every nested `uses:`.
public export
edges : Lock a -> List a
edges l = workflowRefs l ++ concatMap snd (records l)

||| A lock is closed when every ref it names has a record.
public export
Closed : Lock a -> Type
Closed l = All (\r => Elem r (keys l)) (edges l)

------------------------------------------------------------------------
-- Section 2: The dangling list
------------------------------------------------------------------------

||| The refs in `rs` that are not in `ks`, in order. This is what
||| `close-lock.sh --check` prints and what Hypatia's error payload carries.
public export
dangling : DecEq a => List a -> List a -> List a
dangling ks [] = []
dangling ks (r :: rs) with (isElem r ks)
  _ | Yes _ = dangling ks rs
  _ | No _  = r :: dangling ks rs

||| If every ref has a record, nothing dangles.
export
danglingNilOfAll : DecEq a => (ks, rs : List a) ->
                   All (\r => Elem r ks) rs -> dangling ks rs = []
danglingNilOfAll ks [] [] = Refl
danglingNilOfAll ks (r :: rs) (p :: ps) with (isElem r ks)
  _ | Yes _ = danglingNilOfAll ks rs ps
  _ | No np = void (np p)

||| If nothing dangles, every ref has a record.
export
allOfDanglingNil : DecEq a => (ks, rs : List a) ->
                   dangling ks rs = [] -> All (\r => Elem r ks) rs
allOfDanglingNil ks [] _ = []
allOfDanglingNil ks (r :: rs) prf with (isElem r ks)
  _ | Yes p = p :: allOfDanglingNil ks rs prf
  _ | No _  = absurd prf

||| Every reported ref is named and has no record.
export
danglingSound : DecEq a => (ks, rs : List a) ->
                Elem d (dangling ks rs) -> (Elem d rs, Not (Elem d ks))
danglingSound ks [] e = absurd e
danglingSound ks (r :: rs) e with (isElem r ks)
  danglingSound ks (r :: rs) e | Yes _ =
    let (inRs, notKs) = danglingSound ks rs e in (There inRs, notKs)
  danglingSound ks (r :: rs) Here | No np = (Here, np)
  danglingSound ks (r :: rs) (There e') | No np =
    let (inRs, notKs) = danglingSound ks rs e' in (There inRs, notKs)

||| Every named ref without a record is reported.
export
danglingComplete : DecEq a => (ks, rs : List a) ->
                   Elem d rs -> Not (Elem d ks) -> Elem d (dangling ks rs)
danglingComplete ks [] e _ = absurd e
danglingComplete ks (r :: rs) Here nk with (isElem r ks)
  _ | Yes p = void (nk p)
  _ | No _  = Here
danglingComplete ks (r :: rs) (There e) nk with (isElem r ks)
  _ | Yes _ = danglingComplete ks rs e nk
  _ | No _  = There (danglingComplete ks rs e nk)

------------------------------------------------------------------------
-- Section 3: Closure is decided by the dangling list
------------------------------------------------------------------------

||| The dangling list of a lock: its edges without a record.
public export
danglingOf : DecEq a => Lock a -> List a
danglingOf l = dangling (keys l) (edges l)

||| A lock is closed if and only if its dangling list is empty.
export
closedIffNoDangling : DecEq a => (l : Lock a) ->
                      (Closed l -> danglingOf l = [], danglingOf l = [] -> Closed l)
closedIffNoDangling l =
  (danglingNilOfAll (keys l) (edges l), allOfDanglingNil (keys l) (edges l))

||| Closure is decidable.
export
decClosed : DecEq a => (l : Lock a) -> Dec (Closed l)
decClosed l = case decEq (danglingOf l) [] of
  Yes nil    => Yes (allOfDanglingNil (keys l) (edges l) nil)
  No notNil  => No (\c => notNil (danglingNilOfAll (keys l) (edges l) c))

------------------------------------------------------------------------
-- Section 4: The shared closure corpus, row by row
------------------------------------------------------------------------
--
-- Each distinct ref of test/fixtures/actions_lock/closure/ is numbered, after
-- the owner/repo fold (ASSUMED, see the header):
--
--   1  example/build@1111111111111111111111111111111111111111
--   2  example/setup@2222222222222222222222222222222222222222
--   3  example/push-signed@3333333333333333333333333333333333333333
--   4  example/setup-python@v2
--   5  example/generator@4444444444444444444444444444444444444444
--   6  example/rust-cache@5555555555555555555555555555555555555555
--      (closed-case-variant.lock spells the workflow ref Example/Rust-Cache;
--      the fold maps it to the record's key)
--
-- A record's ref:, commit:, owner_id:, repo_id: and hostname: fields carry no
-- edges, so they are not modelled. An empty workflow (`empty.yml: []`)
-- contributes no edge.

||| closed-v0.0.2.lock and closed-v0.0.3.lock (the same edges; v0.0.3 adds a
||| hostname, which carries no edge).
public export
closedV00x : Lock Nat
closedV00x = MkLock [1] [(1, [2]), (2, [])]

||| closed-case-variant.lock, after the fold.
public export
closedCaseVariant : Lock Nat
closedCaseVariant = MkLock [6] [(6, [])]

||| dangling-nested.lock: the standards#963 shape.
public export
danglingNested : Lock Nat
danglingNested = MkLock [3] [(3, [4])]

||| dangling-workflow-ref.lock.
public export
danglingWorkflowRef : Lock Nat
danglingWorkflowRef = MkLock [1, 5] [(1, [])]

||| expected.tsv rows `closed-v0.0.2.lock closed -` and
||| `closed-v0.0.3.lock closed -`.
export
rowClosedV00x : danglingOf ActionsLockClosure.closedV00x = []
rowClosedV00x = Refl

||| expected.tsv row `closed-case-variant.lock closed -`.
export
rowClosedCaseVariant : danglingOf ActionsLockClosure.closedCaseVariant = []
rowClosedCaseVariant = Refl

||| expected.tsv row `dangling-nested.lock dangling example/setup-python@v2`.
export
rowDanglingNested : danglingOf ActionsLockClosure.danglingNested = [4]
rowDanglingNested = Refl

||| expected.tsv row
||| `dangling-workflow-ref.lock dangling example/generator@4444…`.
export
rowDanglingWorkflowRef : danglingOf ActionsLockClosure.danglingWorkflowRef = [5]
rowDanglingWorkflowRef = Refl

||| The closed rows are closed, as proofs of the predicate itself.
export
closedRowsAreClosed : (Closed ActionsLockClosure.closedV00x,
                       Closed ActionsLockClosure.closedCaseVariant)
closedRowsAreClosed =
  ( allOfDanglingNil (keys closedV00x) (edges closedV00x) rowClosedV00x
  , allOfDanglingNil (keys closedCaseVariant) (edges closedCaseVariant)
                     rowClosedCaseVariant )

||| The dangling rows are not closed.
export
danglingRowsAreNotClosed : (Not (Closed ActionsLockClosure.danglingNested),
                            Not (Closed ActionsLockClosure.danglingWorkflowRef))
danglingRowsAreNotClosed =
  ( \c => absurd (trans (sym rowDanglingNested)
                        (danglingNilOfAll (keys danglingNested)
                                          (edges danglingNested) c))
  , \c => absurd (trans (sym rowDanglingWorkflowRef)
                        (danglingNilOfAll (keys danglingWorkflowRef)
                                          (edges danglingWorkflowRef) c)) )
