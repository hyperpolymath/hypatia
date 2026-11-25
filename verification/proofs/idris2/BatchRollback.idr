-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- BatchRollback.idr — a batch either commits every step or leaves the
-- state exactly as it found it.
--
-- The model. A batch is a list of steps over some state. A step either
-- fails (Nothing) or succeeds with the new state and the undo for that
-- change. The executor runs the steps in order and keeps a journal of the
-- undos of the steps that succeeded. When a step fails, it unwinds the
-- journal, newest first, and reports RolledBack.
--
-- The theorem (batchAllOrNothing). If every step's undo inverts its own
-- change (Reversible), then executing a batch from s0 ends in exactly one
-- of two ways:
--   * Committed c, where c is the result of applying every step in order;
--   * RolledBack s0: some step failed and the state is s0 again.
-- Nothing in between is reachable.
--
-- An instance (Section 5): a snapshot of repositories, where a step
-- overwrites one repository's content and its undo writes the old content
-- back. Every such step is Reversible (setStepReversible), so a batch of
-- them is all-or-nothing with no assumption left over.
--
-- Trust boundary. lib/safety/batch_rollback.ex does not run steps itself:
-- it marks a batch's outcomes and emits one `git revert` instruction per
-- outcome. This file proves the journal discipline, not that the Elixir
-- module follows it, and not that `git revert` inverts a change; for the
-- runtime, that is the Reversible premise and it is ASSUMED.

module BatchRollback

import Data.List
import Data.List.Quantifiers

%default total

------------------------------------------------------------------------
-- Section 1: steps, reversibility, and sequential application
------------------------------------------------------------------------

||| One step of a batch: fail, or succeed with the new state and the undo
||| for exactly this change.
public export
record Step (s : Type) where
  constructor MkStep
  apply : s -> Maybe (s, s -> s)

||| The undo a step returns takes its result back to its input.
public export
Reversible : {s : Type} -> Step s -> Type
Reversible st = (x, y : s) -> (u : s -> s) -> st.apply x = Just (y, u) -> u y = x

mutual
  ||| Apply every step in order; Nothing as soon as one fails.
  public export
  applyAll : List (Step s) -> s -> Maybe s
  applyAll [] x = Just x
  applyAll (st :: rest) x = applyNext rest (st.apply x)

  ||| Continue after one step's result.
  public export
  applyNext : List (Step s) -> Maybe (s, s -> s) -> Maybe s
  applyNext _ Nothing = Nothing
  applyNext rest (Just (y, _)) = applyAll rest y

------------------------------------------------------------------------
-- Section 2: the executor with an undo journal
------------------------------------------------------------------------

||| How a batch ends.
public export
data Outcome s = Committed s | RolledBack s

||| Run the journal's undos, newest first.
public export
unwind : List (s -> s) -> s -> s
unwind [] cur = cur
unwind (u :: us) cur = unwind us (u cur)

mutual
  ||| Run the remaining steps from `cur`, with the undos taken so far.
  public export
  run : List (Step s) -> (cur : s) -> (journal : List (s -> s)) -> Outcome s
  run [] cur _ = Committed cur
  run (st :: rest) cur journal = runNext rest cur journal (st.apply cur)

  ||| Continue after one step's result: unwind on failure, journal its
  ||| undo on success.
  public export
  runNext : List (Step s) -> (cur : s) -> (journal : List (s -> s)) ->
            Maybe (s, s -> s) -> Outcome s
  runNext _ cur journal Nothing = RolledBack (unwind journal cur)
  runNext rest _ journal (Just (y, u)) = run rest y (u :: journal)

||| Execute a batch from a starting state with an empty journal.
public export
execute : List (Step s) -> s -> Outcome s
execute steps s0 = run steps s0 []

------------------------------------------------------------------------
-- Section 3: the journal invariant
------------------------------------------------------------------------

||| The executor's two possible ends, from any point in the batch: it
||| commits the sequential result from `cur`, or a step fails and it
||| reports s0.
public export
data RunEnd : {s : Type} -> (s0, cur : s) -> List (Step s) -> Outcome s -> Type where
  EndCommitted : (c : s) -> applyAll steps cur = Just c ->
                 RunEnd s0 cur steps (Committed c)
  EndRolledBack : applyAll steps cur = Nothing ->
                  RunEnd s0 cur steps (RolledBack s0)

||| One successful step: an end of the run after it is an end of the run
||| before it.
extendEnd : {s : Type} -> {s0, cur, y : s} -> {st : Step s} -> {rest : List (Step s)} ->
            {o : Outcome s} -> applyAll (st :: rest) cur = applyAll rest y ->
            RunEnd s0 y rest o -> RunEnd s0 cur (st :: rest) o
extendEnd e (EndCommitted c prf) = EndCommitted c (trans e prf)
extendEnd e (EndRolledBack prf) = EndRolledBack (trans e prf)

||| If the journal unwinds `cur` to s0, the run from `cur` ends in one of
||| the two ways above.
export
runEnds : {s : Type} -> (s0 : s) -> (steps : List (Step s)) -> All Reversible steps ->
          (cur : s) -> (journal : List (s -> s)) -> unwind journal cur = s0 ->
          RunEnd s0 cur steps (run steps cur journal)
runEnds s0 [] [] cur journal inv = EndCommitted cur Refl
runEnds s0 (st :: rest) (rev :: revs) cur journal inv with (st.apply cur) proof eq
  _ | Nothing = rewrite inv in EndRolledBack (rewrite eq in Refl)
  _ | Just (y, u) =
        extendEnd (rewrite eq in Refl)
          (runEnds s0 rest revs y (u :: journal)
             (trans (cong (unwind journal) (rev cur y u eq)) inv))

------------------------------------------------------------------------
-- Section 4: all or nothing
------------------------------------------------------------------------

||| Read a RunEnd as the two-way answer.
fromEnd : {s : Type} -> {s0, cur : s} -> {steps : List (Step s)} -> {o : Outcome s} ->
          RunEnd s0 cur steps o ->
          Either (c : s ** (o = Committed c, applyAll steps cur = Just c))
                 (o = RolledBack s0, applyAll steps cur = Nothing)
fromEnd (EndCommitted c prf) = Left (c ** (Refl, prf))
fromEnd (EndRolledBack prf) = Right (Refl, prf)

||| A batch of reversible steps either commits every step, and its result
||| is the sequential application of all of them, or it fails somewhere
||| and the state is exactly the starting state.
export
batchAllOrNothing : {s : Type} -> (steps : List (Step s)) -> All Reversible steps ->
                    (s0 : s) ->
                    Either (c : s ** (execute steps s0 = Committed c, applyAll steps s0 = Just c))
                           (execute steps s0 = RolledBack s0, applyAll steps s0 = Nothing)
batchAllOrNothing steps revs s0 = fromEnd (runEnds s0 steps revs s0 [] Refl)

committedNotRolledBack : Committed a = RolledBack b -> Void
committedNotRolledBack Refl impossible

nothingNotJust : {0 a : Type} -> {0 x : a} -> Nothing = Just x -> Void
nothingNotJust Refl impossible

injRolledBack : RolledBack a = RolledBack b -> a = b
injRolledBack Refl = Refl

injCommitted : Committed a = Committed b -> a = b
injCommitted Refl = Refl

injJust : Just a = Just b -> a = b
injJust Refl = Refl

||| Rollback restores the original: whenever a batch rolls back, the state
||| it reports is the starting state.
export
rollbackRestoresOriginal : {s : Type} -> (steps : List (Step s)) -> All Reversible steps ->
                           (s0, r : s) -> execute steps s0 = RolledBack r -> r = s0
rollbackRestoresOriginal steps revs s0 r eq with (batchAllOrNothing steps revs s0)
  _ | Left (c ** (committed, _)) = absurd (committedNotRolledBack (trans (sym committed) eq))
  _ | Right (rolled, _) = sym (injRolledBack (trans (sym rolled) eq))

||| A batch commits exactly when every step succeeds, and it commits the
||| sequential result.
export
commitsIffAllSucceed : {s : Type} -> (steps : List (Step s)) -> All Reversible steps ->
                       (s0, c : s) -> applyAll steps s0 = Just c ->
                       execute steps s0 = Committed c
commitsIffAllSucceed steps revs s0 c ok with (batchAllOrNothing steps revs s0)
  _ | Left (c' ** (committed, prf)) =
        trans committed (cong Committed (injJust (trans (sym prf) ok)))
  _ | Right (_, failed) = absurd (nothingNotJust (trans (sym failed) ok))

||| A batch rolls back exactly when some step fails.
export
rollsBackIffSomeStepFails : {s : Type} -> (steps : List (Step s)) -> All Reversible steps ->
                            (s0 : s) -> applyAll steps s0 = Nothing ->
                            execute steps s0 = RolledBack s0
rollsBackIffSomeStepFails steps revs s0 failed with (batchAllOrNothing steps revs s0)
  _ | Left (c ** (_, prf)) = absurd (nothingNotJust (trans (sym failed) prf))
  _ | Right (rolled, _) = rolled

------------------------------------------------------------------------
-- Section 5: an instance — overwriting repository contents
------------------------------------------------------------------------

||| A repository's name and content (a hash, say).
public export
record RepoState where
  constructor MkRepoState
  name : String
  content : String

||| A snapshot of several repositories.
public export
Snapshot : Type
Snapshot = List RepoState

||| The same repository with new content.
public export
withContent : String -> RepoState -> RepoState
withContent c r = MkRepoState r.name c

||| The repository at a position, if there is one.
public export
lookupAt : Nat -> List a -> Maybe a
lookupAt _ [] = Nothing
lookupAt Z (x :: _) = Just x
lookupAt (S k) (_ :: xs) = lookupAt k xs

||| Replace the element at a position; a list too short is left as it is.
public export
setAt : Nat -> a -> List a -> List a
setAt _ _ [] = []
setAt Z v (_ :: xs) = v :: xs
setAt (S k) v (x :: xs) = x :: setAt k v xs

||| Writing back the old element undoes a write at the same position.
export
setAtRestores : (i : Nat) -> (old, new : a) -> (xs : List a) ->
                lookupAt i xs = Just old -> setAt i old (setAt i new xs) = xs
setAtRestores _ _ _ [] prf = absurd (nothingNotJust prf)
setAtRestores Z old new (x :: xs) prf = cong (:: xs) (sym (injJust prf))
setAtRestores (S k) old new (x :: xs) prf = cong (x ::) (setAtRestores k old new xs prf)

||| The step body: no repository at i fails; otherwise write the new
||| content and keep the old repository for the undo.
public export
setNext : (i : Nat) -> (c : String) -> Snapshot -> Maybe RepoState ->
          Maybe (Snapshot, Snapshot -> Snapshot)
setNext _ _ _ Nothing = Nothing
setNext i c snap (Just old) = Just (setAt i (withContent c old) snap, setAt i old)

||| Set the content of the repository at position i.
public export
setContent : (i : Nat) -> (c : String) -> Step Snapshot
setContent i c = MkStep (\snap => setNext i c snap (lookupAt i snap))

setNextReversible : (i : Nat) -> (c : String) -> (snap, y : Snapshot) ->
                    (u : Snapshot -> Snapshot) -> (m : Maybe RepoState) ->
                    lookupAt i snap = m -> setNext i c snap m = Just (y, u) -> u y = snap
setNextReversible i c snap y u Nothing _ eq = absurd (nothingNotJust eq)
setNextReversible i c snap _ _ (Just old) found Refl =
  setAtRestores i old (withContent c old) snap found

||| Every content overwrite is reversible.
export
setStepReversible : (i : Nat) -> (c : String) -> Reversible (setContent i c)
setStepReversible i c snap y u eq = setNextReversible i c snap y u (lookupAt i snap) Refl eq

||| Every batch of content overwrites is reversible, step by step.
export
setStepsReversible : (ws : List (Nat, String)) ->
                     All Reversible (map (\w => setContent (fst w) (snd w)) ws)
setStepsReversible [] = []
setStepsReversible ((i, c) :: ws) = setStepReversible i c :: setStepsReversible ws

||| A batch of repository content overwrites is all-or-nothing, with no
||| premise left: it commits every write, or it leaves the snapshot as it
||| was.
export
repoBatchAllOrNothing : (ws : List (Nat, String)) -> (s0 : Snapshot) ->
  Either (c : Snapshot ** ( execute (map (\w => setContent (fst w) (snd w)) ws) s0 = Committed c
                          , applyAll (map (\w => setContent (fst w) (snd w)) ws) s0 = Just c))
         ( execute (map (\w => setContent (fst w) (snd w)) ws) s0 = RolledBack s0
         , applyAll (map (\w => setContent (fst w) (snd w)) ws) s0 = Nothing)
repoBatchAllOrNothing ws s0 =
  batchAllOrNothing (map (\w => setContent (fst w) (snd w)) ws) (setStepsReversible ws) s0
