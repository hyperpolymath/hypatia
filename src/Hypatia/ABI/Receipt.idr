-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia ABI — The E0 receipt (E0-10)
--
-- A receipt is the record that says what the single gate did with one learning
-- event and on what evidence.  docs/design/e0-receipt.adoc (section 8) names five
-- properties; this module states and proves each, over the Idris2 types only:
--
--   P1  no two receipts of a ledger share an event_id
--         `appendRefusesDuplicate`, `appendKeepsUnique`, `appendFresh`
--   P2  a receipt cannot claim more than its evidence
--         `verifiedHasEvidence`, `creditedSuccessIsEvidenced`, `verdictIsGate`
--   P3  a revision keeps the history
--         `appendKeepsHistory`, `reviseKeepsHistory`
--   P4  a fill never overwrites
--         `fillNeverOverwrites`, `fillChangesOnlyGaps`, `fillValueIsFromExtra`
--   P5  missing_evidence is exact
--         `missingIff`, `missingEmptyIff`, `receiptMissingExact`
--
-- What this model is NOT (each row of verification/proof-ledger.jsonl says it too):
--   * It does not cover the Elixir run time.  lib/hypatia/receipt.ex is held to it
--     by test/receipt_model_mirror_test.exs for P1, P2 and P5 only.  No run-time
--     code builds a revision, a fill or a withdrawal yet (E0-07), so P3 and P4 have
--     no run-time counterpart to hold.
--   * An event_id, a digest and a field value are `Nat`s: identities with decidable
--     equality.  What the real strings are is outside the model.
--   * It does not prove that `missing_evidence` is sorted or free of duplicates
--     (the Nickel contract of schema/receipt.ncl and the mirror test check that),
--     that a withdrawal is idempotent or independent of order, or anything about
--     confidence (a withdrawal of a failure row raises confidence).
--   * Retraction: `Retraction.idr` retracts by POSITION and the run time retracts by
--     NAME.  This module proves neither.  It has no retraction: a withdrawal is a
--     revision (an appended receipt with verdict `Withdrawn` and a parent), so what
--     is proved here is about the append, by event_id.
--   * The runtime `Receipt.write` does not return an error for a duplicate
--     event_id: it writes nothing and returns :ok (an idempotent replay).  The
--     model's `Left DuplicateEventId` is "the ledger is unchanged"; that is the
--     property, and the return value is a difference.
--
-- The gate table `gate` is the table of `verdict_for` in schema/receipt.ncl, which
-- is the one-row case of OutcomeTracker.qualified_outcome_counts/1.
--
-- Proof-bypass primitives: none (scripts/check-verdicts.sh greps for them).

module Hypatia.ABI.Receipt

import Data.List
import Data.List.Elem
import Data.Maybe

%default total

------------------------------------------------------------------------
-- Section 1: The vocabulary
------------------------------------------------------------------------

||| The `verification` of a row, as OutcomeTracker.qualified_outcome_counts/1 reads it.
public export
data Qualification = QVerified | QFailed | QIncomplete | QStillPresent
                   | QRollbackRequested | QUnqualified

||| What the producer said happened (`assertion.claimed_outcome`).
public export
data Claimed = CSuccess | CFailure | CFalsePositive | CUnknown

||| What the single gate did with the event.
public export
data Verdict = CreditedSuccess | CreditedFailure | CreditedFalsePositive
             | AuditOnly | Withdrawn

||| The gate: one row, one answer (`verdict_for` in schema/receipt.ncl).
||| `Withdrawn` is never an answer of the gate.
public export
gate : Qualification -> Claimed -> Verdict
gate QVerified CSuccess = CreditedSuccess
gate QVerified CFailure = CreditedFailure
gate QFailed CFailure = CreditedFailure
gate QIncomplete CFailure = CreditedFailure
gate QVerified CFalsePositive = CreditedFalsePositive
gate QStillPresent CFalsePositive = CreditedFalsePositive
gate QRollbackRequested CFalsePositive = CreditedFalsePositive
gate _ _ = AuditOnly

||| The closed set of names that `missing_evidence` may hold, in the sorted order
||| of the contract (after_hits, before_hits, event_id, exit_code, head_sha,
||| match_kind, pattern_id, rescan, scan_digest, subject_id, subject_identity,
||| test_digest, tool_version, verdict).
public export
data Field = AfterHits | BeforeHits | EventIdF | ExitCode | HeadSha | MatchKind
           | PatternId | Rescan | ScanDigest | SubjectId | SubjectIdentity
           | TestDigest | ToolVersion | ObsVerdict

public export
allFields : List Field
allFields = [ AfterHits, BeforeHits, EventIdF, ExitCode, HeadSha, MatchKind
            , PatternId, Rescan, ScanDigest, SubjectId, SubjectIdentity
            , TestDigest, ToolVersion, ObsVerdict ]

||| Every field is in the list, so no gap can hide outside it.
public export
fieldListed : (f : Field) -> Elem f Hypatia.ABI.Receipt.allFields
fieldListed AfterHits = Here
fieldListed BeforeHits = There Here
fieldListed EventIdF = There (There Here)
fieldListed ExitCode = There (There (There Here))
fieldListed HeadSha = There (There (There (There Here)))
fieldListed MatchKind = There (There (There (There (There Here))))
fieldListed PatternId = There (There (There (There (There (There Here)))))
fieldListed Rescan = There (There (There (There (There (There (There Here))))))
fieldListed ScanDigest = There (There (There (There (There (There (There (There Here)))))))
fieldListed SubjectId = There (There (There (There (There (There (There (There (There Here))))))))
fieldListed SubjectIdentity = There (There (There (There (There (There (There (There (There (There Here)))))))))
fieldListed TestDigest = There (There (There (There (There (There (There (There (There (There (There Here))))))))))
fieldListed ToolVersion = There (There (There (There (There (There (There (There (There (There (There (There Here)))))))))))
fieldListed ObsVerdict = There (There (There (There (There (There (There (There (There (There (There (There (There Here))))))))))))

||| The values of the fields that may be absent: `Nothing` is a gap.
public export
Body : Type
Body = Field -> Maybe Nat

------------------------------------------------------------------------
-- Section 2: P5 — missing_evidence is exact
------------------------------------------------------------------------

public export
gapIf : Maybe Nat -> Field -> List Field -> List Field
gapIf Nothing f rest = f :: rest
gapIf (Just _) _ rest = rest

public export
missingOf : Body -> List Field -> List Field
missingOf _ [] = []
missingOf b (f :: fs) = gapIf (b f) f (missingOf b fs)

||| The fields of a body that are absent, in the order of `allFields`.
public export
missing : Body -> List Field
missing b = missingOf b allFields

elemGapIfNothing : (m : Maybe Nat) -> m = Nothing -> Elem f (gapIf m f rest)
elemGapIfNothing Nothing Refl = Here
elemGapIfNothing (Just _) Refl impossible

elemGapIfKeeps : (m : Maybe Nat) -> Elem x rest -> Elem x (gapIf m f rest)
elemGapIfKeeps Nothing e = There e
elemGapIfKeeps (Just _) e = e

||| A field that is a gap is named.
namedIfGap : (b : Body) -> (fs : List Field) -> (f : Field) -> b f = Nothing
          -> Elem f fs -> Elem f (missingOf b fs)
namedIfGap b (f :: fs) f eq Here = elemGapIfNothing (b f) eq
namedIfGap b (g :: fs) f eq (There e) = elemGapIfKeeps (b g) (namedIfGap b fs f eq e)

gapIfElem : (m : Maybe Nat) -> Elem x (gapIf m f rest)
         -> Either (x = f, m = Nothing) (Elem x rest)
gapIfElem Nothing Here = Left (Refl, Refl)
gapIfElem Nothing (There e) = Right e
gapIfElem (Just _) e = Right e

||| A field that is named is a gap.
gapIfNamed : (b : Body) -> (fs : List Field) -> (f : Field)
          -> Elem f (missingOf b fs) -> b f = Nothing
gapIfNamed b (g :: fs) f e = case gapIfElem (b g) e of
  Left (prf, eq) => rewrite prf in eq
  Right e' => gapIfNamed b fs f e'

||| P5: a field is named in `missing_evidence` exactly when it is absent.
public export
missingIff : (b : Body) -> (f : Field) -> (Elem f (missing b) -> b f = Nothing, b f = Nothing -> Elem f (missing b))
missingIff b f = (gapIfNamed b allFields f, \eq => namedIfGap b allFields f eq (fieldListed f))

gapIfEmpty : (m : Maybe Nat) -> IsJust m -> rest = [] -> gapIf m f rest = []
gapIfEmpty (Just _) ItIsJust prf = prf

allPresentEmpty : (b : Body) -> ((f : Field) -> IsJust (b f)) -> (fs : List Field) -> missingOf b fs = []
allPresentEmpty b pr [] = Refl
allPresentEmpty b pr (f :: fs) = gapIfEmpty (b f) (pr f) (allPresentEmpty b pr fs)

justOrNothing : (m : Maybe a) -> Either (m = Nothing) (IsJust m)
justOrNothing Nothing = Left Refl
justOrNothing (Just _) = Right ItIsJust

||| P5: the set is empty exactly when every field is present.
public export
missingEmptyIff : (b : Body) -> (missing b = [] -> (f : Field) -> IsJust (b f), ((f : Field) -> IsJust (b f)) -> missing b = [])
missingEmptyIff b = (forward, \pr => allPresentEmpty b pr allFields)
  where
    forward : missing b = [] -> (f : Field) -> IsJust (b f)
    forward emp f = case justOrNothing (b f) of
      Right j => j
      Left eq => absurd (replace {p = Elem f} emp (snd (missingIff b f) eq))

------------------------------------------------------------------------
-- Section 3: Receipts (P2)
------------------------------------------------------------------------

||| A receipt.  It cannot be built unless:
|||  * its verdict is the answer of the gate for its qualification and claimed
|||    outcome, or it is a withdrawal that names a parent (`verdictOk`);
|||  * a `verified` receipt has a row digest and at least one observation
|||    (`verifiedOk`);
|||  * `missingEv` is exactly the gaps of `body` (`missingOk`).
public export
record Receipt where
  constructor MkReceipt
  eventId      : Nat
  parents      : List Nat
  qual         : Qualification
  claimed      : Claimed
  verdict      : Verdict
  rowDigest    : Maybe Nat
  observations : List Nat
  body         : Body
  missingEv    : List Field
  verdictOk    : Either (verdict = Withdrawn, NonEmpty parents) (verdict = gate qual claimed)
  verifiedOk   : qual = QVerified -> (IsJust rowDigest, NonEmpty observations)
  missingOk    : missingEv = missing body

||| Build a first receipt (no parent).  The verdict is the gate's; the gaps are computed.
public export
credit : (eid : Nat) -> (q : Qualification) -> (c : Claimed) -> (digest : Maybe Nat)
      -> (obs : List Nat) -> (b : Body)
      -> (q = QVerified -> (IsJust digest, NonEmpty obs)) -> Receipt
credit eid q c digest obs b ev =
  MkReceipt eid [] q c (gate q c) digest obs b (missing b) (Right Refl) ev Refl

||| The gate never answers `Withdrawn`.
public export
gateNeverWithdrawn : (q : Qualification) -> (c : Claimed) -> Not (gate q c = Withdrawn)
gateNeverWithdrawn QVerified CSuccess Refl impossible
gateNeverWithdrawn QVerified CFailure Refl impossible
gateNeverWithdrawn QVerified CFalsePositive Refl impossible
gateNeverWithdrawn QVerified CUnknown Refl impossible
gateNeverWithdrawn QFailed CSuccess Refl impossible
gateNeverWithdrawn QFailed CFailure Refl impossible
gateNeverWithdrawn QFailed CFalsePositive Refl impossible
gateNeverWithdrawn QFailed CUnknown Refl impossible
gateNeverWithdrawn QIncomplete CSuccess Refl impossible
gateNeverWithdrawn QIncomplete CFailure Refl impossible
gateNeverWithdrawn QIncomplete CFalsePositive Refl impossible
gateNeverWithdrawn QIncomplete CUnknown Refl impossible
gateNeverWithdrawn QStillPresent CSuccess Refl impossible
gateNeverWithdrawn QStillPresent CFailure Refl impossible
gateNeverWithdrawn QStillPresent CFalsePositive Refl impossible
gateNeverWithdrawn QStillPresent CUnknown Refl impossible
gateNeverWithdrawn QRollbackRequested CSuccess Refl impossible
gateNeverWithdrawn QRollbackRequested CFailure Refl impossible
gateNeverWithdrawn QRollbackRequested CFalsePositive Refl impossible
gateNeverWithdrawn QRollbackRequested CUnknown Refl impossible
gateNeverWithdrawn QUnqualified CSuccess Refl impossible
gateNeverWithdrawn QUnqualified CFailure Refl impossible
gateNeverWithdrawn QUnqualified CFalsePositive Refl impossible
gateNeverWithdrawn QUnqualified CUnknown Refl impossible

successNotWithdrawn : Not (CreditedSuccess = Withdrawn)
successNotWithdrawn Refl impossible

||| What a credited success needs: the gate answers it only for a verified, claimed success.
public export
gateSuccessInverse : (q : Qualification) -> (c : Claimed) -> gate q c = CreditedSuccess -> (q = QVerified, c = CSuccess)
gateSuccessInverse QVerified CSuccess _ = (Refl, Refl)
gateSuccessInverse QVerified CFailure Refl impossible
gateSuccessInverse QVerified CFalsePositive Refl impossible
gateSuccessInverse QVerified CUnknown Refl impossible
gateSuccessInverse QFailed CSuccess Refl impossible
gateSuccessInverse QFailed CFailure Refl impossible
gateSuccessInverse QFailed CFalsePositive Refl impossible
gateSuccessInverse QFailed CUnknown Refl impossible
gateSuccessInverse QIncomplete CSuccess Refl impossible
gateSuccessInverse QIncomplete CFailure Refl impossible
gateSuccessInverse QIncomplete CFalsePositive Refl impossible
gateSuccessInverse QIncomplete CUnknown Refl impossible
gateSuccessInverse QStillPresent CSuccess Refl impossible
gateSuccessInverse QStillPresent CFailure Refl impossible
gateSuccessInverse QStillPresent CFalsePositive Refl impossible
gateSuccessInverse QStillPresent CUnknown Refl impossible
gateSuccessInverse QRollbackRequested CSuccess Refl impossible
gateSuccessInverse QRollbackRequested CFailure Refl impossible
gateSuccessInverse QRollbackRequested CFalsePositive Refl impossible
gateSuccessInverse QRollbackRequested CUnknown Refl impossible
gateSuccessInverse QUnqualified CSuccess Refl impossible
gateSuccessInverse QUnqualified CFailure Refl impossible
gateSuccessInverse QUnqualified CFalsePositive Refl impossible
gateSuccessInverse QUnqualified CUnknown Refl impossible

||| P2: a receipt that is not a withdrawal has the verdict of the gate.
public export
verdictIsGate : (r : Receipt) -> Not (r.verdict = Withdrawn) -> r.verdict = gate r.qual r.claimed
verdictIsGate r notW = case r.verdictOk of
  Left (w, _) => absurd (notW w)
  Right g => g

||| P2: a receipt that is `verified` has a row digest and an observation.
public export
verifiedHasEvidence : (r : Receipt) -> r.qual = QVerified -> (IsJust r.rowDigest, NonEmpty r.observations)
verifiedHasEvidence r = r.verifiedOk

||| P2: a verified receipt with no observation does not exist.
public export
noVerifiedWithoutObservations : (r : Receipt) -> r.qual = QVerified -> r.observations = [] -> Void
noVerifiedWithoutObservations r q emp = case (verifiedHasEvidence r q) of
  (_, ne) => case replace {p = \o => NonEmpty o} emp ne of
    _ impossible

||| P2: a receipt that credits a success has a row digest and an observation.
public export
creditedSuccessIsEvidenced : (r : Receipt) -> r.verdict = CreditedSuccess
                          -> (IsJust r.rowDigest, NonEmpty r.observations)
creditedSuccessIsEvidenced r v = case r.verdictOk of
  Left (w, _) => absurd (successNotWithdrawn (trans (sym v) w))
  Right g => case gateSuccessInverse r.qual r.claimed (trans (sym g) v) of
    (q, _) => verifiedHasEvidence r q

||| A withdrawal names a parent.
public export
withdrawnHasParent : (r : Receipt) -> r.verdict = Withdrawn -> NonEmpty r.parents
withdrawnHasParent r w = case r.verdictOk of
  Left (_, ne) => ne
  Right g => absurd (gateNeverWithdrawn r.qual r.claimed (trans (sym g) w))

||| P5 on a receipt: a field is named in its `missing_evidence` exactly when it is absent.
public export
receiptMissingExact : (r : Receipt) -> (f : Field)
  -> (Elem f r.missingEv -> r.body f = Nothing, r.body f = Nothing -> Elem f r.missingEv)
receiptMissingExact r f =
  ( \e => fst (missingIff r.body f) (replace {p = Elem f} r.missingOk e)
  , \eq => replace {p = Elem f} (sym r.missingOk) (snd (missingIff r.body f) eq) )

------------------------------------------------------------------------
-- Section 4: P1 and P3 — the ledger
------------------------------------------------------------------------

public export
ids : List Receipt -> List Nat
ids = map eventId

public export
predNat : Nat -> Nat
predNat Z = Z
predNat (S k) = k

public export
zeroNotSucc : Not (Z = S k)
zeroNotSucc Refl impossible

public export
succNotZero : Not (S k = Z)
succNotZero Refl impossible

||| Equality of identities, decided structurally.
public export
decNat : (a, b : Nat) -> Dec (a = b)
decNat Z Z = Yes Refl
decNat Z (S _) = No zeroNotSucc
decNat (S _) Z = No succNotZero
decNat (S a) (S b) = case decNat a b of
  Yes Refl => Yes Refl
  No c => No (\e => c (cong predNat e))

||| Membership of an identity, decided.
public export
decElem : (n : Nat) -> (ns : List Nat) -> Dec (Elem n ns)
decElem n [] = No (\e => case e of _ impossible)
decElem n (m :: ms) = case decNat n m of
  Yes Refl => Yes Here
  No c => case decElem n ms of
    Yes e => Yes (There e)
    No c2 => No (\e => case e of
      Here => c Refl
      There e2 => c2 e2)

||| The event_ids of a list are pairwise distinct.
public export
data Unique : List Nat -> Type where
  UNil  : Unique []
  UCons : Not (Elem x xs) -> Unique xs -> Unique (x :: xs)

public export
data Refusal = DuplicateEventId

public export
appendWith : (r : Receipt) -> (l : List Receipt) -> Dec (Elem (eventId r) (ids l))
          -> Either Refusal (List Receipt)
appendWith _ _ (Yes _) = Left DuplicateEventId
appendWith r l (No _) = Right (r :: l)

||| Append a receipt.  A ledger that already holds the receipt's event_id is not changed.
public export
append : (r : Receipt) -> (l : List Receipt) -> Either Refusal (List Receipt)
append r l = appendWith r l (decElem (eventId r) (ids l))

||| P1: appending to a ledger that holds the event_id is refused.
public export
appendRefusesDuplicate : (r : Receipt) -> (l : List Receipt) -> Elem (eventId r) (ids l)
                      -> append r l = Left DuplicateEventId
appendRefusesDuplicate r l e = go (decElem (eventId r) (ids l))
  where
    go : (d : Dec (Elem (eventId r) (ids l))) -> appendWith r l d = Left DuplicateEventId
    go (Yes _) = Refl
    go (No c) = absurd (c e)

||| P1: appending to a ledger that does not hold it succeeds.
public export
appendFresh : (r : Receipt) -> (l : List Receipt) -> Not (Elem (eventId r) (ids l))
           -> append r l = Right (r :: l)
appendFresh r l n = go (decElem (eventId r) (ids l))
  where
    go : (d : Dec (Elem (eventId r) (ids l))) -> appendWith r l d = Right (r :: l)
    go (Yes e) = absurd (n e)
    go (No _) = Refl

||| P1: a ledger of distinct event_ids stays one.
public export
appendKeepsUnique : (r : Receipt) -> (l : List Receipt) -> Unique (ids l)
                 -> append r l = Right l' -> Unique (ids l')
appendKeepsUnique r l u prf = go (decElem (eventId r) (ids l)) prf
  where
    go : {0 m : List Receipt} -> (d : Dec (Elem (eventId r) (ids l))) -> appendWith r l d = Right m -> Unique (ids m)
    go (Yes _) Refl impossible
    go (No c) Refl = UCons c u

||| P3: an append keeps every receipt of the ledger before it.
public export
appendKeepsHistory : (r : Receipt) -> (l : List Receipt) -> append r l = Right l'
                  -> Elem x l -> Elem x l'
appendKeepsHistory r l prf e = go (decElem (eventId r) (ids l)) prf
  where
    go : {0 m : List Receipt} -> (d : Dec (Elem (eventId r) (ids l))) -> appendWith r l d = Right m -> Elem x m
    go (Yes _) Refl impossible
    go (No _) Refl = There e

------------------------------------------------------------------------
-- Section 5: P4 — a fill never overwrites
------------------------------------------------------------------------

||| What a gap-filling revision holds for a field: the old value if there is one,
||| else the new one.
public export
fillBody : (old : Body) -> (extra : Body) -> Body
fillBody old extra f = case old f of
  Just v => Just v
  Nothing => extra f

||| P4: a present field is never overwritten.
public export
fillNeverOverwrites : (old, extra : Body) -> (f : Field) -> old f = Just v
                   -> fillBody old extra f = Just v
fillNeverOverwrites old extra f eq = rewrite eq in Refl

justInj : Just a = Just b -> a = b
justInj Refl = Refl

||| P4: a value that the revision holds is the old one or one that came from `extra`
||| into a gap.
public export
fillValueIsFromExtra : (old, extra : Body) -> (f : Field) -> fillBody old extra f = Just v
                    -> Either (old f = Just v) (old f = Nothing, extra f = Just v)
fillValueIsFromExtra old extra f prf with (old f) proof eq
  fillValueIsFromExtra old extra f prf | Just w = Left (cong Just (justInj prf))
  fillValueIsFromExtra old extra f prf | Nothing = Right (Refl, prf)

||| P4: a field whose value a fill changes was in `missing_evidence`.
public export
fillChangesOnlyGaps : (old, extra : Body) -> (f : Field)
                   -> Not (fillBody old extra f = old f) -> Elem f (missing old)
fillChangesOnlyGaps old extra f changed = case justOrNothing (old f) of
  Left eq => snd (missingIff old f) eq
  Right j => absurd (changed (keeps j))
  where
    keeps : IsJust (old f) -> fillBody old extra f = old f
    keeps j with (old f) proof eq
      keeps ItIsJust | Just _ = Refl

||| A revision that fills gaps: a new receipt with its own event_id whose parent is
||| the receipt it revises.  It holds the same assertion, verdict and evidence.
public export
reviseFill : (old : Receipt) -> (newId : Nat) -> (extra : Body) -> Receipt
reviseFill old newId extra =
  MkReceipt newId [eventId old] (qual old) (claimed old) (verdict old)
            (rowDigest old) (observations old) (fillBody (body old) extra)
            (missing (fillBody (body old) extra))
            (reparent (verdictOk old)) (verifiedOk old) Refl
  where
    reparent : Either (verdict old = Withdrawn, NonEmpty (parents old)) (verdict old = gate (qual old) (claimed old))
            -> Either (verdict old = Withdrawn, NonEmpty [eventId old]) (verdict old = gate (qual old) (claimed old))
    reparent (Left (w, _)) = Left (w, IsNonEmpty)
    reparent (Right g) = Right g

||| P3: a revision appended to a ledger that holds the receipt it revises keeps
||| every receipt of the ledger.
public export
reviseKeepsHistory : (old : Receipt) -> (newId : Nat) -> (extra : Body) -> (l : List Receipt)
                  -> append (reviseFill old newId extra) l = Right l' -> Elem x l -> Elem x l'
reviseKeepsHistory old newId extra l = appendKeepsHistory (reviseFill old newId extra) l

||| P4 on the revision: it never overwrites a present field of the receipt.
public export
reviseNeverOverwrites : (old : Receipt) -> (newId : Nat) -> (extra : Body) -> (f : Field)
                     -> old.body f = Just v -> (reviseFill old newId extra).body f = Just v
reviseNeverOverwrites old newId extra f = fillNeverOverwrites old.body extra f

||| The revision names the receipt it revises, and keeps what the receipt claimed.
public export
reviseKeepsClaim : (old : Receipt) -> (newId : Nat) -> (extra : Body)
                -> ( (reviseFill old newId extra).parents = [old.eventId]
                   , (reviseFill old newId extra).qual = old.qual
                   , (reviseFill old newId extra).claimed = old.claimed
                   , (reviseFill old newId extra).verdict = old.verdict )
reviseKeepsClaim old newId extra = (Refl, Refl, Refl, Refl)
