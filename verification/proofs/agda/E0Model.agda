{-# OPTIONS --safe --without-K --cubical-compatible --double-check #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Finite model of the E0 qualification gate.  Builtins only.
module E0Model where

open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Equality

------------------------------------------------------------------------
-- Prelude

data ⊥ : Set where

¬_ : Set → Set
¬ A = A → ⊥

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixr 4 _,_
infixr 2 _×_

cong : ∀ {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

sym : ∀ {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl q = q

if_then_else_ : ∀ {A : Set} → Bool → A → A → A
if true  then x else _ = x
if false then _ else y = y

record ⊤ : Set where
  constructor tt

_++_ : ∀ {A : Set} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

length : ∀ {A : Set} → List A → Nat
length [] = 0
length (_ ∷ xs) = suc (length xs)

+-identityʳ : ∀ n → n + 0 ≡ n
+-identityʳ zero = refl
+-identityʳ (suc n) = cong suc (+-identityʳ n)

+-assoc : ∀ a b c → (a + b) + c ≡ a + (b + c)
+-assoc zero b c = refl
+-assoc (suc a) b c = cong suc (+-assoc a b c)

+-suc : ∀ a b → a + suc b ≡ suc (a + b)
+-suc zero b = refl
+-suc (suc a) b = cong suc (+-suc a b)

data _≤_ : Nat → Nat → Set where
  z≤n : ∀ {n} → zero ≤ n
  s≤s : ∀ {m n} → m ≤ n → suc m ≤ suc n

-- n < k is false exactly when k ≤ n.
not-lt⇒le : ∀ n k → (n < k) ≡ false → k ≤ n
not-lt⇒le n zero p = z≤n
not-lt⇒le zero (suc k) ()
not-lt⇒le (suc n) (suc k) p = s≤s (not-lt⇒le n k p)

------------------------------------------------------------------------
-- The row, as the gate reads it.  `batchRollback` is the conjunction of the
-- two conditions of the batch-rollback clause (caused_by is "batch_rollback"
-- and batch_id is a non-empty string).

data Outcome : Set where
  success failure falsePositive otherOutcome : Outcome

data Verification : Set where
  verified failed incomplete stillPresent rollbackRequested : Verification
  unverified noOp unqualified scanFailed scanSkipped : Verification
  otherVerification missingVerification : Verification

record Row : Set where
  constructor row
  field
    outcome       : Outcome
    verification  : Verification
    batchRollback : Bool

open Row public

data Bucket : Set where
  creditSuccess creditFailure creditFalsePositive ignored : Bucket

-- A failure row counts with these verifications.
failureEvidence : Verification → Bool
failureEvidence failed     = true
failureEvidence incomplete = true
failureEvidence verified   = true
failureEvidence _          = false

-- A false-positive row counts with these verifications (the batch-rollback
-- clause is separate).
falsePositiveEvidence : Verification → Bool
falsePositiveEvidence stillPresent = true
falsePositiveEvidence verified     = true
falsePositiveEvidence _            = false

-- OutcomeTracker.qualified_outcome_counts/1, one row.
gate : Outcome → Verification → Bool → Bucket
gate success verified _ = creditSuccess
gate success _ _ = ignored
gate failure v _ = if failureEvidence v then creditFailure else ignored
gate falsePositive rollbackRequested true = creditFalsePositive
gate falsePositive v _ = if falsePositiveEvidence v then creditFalsePositive else ignored
gate otherOutcome _ _ = ignored

classify : Row → Bucket
classify (row o v b) = gate o v b

------------------------------------------------------------------------
-- T4. Positive credit needs the outcome success AND the verification verified.

creditSuccess≢ignored : creditSuccess ≡ ignored → ⊥
creditSuccess≢ignored ()

gate-success-sound : ∀ o v b → gate o v b ≡ creditSuccess → (o ≡ success) × (v ≡ verified)
gate-success-sound success verified b p = refl , refl
gate-success-sound success failed b ()
gate-success-sound success incomplete b ()
gate-success-sound success stillPresent b ()
gate-success-sound success rollbackRequested b ()
gate-success-sound success unverified b ()
gate-success-sound success noOp b ()
gate-success-sound success unqualified b ()
gate-success-sound success scanFailed b ()
gate-success-sound success scanSkipped b ()
gate-success-sound success otherVerification b ()
gate-success-sound success missingVerification b ()
gate-success-sound failure verified b ()
gate-success-sound failure failed b ()
gate-success-sound failure incomplete b ()
gate-success-sound failure stillPresent b ()
gate-success-sound failure rollbackRequested b ()
gate-success-sound failure unverified b ()
gate-success-sound failure noOp b ()
gate-success-sound failure unqualified b ()
gate-success-sound failure scanFailed b ()
gate-success-sound failure scanSkipped b ()
gate-success-sound failure otherVerification b ()
gate-success-sound failure missingVerification b ()
gate-success-sound falsePositive verified b ()
gate-success-sound falsePositive failed b ()
gate-success-sound falsePositive incomplete b ()
gate-success-sound falsePositive stillPresent b ()
gate-success-sound falsePositive rollbackRequested true ()
gate-success-sound falsePositive rollbackRequested false ()
gate-success-sound falsePositive unverified b ()
gate-success-sound falsePositive noOp b ()
gate-success-sound falsePositive unqualified b ()
gate-success-sound falsePositive scanFailed b ()
gate-success-sound falsePositive scanSkipped b ()
gate-success-sound falsePositive otherVerification b ()
gate-success-sound falsePositive missingVerification b ()
gate-success-sound otherOutcome verified b ()
gate-success-sound otherOutcome failed b ()
gate-success-sound otherOutcome incomplete b ()
gate-success-sound otherOutcome stillPresent b ()
gate-success-sound otherOutcome rollbackRequested b ()
gate-success-sound otherOutcome unverified b ()
gate-success-sound otherOutcome noOp b ()
gate-success-sound otherOutcome unqualified b ()
gate-success-sound otherOutcome scanFailed b ()
gate-success-sound otherOutcome scanSkipped b ()
gate-success-sound otherOutcome otherVerification b ()
gate-success-sound otherOutcome missingVerification b ()

credit-success-sound : ∀ r → classify r ≡ creditSuccess → (outcome r ≡ success) × (verification r ≡ verified)
credit-success-sound (row o v b) = gate-success-sound o v b

------------------------------------------------------------------------
-- Counts: one bucket for each row, and the counts add up to the rows read.

record Counts : Set where
  constructor counts
  field
    succ fail fp ign : Nat

open Counts public

infixl 6 _⊕_

_⊕_ : Counts → Counts → Counts
counts a b c d ⊕ counts a′ b′ c′ d′ = counts (a + a′) (b + b′) (c + c′) (d + d′)

zeroC : Counts
zeroC = counts 0 0 0 0

bump : Bucket → Counts
bump creditSuccess       = counts 1 0 0 0
bump creditFailure       = counts 0 1 0 0
bump creditFalsePositive = counts 0 0 1 0
bump ignored             = counts 0 0 0 1

count : List Row → Counts
count [] = zeroC
count (r ∷ rs) = bump (classify r) ⊕ count rs

total : Counts → Nat
total (counts a b c d) = a + (b + (c + d))

-- OutcomeTracker.qualified_count/1.
qualified : Counts → Nat
qualified c = succ c + (fail c + fp c)

cong₄ : ∀ {A : Set} (f : Nat → Nat → Nat → Nat → A) {a a′ b b′ c c′ d d′} →
        a ≡ a′ → b ≡ b′ → c ≡ c′ → d ≡ d′ → f a b c d ≡ f a′ b′ c′ d′
cong₄ f refl refl refl refl = refl

-- T1. A row is in exactly one bucket: the four counts add up to the rows read.
total-bump : ∀ b c → total (bump b ⊕ c) ≡ suc (total c)
total-bump creditSuccess (counts a b c d) = refl
total-bump creditFailure (counts a b c d) = +-suc a (b + (c + d))
total-bump creditFalsePositive (counts a b c d) =
  trans (cong (a +_) (+-suc b (c + d))) (+-suc a (b + (c + d)))
total-bump ignored (counts a b c d) =
  trans (cong (λ z → a + (b + z)) (+-suc c d))
        (trans (cong (a +_) (+-suc b (c + d))) (+-suc a (b + (c + d))))

counts-total : ∀ xs → total (count xs) ≡ length xs
counts-total [] = refl
counts-total (r ∷ rs) = trans (total-bump (classify r) (count rs)) (cong suc (counts-total rs))

-- T2. Counting is additive over appended logs (a replayed or split log gives
-- the sum of its parts), so the order of two batches does not matter.
⊕-assoc : ∀ x y z → (x ⊕ y) ⊕ z ≡ x ⊕ (y ⊕ z)
⊕-assoc (counts a b c d) (counts a′ b′ c′ d′) (counts a″ b″ c″ d″) =
  cong₄ counts (+-assoc a a′ a″) (+-assoc b b′ b″) (+-assoc c c′ c″) (+-assoc d d′ d″)

count-++ : ∀ xs ys → count (xs ++ ys) ≡ count xs ⊕ count ys
count-++ [] ys = refl
count-++ (r ∷ xs) ys =
  trans (cong (bump (classify r) ⊕_) (count-++ xs ys))
        (sym (⊕-assoc (bump (classify r)) (count xs) (count ys)))

+-comm : ∀ a b → a + b ≡ b + a
+-comm zero b = sym (+-identityʳ b)
+-comm (suc a) b = trans (cong suc (+-comm a b)) (sym (+-suc b a))

⊕-comm : ∀ x y → x ⊕ y ≡ y ⊕ x
⊕-comm (counts a b c d) (counts a′ b′ c′ d′) =
  cong₄ counts (+-comm a a′) (+-comm b b′) (+-comm c c′) (+-comm d d′)

count-swap : ∀ xs ys → count (xs ++ ys) ≡ count (ys ++ xs)
count-swap xs ys = trans (count-++ xs ys) (trans (⊕-comm (count xs) (count ys)) (sym (count-++ ys xs)))

-- T3. Rows that the gate ignores add nothing to any credited count.
AllIgnored : List Row → Set
AllIgnored [] = ⊤
AllIgnored (r ∷ rs) = (classify r ≡ ignored) × AllIgnored rs

count-ignored : ∀ ys → AllIgnored ys → count ys ≡ counts 0 0 0 (length ys)
count-ignored [] tt = refl
count-ignored (r ∷ ys) (p , ps) rewrite p | count-ignored ys ps = refl

⊕-ignored : ∀ c n → c ⊕ counts 0 0 0 n ≡ counts (succ c) (fail c) (fp c) (ign c + n)
⊕-ignored (counts a b c d) n =
  cong₄ counts (+-identityʳ a) (+-identityʳ b) (+-identityʳ c) refl

credits-neutral : ∀ xs ys → AllIgnored ys →
  (succ (count (xs ++ ys)) ≡ succ (count xs)) × (fail (count (xs ++ ys)) ≡ fail (count xs))
  × (fp (count (xs ++ ys)) ≡ fp (count xs))
credits-neutral xs ys ps rewrite count-++ xs ys | count-ignored ys ps =
  let e = ⊕-ignored (count xs) (length ys)
  in cong succ e , cong fail e , cong fp e

qualified-neutral : ∀ xs ys → AllIgnored ys → qualified (count (xs ++ ys)) ≡ qualified (count xs)
qualified-neutral xs ys ps with credits-neutral xs ys ps
... | s , f , p = trans (cong (λ z → z + (fail (count (xs ++ ys)) + fp (count (xs ++ ys)))) s)
                  (trans (cong (λ z → succ (count xs) + (z + fp (count (xs ++ ys)))) f)
                         (cong (λ z → succ (count xs) + (fail (count xs) + z)) p))

------------------------------------------------------------------------
-- T5. The guard of E0-04 and E0-27: the public entry point cannot stamp
-- "verified" (positive evidence) or "rollback_requested" (negative evidence about a
-- batch rollback; owner ruling 7A). Only a rescan, the qualified fleet import and
-- BatchRollback can.

sanitize : Verification → Verification
sanitize verified = unverified
sanitize rollbackRequested = unverified
sanitize v = v

sanitize-not-verified : ∀ v → sanitize v ≡ verified → ⊥
sanitize-not-verified verified ()
sanitize-not-verified failed ()
sanitize-not-verified incomplete ()
sanitize-not-verified stillPresent ()
sanitize-not-verified rollbackRequested ()
sanitize-not-verified unverified ()
sanitize-not-verified noOp ()
sanitize-not-verified unqualified ()
sanitize-not-verified scanFailed ()
sanitize-not-verified scanSkipped ()
sanitize-not-verified otherVerification ()
sanitize-not-verified missingVerification ()

sanitize-not-rollback : ∀ v → sanitize v ≡ rollbackRequested → ⊥
sanitize-not-rollback verified ()
sanitize-not-rollback failed ()
sanitize-not-rollback incomplete ()
sanitize-not-rollback stillPresent ()
sanitize-not-rollback rollbackRequested ()
sanitize-not-rollback unverified ()
sanitize-not-rollback noOp ()
sanitize-not-rollback unqualified ()
sanitize-not-rollback scanFailed ()
sanitize-not-rollback scanSkipped ()
sanitize-not-rollback otherVerification ()
sanitize-not-rollback missingVerification ()

-- The guard loses nothing but the two trusted-only values.
sanitize-keeps : ∀ v → (v ≡ verified → ⊥) → (v ≡ rollbackRequested → ⊥) → sanitize v ≡ v
sanitize-keeps verified n _ with n refl
... | ()
sanitize-keeps failed _ _ = refl
sanitize-keeps incomplete _ _ = refl
sanitize-keeps stillPresent _ _ = refl
sanitize-keeps rollbackRequested _ n with n refl
... | ()
sanitize-keeps unverified _ _ = refl
sanitize-keeps noOp _ _ = refl
sanitize-keeps unqualified _ _ = refl
sanitize-keeps scanFailed _ _ = refl
sanitize-keeps scanSkipped _ _ = refl
sanitize-keeps otherVerification _ _ = refl
sanitize-keeps missingVerification _ _ = refl

-- The row that OutcomeTracker.record_outcome/5 writes for a caller's claim.
publicRow : Outcome → Verification → Bool → Row
publicRow o v b = row o (sanitize v) b

public-never-credits-success : ∀ o v b → classify (publicRow o v b) ≡ creditSuccess → ⊥
public-never-credits-success o v b p with credit-success-sound (publicRow o v b) p
... | _ , q = sanitize-not-verified v q

-- E0-27. Through the public entry a false positive is credited only for
-- "still_present" (a claim that only lowers trust): never for a rollback and never
-- for "verified". So a made-up batch rollback is not counted.
public-fp-needs-still-present : ∀ v b → classify (publicRow falsePositive v b) ≡ creditFalsePositive →
  sanitize v ≡ stillPresent
public-fp-needs-still-present verified b ()
public-fp-needs-still-present failed b ()
public-fp-needs-still-present incomplete b ()
public-fp-needs-still-present stillPresent b _ = refl
public-fp-needs-still-present rollbackRequested b ()
public-fp-needs-still-present unverified b ()
public-fp-needs-still-present noOp b ()
public-fp-needs-still-present unqualified b ()
public-fp-needs-still-present scanFailed b ()
public-fp-needs-still-present scanSkipped b ()
public-fp-needs-still-present otherVerification b ()
public-fp-needs-still-present missingVerification b ()

public-never-credits-rollback : ∀ b → classify (publicRow falsePositive rollbackRequested b) ≡ ignored
public-never-credits-rollback b = refl

------------------------------------------------------------------------
-- T6. The fleet classification of E0-03 (LearningScheduler.classify_fleet_record/1).

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

data _⊎_ (A B : Set) : Set where
  inl : A → A ⊎ B
  inr : B → A ⊎ B

infixr 1 _⊎_

data Verdict : Set where
  vVerified vFailed vIncomplete vNoOp vUnverified vNone vOther : Verdict

data FOutcome : Set where
  fSuccess fFailure fOther : FOutcome

-- An integer field of the fleet row, as the guards read it.  `zeroFloat` is
-- 0.0: it is == 0 in Elixir and it is not an integer.  `absent` is a missing
-- field or a value that is not a number.
data Hit : Set where
  posInt negInt zeroInt zeroFloat absent : Hit

data Ctx : Set where
  cOk       : Ctx   -- a recipe and a repo, and not a row that Hypatia wrote itself
  cHypatia  : Ctx   -- "bot" == "hypatia"
  cInvalid  : Ctx   -- no recipe or no repo

_∧_ : Bool → Bool → Bool
true ∧ b = b
false ∧ _ = false

nonZeroInt : Hit → Bool       -- is_integer(x) and x != 0
nonZeroInt posInt = true
nonZeroInt negInt = true
nonZeroInt _ = false

positiveInt : Hit → Bool      -- is_integer(x) and x > 0
positiveInt posInt = true
positiveInt _ = false

equalsZero : Hit → Bool       -- x == 0
equalsZero zeroInt = true
equalsZero zeroFloat = true
equalsZero _ = false

-- The `case {verdict, outcome}` of the classifier, clause for clause.
qualify : Verdict → FOutcome → Hit → Hit → Hit → Maybe (Outcome × Verification)
qualify vVerified fSuccess _ before after =
  if positiveInt before ∧ equalsZero after then just (success , verified) else nothing
qualify vFailed fFailure exit _ _ =
  if nonZeroInt exit then just (failure , failed) else nothing
qualify vIncomplete fSuccess _ _ after =
  if positiveInt after then just (failure , incomplete) else nothing
qualify vIncomplete fFailure _ _ after =
  if positiveInt after then just (failure , incomplete) else nothing
qualify _ _ _ _ _ = nothing

auditVerification : Verdict → Verification
auditVerification vNoOp = noOp
auditVerification vUnverified = unverified
auditVerification _ = unqualified

auditOutcome : FOutcome → Outcome
auditOutcome fSuccess = success
auditOutcome fFailure = failure
auditOutcome fOther = otherOutcome

data FleetResult : Set where
  qualifiedRow : Outcome → Verification → FleetResult
  auditRow     : Outcome → Verification → FleetResult
  skipRow      : FleetResult

fleetFrom : Maybe (Outcome × Verification) → Verdict → FOutcome → FleetResult
fleetFrom (just (o , v)) _ _ = qualifiedRow o v
fleetFrom nothing vd fo = auditRow (auditOutcome fo) (auditVerification vd)

classifyFleet : Ctx → Verdict → FOutcome → Hit → Hit → Hit → FleetResult
classifyFleet cHypatia _ _ _ _ _ = skipRow
classifyFleet cInvalid _ _ _ _ _ = skipRow
classifyFleet cOk vd fo exit before after = fleetFrom (qualify vd fo exit before after) vd fo

-- F1/F2. A row that is kept as an audit row is ignored by the gate, whatever
-- the batch flag: it can give no positive and no negative learning.
data AuditSafe : Verification → Set where
  safeNoOp       : AuditSafe noOp
  safeUnverified : AuditSafe unverified
  safeUnqualified : AuditSafe unqualified

audit-verification-safe : ∀ vd → AuditSafe (auditVerification vd)
audit-verification-safe vVerified = safeUnqualified
audit-verification-safe vFailed = safeUnqualified
audit-verification-safe vIncomplete = safeUnqualified
audit-verification-safe vNoOp = safeNoOp
audit-verification-safe vUnverified = safeUnverified
audit-verification-safe vNone = safeUnqualified
audit-verification-safe vOther = safeUnqualified

safe-ignored : ∀ o v b → AuditSafe v → gate o v b ≡ ignored
safe-ignored success _ _ safeNoOp = refl
safe-ignored success _ _ safeUnverified = refl
safe-ignored success _ _ safeUnqualified = refl
safe-ignored failure _ _ safeNoOp = refl
safe-ignored failure _ _ safeUnverified = refl
safe-ignored failure _ _ safeUnqualified = refl
safe-ignored falsePositive _ _ safeNoOp = refl
safe-ignored falsePositive _ _ safeUnverified = refl
safe-ignored falsePositive _ _ safeUnqualified = refl
safe-ignored otherOutcome _ _ safeNoOp = refl
safe-ignored otherOutcome _ _ safeUnverified = refl
safe-ignored otherOutcome _ _ safeUnqualified = refl

-- The bucket of the row that the import stores for a result (a skipped row is not stored).
data Stored : Set where
  stored  : Bucket → Stored
  notKept : Stored

storedBucket : FleetResult → Bool → Stored
storedBucket (qualifiedRow o v) b = stored (gate o v b)
storedBucket (auditRow o v) b = stored (gate o v b)
storedBucket skipRow _ = notKept

isAudit : FleetResult → Bool
isAudit (auditRow _ _) = true
isAudit _ = false

isQualified : FleetResult → Bool
isQualified (qualifiedRow _ _) = true
isQualified _ = false

false≢true : false ≡ true → ⊥
false≢true ()

fleetFrom-audit : ∀ m vd fo b → isAudit (fleetFrom m vd fo) ≡ true →
  storedBucket (fleetFrom m vd fo) b ≡ stored ignored
fleetFrom-audit nothing vd fo b _ =
  cong stored (safe-ignored (auditOutcome fo) (auditVerification vd) b (audit-verification-safe vd))
fleetFrom-audit (just (o , v)) vd fo b ()

-- F2. Whatever the batch flag, a row that the classifier keeps as an audit row
-- is put in the ignored bucket by the gate: no positive and no negative learning.
fleet-audit-ignored : ∀ c vd fo e bf af b → isAudit (classifyFleet c vd fo e bf af) ≡ true →
  storedBucket (classifyFleet c vd fo e bf af) b ≡ stored ignored
fleet-audit-ignored cOk vd fo e bf af b = fleetFrom-audit (qualify vd fo e bf af) vd fo b
fleet-audit-ignored cHypatia vd fo e bf af b ()
fleet-audit-ignored cInvalid vd fo e bf af b ()

-- F3. A row that the classifier qualifies is never put in the ignored bucket.
qualifiedBucketOK : Maybe (Outcome × Verification) → Bool → Bool
qualifiedBucketOK nothing _ = true
qualifiedBucketOK (just (o , v)) b = notIgnored (gate o v b)
  where
    notIgnored : Bucket → Bool
    notIgnored ignored = false
    notIgnored _ = true

qualify-credited : ∀ vd fo e bf af b → qualifiedBucketOK (qualify vd fo e bf af) b ≡ true
qualify-credited vVerified fSuccess e bf af b with positiveInt bf ∧ equalsZero af
... | true = refl
... | false = refl
qualify-credited vVerified fFailure e bf af b = refl
qualify-credited vVerified fOther e bf af b = refl
qualify-credited vFailed fSuccess e bf af b = refl
qualify-credited vFailed fFailure e bf af b with nonZeroInt e
... | true = refl
... | false = refl
qualify-credited vFailed fOther e bf af b = refl
qualify-credited vIncomplete fSuccess e bf af b with positiveInt af
... | true = refl
... | false = refl
qualify-credited vIncomplete fFailure e bf af b with positiveInt af
... | true = refl
... | false = refl
qualify-credited vIncomplete fOther e bf af b = refl
qualify-credited vNoOp fo e bf af b = refl
qualify-credited vUnverified fo e bf af b = refl
qualify-credited vNone fo e bf af b = refl
qualify-credited vOther fo e bf af b = refl

fleetFrom-qualified : ∀ m vd fo b → qualifiedBucketOK m b ≡ true → isQualified (fleetFrom m vd fo) ≡ true →
  (storedBucket (fleetFrom m vd fo) b ≡ stored ignored) → ⊥
fleetFrom-qualified nothing vd fo b _ ()
fleetFrom-qualified (just (o , v)) vd fo b ok _ q with gate o v b | ok
... | creditSuccess | _ = stored-not-ignored q
  where stored-not-ignored : stored creditSuccess ≡ stored ignored → ⊥
        stored-not-ignored ()
... | creditFailure | _ = stored-not-ignored q
  where stored-not-ignored : stored creditFailure ≡ stored ignored → ⊥
        stored-not-ignored ()
... | creditFalsePositive | _ = stored-not-ignored q
  where stored-not-ignored : stored creditFalsePositive ≡ stored ignored → ⊥
        stored-not-ignored ()
... | ignored | ()

-- F5. E0-03: the fleet writes the legacy outcome "success" beside "incomplete".
-- With the weak point still there afterwards (after_hits a positive integer),
-- the row is qualified as the failure that it is, whatever the other two
-- numbers are, for both outcomes.
incomplete-success-is-failure : ∀ e bf → classifyFleet cOk vIncomplete fSuccess e bf posInt ≡ qualifiedRow failure incomplete
incomplete-success-is-failure e bf = refl

incomplete-failure-is-failure : ∀ e bf → classifyFleet cOk vIncomplete fFailure e bf posInt ≡ qualifiedRow failure incomplete
incomplete-failure-is-failure e bf = refl

-- ... and without it, it is an audit row, never a success.
incomplete-without-hits-is-audit : ∀ e bf → classifyFleet cOk vIncomplete fSuccess e bf zeroInt ≡ auditRow success unqualified
incomplete-without-hits-is-audit e bf = refl

-- F6. Contradictory evidence (verdict and outcome disagree) is audit evidence.
failed-with-success-is-audit : ∀ e bf af → classifyFleet cOk vFailed fSuccess e bf af ≡ auditRow success unqualified
failed-with-success-is-audit e bf af = refl

verified-with-failure-is-audit : ∀ e bf af → classifyFleet cOk vVerified fFailure e bf af ≡ auditRow failure unqualified
verified-with-failure-is-audit e bf af = refl

-- ... and "verified" needs the numbers: a positive before and a zero after.
verified-needs-before-positive : ∀ e af → classifyFleet cOk vVerified fSuccess e zeroInt af ≡ auditRow success unqualified
verified-needs-before-positive e af = refl

-- A row that Hypatia wrote itself, or with no recipe or no repo, is never stored.
hypatia-rows-skipped : ∀ vd fo e bf af → classifyFleet cHypatia vd fo e bf af ≡ skipRow
hypatia-rows-skipped vd fo e bf af = refl

------------------------------------------------------------------------
-- Maturity: the stage and the dispatch tier depend on the qualified count
-- only (ConfidenceAnnealing.compute_stage/1 and max_dispatch_tier/1).

data Stage : Set where
  nascent adolescent mature veteran : Stage

data Tier : Set where
  reportOnly review autoExecute : Tier

pick : Bool → Bool → Bool → Stage
pick true  _     _     = nascent
pick false true  _     = adolescent
pick false false true  = mature
pick false false false = veteran

-- {nascent, 0} {adolescent, 5} {mature, 15} {veteran, 50}
stageOf : Nat → Stage
stageOf n = pick (n < 5) (n < 15) (n < 50)

tierOf : Stage → Tier
tierOf nascent = reportOnly
tierOf adolescent = review
tierOf mature = autoExecute
tierOf veteran = autoExecute

rowsTier : List Row → Tier
rowsTier xs = tierOf (stageOf (qualified (count xs)))

-- A3. Rows that the gate ignores cannot raise the stage or the tier: an exit
-- code of zero, an absent verifier, a scan that was skipped, an audit row.
ignored-rows-keep-tier : ∀ xs ys → AllIgnored ys → rowsTier (xs ++ ys) ≡ rowsTier xs
ignored-rows-keep-tier xs ys p = cong (λ n → tierOf (stageOf n)) (qualified-neutral xs ys p)

ignored-rows-keep-stage : ∀ xs ys → AllIgnored ys →
  stageOf (qualified (count (xs ++ ys))) ≡ stageOf (qualified (count xs))
ignored-rows-keep-stage xs ys p = cong stageOf (qualified-neutral xs ys p)

-- S1. No recipe is dispatched at the auto-execute tier on fewer than 15
-- qualified outcomes.
auto-aux : ∀ n b5 b15 b50 → (n < 15) ≡ b15 → tierOf (pick b5 b15 b50) ≡ autoExecute → 15 ≤ n
auto-aux n true  b15 b50 e ()
auto-aux n false true b50 e ()
auto-aux n false false b50 e _ = not-lt⇒le n 15 e

auto-needs-15 : ∀ n → tierOf (stageOf n) ≡ autoExecute → 15 ≤ n
auto-needs-15 n = auto-aux n (n < 5) (n < 15) (n < 50) refl

-- S2. No recipe is dispatched above report-only on fewer than 5 qualified outcomes.
review-aux : ∀ n b5 b15 b50 → (n < 5) ≡ b5 → (tierOf (pick b5 b15 b50) ≡ reportOnly → ⊥) → 5 ≤ n
review-aux n true b15 b50 e q with q refl
... | ()
review-aux n false b15 b50 e q = not-lt⇒le n 5 e

above-report-needs-5 : ∀ n → (tierOf (stageOf n) ≡ reportOnly → ⊥) → 5 ≤ n
above-report-needs-5 n = review-aux n (n < 5) (n < 15) (n < 50) refl

-- S3. For a log, the same two bounds in terms of the qualified rows.
rows-auto-needs-15 : ∀ xs → rowsTier xs ≡ autoExecute → 15 ≤ qualified (count xs)
rows-auto-needs-15 xs = auto-needs-15 (qualified (count xs))

------------------------------------------------------------------------
-- Non-vacuity and a planted control.  The theorems above are not true for
-- want of cases: the gate does credit, the hypotheses do hold for real
-- logs, and without the guard a forged "verified" would be credited.

credit-exists : classify (row success verified false) ≡ creditSuccess
credit-exists = refl

-- The control: the row that a forged claim would produce without the guard.
unguarded-credits : classify (row success verified false) ≡ creditSuccess
unguarded-credits = refl

guarded-claim-ignored : classify (publicRow success verified false) ≡ ignored
guarded-claim-ignored = refl

-- The control for E0-27: a rollback row that a forged claim would produce without
-- the guard is credited (the batch flag is true when caused_by and batch_id are given).
unguarded-rollback-credits : classify (row falsePositive rollbackRequested true) ≡ creditFalsePositive
unguarded-rollback-credits = refl

guarded-rollback-ignored : classify (publicRow falsePositive rollbackRequested true) ≡ ignored
guarded-rollback-ignored = refl

ignored-rows-witness : AllIgnored (row success unverified false ∷ row success noOp true ∷ row failure unqualified false ∷ [])
ignored-rows-witness = refl , refl , refl , tt

-- Four ignored rows after one credited row: the count of the credited row is
-- unchanged, the four are all in the ignored bucket, and the tier is the same.
ignored-after-credit : count (row success verified false ∷ row success unverified false ∷ row success noOp false ∷ [])
                       ≡ counts 1 0 0 2
ignored-after-credit = refl
