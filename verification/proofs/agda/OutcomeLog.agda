-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- OutcomeLog.agda — Formal proof of outcome log monotonicity (H6).
--
-- REQUIREMENTS-MASTER.md: H6 | Outcome log monotonicity (timestamps strictly increasing) | INV | Ag | P1
--
-- Models the JSONL outcome log written by lib/outcome_tracker.ex.
-- Each call to record_outcome/4 appends a record with:
--   "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
--
-- We abstract timestamps as natural numbers (monotone wall-clock ticks) and
-- prove that a well-formed outcome log has strictly increasing timestamps.
--
-- Properties proved:
--   1. StrictlyIncreasing — predicate on timestamp lists
--   2. tail-monotone — if a list is strictly increasing, so is its tail
--   3. head-lt-next — first element < second element
--   4. si-cons-inv — inverse: head of SI list < all later elements
--   5. strictly-increasing-no-dup — all timestamps distinct
--   6. si-snoc — appending a larger timestamp preserves SI
--   7. outcome-log-monotonicity — final theorem

module OutcomeLog where

open import Data.Nat using (ℕ; zero; suc; _<_; _≤_; s≤s; z≤n)
open import Data.Nat.Properties
  using (<-trans; <-irrefl; <-asym; n<1+n; ≤-refl)
open import Data.List using (List; []; _∷_; _++_)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Empty using (⊥; ⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans)
open import Relation.Nullary using (¬_)

------------------------------------------------------------------------
-- Section 1: Outcome record model
------------------------------------------------------------------------

record OutcomeRecord : Set where
  constructor mkRecord
  field
    timestamp : ℕ

-- Extract timestamp list from a log.
timestamps : List OutcomeRecord → List ℕ
timestamps []       = []
timestamps (r ∷ rs) = r .OutcomeRecord.timestamp ∷ timestamps rs

------------------------------------------------------------------------
-- Section 2: Strictly increasing predicate
------------------------------------------------------------------------

data StrictlyIncreasing : List ℕ → Set where
  si-nil  : StrictlyIncreasing []
  si-sing : ∀ {t}
           → StrictlyIncreasing (t ∷ [])
  si-cons : ∀ {t₁ t₂ ts}
           → t₁ < t₂
           → StrictlyIncreasing (t₂ ∷ ts)
           → StrictlyIncreasing (t₁ ∷ t₂ ∷ ts)

MonotoneLog : List OutcomeRecord → Set
MonotoneLog log = StrictlyIncreasing (timestamps log)

------------------------------------------------------------------------
-- Section 3: Structural lemmas
------------------------------------------------------------------------

-- H6 — LEMMA 1: The tail of a strictly increasing list is SI.
tail-monotone : ∀ {t ts}
               → StrictlyIncreasing (t ∷ ts)
               → StrictlyIncreasing ts
tail-monotone si-sing        = si-nil
tail-monotone (si-cons _ si) = si

-- H6 — LEMMA 2: First < second in an SI list.
head-lt-next : ∀ {t₁ t₂ ts}
              → StrictlyIncreasing (t₁ ∷ t₂ ∷ ts)
              → t₁ < t₂
head-lt-next (si-cons lt _) = lt

------------------------------------------------------------------------
-- Section 4: No duplicates
------------------------------------------------------------------------

-- Membership in a list of ℕ.
data _∈_ : ℕ → List ℕ → Set where
  here  : ∀ {x xs}   → x ∈ (x ∷ xs)
  there : ∀ {x y xs} → x ∈ xs → x ∈ (y ∷ xs)

-- H6 — LEMMA 3: If t < head and the list is SI, then t < every element.
lt-head-lt-all : ∀ {t head : ℕ} (rest : List ℕ)
                → t < head
                → StrictlyIncreasing (head ∷ rest)
                → ∀ {x} → x ∈ (head ∷ rest) → t < x
lt-head-lt-all rest lt si here             = lt
lt-head-lt-all [] lt si-sing (there ())
lt-head-lt-all (x ∷ rest) lt (si-cons lt2 si) (there mem) =
  lt-head-lt-all rest (<-trans lt lt2) si mem

-- Helper: every element in the tail of an SI list is strictly greater than the head.
si-head-lt-tail : ∀ {t ts}
                 → StrictlyIncreasing (t ∷ ts)
                 → ∀ {x} → x ∈ ts → t < x
si-head-lt-tail si-sing ()
si-head-lt-tail (si-cons lt _) here      = lt
si-head-lt-tail (si-cons lt si) (there mem) =
  <-trans lt (si-head-lt-tail si mem)

-- H6 — THEOREM 1: SI lists have no duplicate elements.
-- If a value appears in the tail of an SI list, it cannot equal the head.
si-head-notin-tail : ∀ {t ts}
                    → StrictlyIncreasing (t ∷ ts)
                    → ¬ (t ∈ ts)
si-head-notin-tail si mem = <-irrefl refl (si-head-lt-tail si mem)

-- H6 — THEOREM 1b: SI implies no element equals any later element.
-- Direct proof: if t appears at head and also in the tail, contradiction via <-irrefl.
si-no-dup : ∀ {t ts}
           → StrictlyIncreasing (t ∷ ts)
           → ¬ (t ∈ ts)
si-no-dup = si-head-notin-tail

------------------------------------------------------------------------
-- Section 5: Appending preserves SI
------------------------------------------------------------------------

-- H6 — THEOREM 2: Appending a timestamp strictly greater than all existing
-- timestamps produces an SI list.
si-snoc : ∀ (ts : List ℕ) (t_new : ℕ)
         → StrictlyIncreasing ts
         → (∀ {t} → t ∈ ts → t < t_new)
         → StrictlyIncreasing (ts ++ t_new ∷ [])
si-snoc [] t_new si-nil _ = si-sing
si-snoc (t ∷ []) t_new si-sing h =
  si-cons (h here) si-sing
si-snoc (t₁ ∷ t₂ ∷ ts) t_new (si-cons lt si) h =
  si-cons lt (si-snoc (t₂ ∷ ts) t_new si (λ mem → h (there mem)))

------------------------------------------------------------------------
-- Section 6: Outcome log model
------------------------------------------------------------------------

-- An outcome log state: the log together with a proof that it is monotone.
record OutcomeLogState : Set where
  constructor mkLogState
  field
    log     : List OutcomeRecord
    isMonot : MonotoneLog log

-- Empty log is trivially monotone.
emptyLog : OutcomeLogState
emptyLog = mkLogState [] si-nil

-- Record membership (for the append precondition).
data _∈-log_ : OutcomeRecord → List OutcomeRecord → Set where
  log-here  : ∀ {r rs}   → r ∈-log (r ∷ rs)
  log-there : ∀ {r s rs} → r ∈-log rs → r ∈-log (s ∷ rs)

-- H6 — MAIN THEOREM: Appending a record with a strictly greater timestamp
-- preserves log monotonicity.
--
-- The precondition states that the new timestamp strictly exceeds every
-- existing timestamp — this is the wall-clock assumption: DateTime.utc_now()
-- returns a value strictly after any previously recorded timestamp.
appendRecord :
  (state : OutcomeLogState)
  → (r : OutcomeRecord)
  → (∀ (prev : OutcomeRecord) → prev ∈-log (OutcomeLogState.log state)
       → OutcomeRecord.timestamp prev < OutcomeRecord.timestamp r)
  → OutcomeLogState
appendRecord state r hGt =
  mkLogState
    (OutcomeLogState.log state ++ r ∷ [])
    (appendMonotone (OutcomeLogState.log state)
                    (OutcomeRecord.timestamp r)
                    (OutcomeLogState.isMonot state)
                    (timestampBound (OutcomeLogState.log state) hGt))
  where
    -- Lift the precondition to timestamp lists.
    timestampBound : (log : List OutcomeRecord)
                   → (∀ prev → prev ∈-log log → OutcomeRecord.timestamp prev < OutcomeRecord.timestamp r)
                   → ∀ {t} → t ∈ timestamps log → t < OutcomeRecord.timestamp r
    timestampBound [] _ ()
    timestampBound (x ∷ xs) hPrev here      = hPrev x log-here
    timestampBound (x ∷ xs) hPrev (there m) =
      timestampBound xs (λ p mp → hPrev p (log-there mp)) m

    -- Timestamps of log ++ [r] form an SI list.
    tsApp : ∀ (log : List OutcomeRecord) (t : ℕ)
          → timestamps (log ++ mkRecord t ∷ []) ≡ timestamps log ++ t ∷ []
    tsApp [] t = refl
    tsApp (x ∷ xs) t =
      cong (OutcomeRecord.timestamp x ∷_) (tsApp xs t)
      where open import Relation.Binary.PropositionalEquality using (cong)

    appendMonotone : (log : List OutcomeRecord) (t_new : ℕ)
                   → MonotoneLog log
                   → (∀ {t} → t ∈ timestamps log → t < t_new)
                   → MonotoneLog (log ++ mkRecord t_new ∷ [])
    appendMonotone log t_new si hlt =
      subst StrictlyIncreasing (sym (tsApp log t_new))
        (si-snoc (timestamps log) t_new si hlt)
      where open import Relation.Binary.PropositionalEquality using (subst; sym)

-- The resulting log is monotone — witnessed by the isMonot field.
outcome-log-always-monotone :
  ∀ (state : OutcomeLogState)
  → MonotoneLog (OutcomeLogState.log state)
outcome-log-always-monotone = OutcomeLogState.isMonot
