{-# OPTIONS --safe --without-K --cubical-compatible --double-check #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Finite model of the fleet import (LearningScheduler.import_fleet_lines/2):
-- replay, identical-but-distinct attempts, and a failed write.  Builtins only.
--
-- The one thing assumed about identity is the module parameter `same-refl`:
-- an id is the same as itself.  Whether two DIFFERENT lines get different ids
-- is the property of the id function (today a hash of line number and line;
-- after E0-11 a producer event_id), and it is a hypothesis of the theorems
-- that need it, never an axiom.
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Equality

module E0Import (Id : Set) (same : Id → Id → Bool) (same-refl : ∀ x → same x x ≡ true) where

data ⊥ : Set where

record ⊤ : Set where
  constructor tt

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixr 4 _,_
infixr 2 _×_

cong : ∀ {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl q = q

sym : ∀ {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

+-identityʳ : ∀ n → n + 0 ≡ n
+-identityʳ zero = refl
+-identityʳ (suc n) = cong suc (+-identityʳ n)

_∨_ : Bool → Bool → Bool
true ∨ _ = true
false ∨ b = b

data Status : Set where
  running halted : Status

-- A source line: the identity of the event, whether it is learning evidence
-- (a qualified row) or only an audit row, and whether its write succeeds.
record Line : Set where
  constructor line
  field
    lineId  : Id
    credit  : Bool
    writeOk : Bool

open Line public

record St : Set where
  constructor st
  field
    seen   : List Id
    n      : Nat
    status : Status

open St public

member : Id → List Id → Bool
member x [] = false
member x (y ∷ ys) = same x y ∨ member x ys

creditN : Line → Nat
creditN l with credit l
... | true = 1
... | false = 0

-- One line.  A known id is skipped.  A failed write halts the import and
-- does not acknowledge the id.  Otherwise the id is acknowledged and a
-- qualified row is credited once.
stepRun : Line → List Id → Nat → Bool → Bool → St
stepRun l s k true  _     = st s k running
stepRun l s k false false = st s k halted
stepRun l s k false true  = st (lineId l ∷ s) (k + creditN l) running

step : Line → St → St
step l (st s k halted) = st s k halted
step l (st s k running) = stepRun l s k (member (lineId l) s) (writeOk l)

run : List Line → St → St
run [] z = z
run (l ∷ ls) z = run ls (step l z)

------------------------------------------------------------------------
-- Membership

member-self : ∀ x s → member x (x ∷ s) ≡ true
member-self x s rewrite same-refl x = refl

member-cons : ∀ x y s → member x s ≡ true → member x (y ∷ s) ≡ true
member-cons x y s p rewrite p with same x y
... | true = refl
... | false = refl

-- The seen set only grows.
step-grows : ∀ l z x → member x (seen z) ≡ true → member x (seen (step l z)) ≡ true
step-grows l (st s k halted) x p = p
step-grows l (st s k running) x p = stepRun-grows l s k (member (lineId l) s) (writeOk l) x p
  where
    stepRun-grows : ∀ l s k m w x → member x s ≡ true → member x (seen (stepRun l s k m w)) ≡ true
    stepRun-grows l s k true w x p = p
    stepRun-grows l s k false false x p = p
    stepRun-grows l s k false true x p = member-cons x (lineId l) s p

run-grows : ∀ ls z x → member x (seen z) ≡ true → member x (seen (run ls z)) ≡ true
run-grows [] z x p = p
run-grows (l ∷ ls) z x p = run-grows ls (step l z) x (step-grows l z x p)

------------------------------------------------------------------------
-- A1. Replay.  A second pass over the same source, from the seen set that the
-- first pass left, credits nothing and changes nothing.

Known : List Id → List Line → Set
Known s [] = ⊤
Known s (l ∷ ls) = (member (lineId l) s ≡ true) × Known s ls

Succeeds : List Line → Set
Succeeds [] = ⊤
Succeeds (l ∷ ls) = (writeOk l ≡ true) × Succeeds ls

-- A pass over lines that are all known changes nothing.
run-known : ∀ ls s k → Known s ls → run ls (st s k running) ≡ st s k running
run-known [] s k tt = refl
run-known (l ∷ ls) s k (p , ps) rewrite p = run-known ls s k ps

-- A running line whose write succeeds leaves its id in the seen set, and
-- the import is still running.
step-knows : ∀ l z → writeOk l ≡ true → status z ≡ running → member (lineId l) (seen (step l z)) ≡ true
step-knows l (st s k halted) w ()
step-knows l (st s k running) w _ rewrite w = go (member (lineId l) s) refl
  where
    go : ∀ m → m ≡ member (lineId l) s → member (lineId l) (seen (stepRun l s k m true)) ≡ true
    go true  e = sym e
    go false e = member-self (lineId l) s

step-running : ∀ l z → writeOk l ≡ true → status z ≡ running → status (step l z) ≡ running
step-running l (st s k halted) w ()
step-running l (st s k running) w _ rewrite w = go (member (lineId l) s)
  where
    go : ∀ m → status (stepRun l s k m true) ≡ running
    go true = refl
    go false = refl

-- A pass over lines whose writes all succeed knows every id afterwards, and
-- is still running.
pass-knows : ∀ ls z → Succeeds ls → status z ≡ running →
  Known (seen (run ls z)) ls × (status (run ls z) ≡ running)
pass-knows [] z tt r = tt , r
pass-knows (l ∷ ls) z (w , ws) r with pass-knows ls (step l z) ws (step-running l z w r)
... | knowns , stat =
  (run-grows ls (step l z) (lineId l) (step-knows l z w r) , knowns) , stat

replay-credits-nothing : ∀ ls s k → Succeeds ls →
  run ls (st (seen (run ls (st s k running))) 0 running) ≡ st (seen (run ls (st s k running))) 0 running
replay-credits-nothing ls s k ws with pass-knows ls (st s k running) ws refl
... | knowns , _ = run-known ls (seen (run ls (st s k running))) 0 knowns

------------------------------------------------------------------------
-- A2. Distinct attempts with identical outcomes credit twice; the replay of
-- one event never credits twice.

-- Two different events (their ids differ: the hypothesis `distinct`, which is
-- the property of the id function) are both acknowledged and both credited.
distinct-attempts-credit-twice : ∀ l₁ l₂ → writeOk l₁ ≡ true → writeOk l₂ ≡ true →
  same (lineId l₂) (lineId l₁) ≡ false →
  run (l₁ ∷ l₂ ∷ []) (st [] 0 running) ≡ st (lineId l₂ ∷ lineId l₁ ∷ []) (creditN l₁ + creditN l₂) running
distinct-attempts-credit-twice l₁ l₂ w₁ w₂ d rewrite w₁ | w₂ | d = refl

-- The same event delivered twice (equal ids) is credited once.
same-event-credits-once : ∀ l₁ l₂ → writeOk l₁ ≡ true → lineId l₂ ≡ lineId l₁ →
  run (l₁ ∷ l₂ ∷ []) (st [] 0 running) ≡ st (lineId l₁ ∷ []) (creditN l₁) running
same-event-credits-once l₁ l₂ w₁ e rewrite w₁ | e | same-refl (lineId l₁) = refl

-- An audit row (credit = false) is acknowledged and adds nothing to the count.
audit-row-credits-nothing : ∀ s k i → member i s ≡ false →
  step (line i false true) (st s k running) ≡ st (i ∷ s) k running
audit-row-credits-nothing s k i p rewrite p | +-identityʳ k = refl

------------------------------------------------------------------------
-- A4. A failed write does not acknowledge the event.  The import halts, the
-- seen set and the credit are unchanged, the lines after it are not read, and
-- the next poll imports the event once.

failed-write-halts : ∀ l s k → member (lineId l) s ≡ false → writeOk l ≡ false →
  step l (st s k running) ≡ st s k halted
failed-write-halts l s k m w rewrite m | w = refl

halted-stays : ∀ ls s k → run ls (st s k halted) ≡ st s k halted
halted-stays [] s k = refl
halted-stays (l ∷ ls) s k = halted-stays ls s k

failed-write-leaves-the-rest : ∀ l ls s k → member (lineId l) s ≡ false → writeOk l ≡ false →
  run (l ∷ ls) (st s k running) ≡ st s k halted
failed-write-leaves-the-rest l ls s k m w rewrite failed-write-halts l s k m w = halted-stays ls s k

-- The next poll, with the obstacle gone: the same event is imported once.
retry-imports-once : ∀ i c s k → member i s ≡ false →
  run (line i c false ∷ []) (st s k running) ≡ st s k halted
  × (run (line i c true ∷ []) (st s k running) ≡ st (i ∷ s) (k + creditN (line i c true)) running)
retry-imports-once i c s k m rewrite m = refl , refl

-- ... and a third poll, over the same source, imports nothing.
no-third-import : ∀ i c s k → member i s ≡ false →
  run (line i c true ∷ []) (st (i ∷ s) (k + creditN (line i c true)) running)
  ≡ st (i ∷ s) (k + creditN (line i c true)) running
no-third-import i c s k m rewrite member-self i s = refl
