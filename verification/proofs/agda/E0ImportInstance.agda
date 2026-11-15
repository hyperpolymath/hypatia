{-# OPTIONS --safe --without-K --cubical-compatible --double-check #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- The import model of E0Import.agda, instantiated with natural-number ids, so
-- that its parameter and its hypotheses are shown to be satisfiable, and its
-- theorems to apply to a concrete log (non-vacuity).
module E0ImportInstance where

open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Equality

==-refl : ∀ n → (n == n) ≡ true
==-refl zero = refl
==-refl (suc n) = ==-refl n

open import E0Import Nat _==_ ==-refl

-- Three lines: ids 1 and 2 are different events with the same payload
-- (both qualified), id 3 is an audit row. All writes succeed.
log : List Line
log = line 1 true true ∷ line 2 true true ∷ line 3 false true ∷ []

-- First delivery: 3 events acknowledged, 2 credited.
first-pass : run log (st [] 0 running) ≡ st (3 ∷ 2 ∷ 1 ∷ []) 2 running
first-pass = refl

-- The theorem of A1 applies to it, and gives (by its proof) a second pass
-- that credits nothing:
second-pass : run log (st (3 ∷ 2 ∷ 1 ∷ []) 0 running) ≡ st (3 ∷ 2 ∷ 1 ∷ []) 0 running
second-pass = replay-credits-nothing log [] 0 (refl , refl , refl , tt)

-- Two attempts with identical content but different ids credit twice (A2).
twice : run (line 7 true true ∷ line 8 true true ∷ []) (st [] 0 running) ≡ st (8 ∷ 7 ∷ []) 2 running
twice = distinct-attempts-credit-twice (line 7 true true) (line 8 true true) refl refl refl

-- A failed write on id 2: nothing after it is read, nothing is acknowledged
-- for it, and the next poll imports it once (A4).
failed : run (line 1 true true ∷ line 2 true false ∷ line 3 true true ∷ []) (st [] 0 running)
         ≡ st (1 ∷ []) 1 halted
failed = refl

retried : run (line 2 true true ∷ line 3 true true ∷ []) (st (1 ∷ []) 1 running)
          ≡ st (3 ∷ 2 ∷ 1 ∷ []) 3 running
retried = refl
