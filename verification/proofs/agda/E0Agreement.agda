{-# OPTIONS --safe --without-K --cubical-compatible --double-check #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Equality on the finite types of E0Model, with SOUNDNESS PROVED, and the
-- agreement predicates that the generated table module (E0ModelTables)
-- proves.  The soundness lemmas are what make an agreement result mean
-- something: a checker that said "true" too easily could not have them.
module E0Agreement where

open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Equality
open import E0Model

eqOutcome : Outcome → Outcome → Bool
eqOutcome success success = true
eqOutcome failure failure = true
eqOutcome falsePositive falsePositive = true
eqOutcome otherOutcome otherOutcome = true
eqOutcome _ _ = false

eqOutcome-sound : ∀ a b → eqOutcome a b ≡ true → a ≡ b
eqOutcome-sound success success _ = refl
eqOutcome-sound success failure ()
eqOutcome-sound success falsePositive ()
eqOutcome-sound success otherOutcome ()
eqOutcome-sound failure success ()
eqOutcome-sound failure failure _ = refl
eqOutcome-sound failure falsePositive ()
eqOutcome-sound failure otherOutcome ()
eqOutcome-sound falsePositive success ()
eqOutcome-sound falsePositive failure ()
eqOutcome-sound falsePositive falsePositive _ = refl
eqOutcome-sound falsePositive otherOutcome ()
eqOutcome-sound otherOutcome success ()
eqOutcome-sound otherOutcome failure ()
eqOutcome-sound otherOutcome falsePositive ()
eqOutcome-sound otherOutcome otherOutcome _ = refl
eqVerification : Verification → Verification → Bool
eqVerification verified verified = true
eqVerification failed failed = true
eqVerification incomplete incomplete = true
eqVerification stillPresent stillPresent = true
eqVerification rollbackRequested rollbackRequested = true
eqVerification unverified unverified = true
eqVerification noOp noOp = true
eqVerification unqualified unqualified = true
eqVerification scanFailed scanFailed = true
eqVerification scanSkipped scanSkipped = true
eqVerification otherVerification otherVerification = true
eqVerification missingVerification missingVerification = true
eqVerification _ _ = false

eqVerification-sound : ∀ a b → eqVerification a b ≡ true → a ≡ b
eqVerification-sound verified verified _ = refl
eqVerification-sound verified failed ()
eqVerification-sound verified incomplete ()
eqVerification-sound verified stillPresent ()
eqVerification-sound verified rollbackRequested ()
eqVerification-sound verified unverified ()
eqVerification-sound verified noOp ()
eqVerification-sound verified unqualified ()
eqVerification-sound verified scanFailed ()
eqVerification-sound verified scanSkipped ()
eqVerification-sound verified otherVerification ()
eqVerification-sound verified missingVerification ()
eqVerification-sound failed verified ()
eqVerification-sound failed failed _ = refl
eqVerification-sound failed incomplete ()
eqVerification-sound failed stillPresent ()
eqVerification-sound failed rollbackRequested ()
eqVerification-sound failed unverified ()
eqVerification-sound failed noOp ()
eqVerification-sound failed unqualified ()
eqVerification-sound failed scanFailed ()
eqVerification-sound failed scanSkipped ()
eqVerification-sound failed otherVerification ()
eqVerification-sound failed missingVerification ()
eqVerification-sound incomplete verified ()
eqVerification-sound incomplete failed ()
eqVerification-sound incomplete incomplete _ = refl
eqVerification-sound incomplete stillPresent ()
eqVerification-sound incomplete rollbackRequested ()
eqVerification-sound incomplete unverified ()
eqVerification-sound incomplete noOp ()
eqVerification-sound incomplete unqualified ()
eqVerification-sound incomplete scanFailed ()
eqVerification-sound incomplete scanSkipped ()
eqVerification-sound incomplete otherVerification ()
eqVerification-sound incomplete missingVerification ()
eqVerification-sound stillPresent verified ()
eqVerification-sound stillPresent failed ()
eqVerification-sound stillPresent incomplete ()
eqVerification-sound stillPresent stillPresent _ = refl
eqVerification-sound stillPresent rollbackRequested ()
eqVerification-sound stillPresent unverified ()
eqVerification-sound stillPresent noOp ()
eqVerification-sound stillPresent unqualified ()
eqVerification-sound stillPresent scanFailed ()
eqVerification-sound stillPresent scanSkipped ()
eqVerification-sound stillPresent otherVerification ()
eqVerification-sound stillPresent missingVerification ()
eqVerification-sound rollbackRequested verified ()
eqVerification-sound rollbackRequested failed ()
eqVerification-sound rollbackRequested incomplete ()
eqVerification-sound rollbackRequested stillPresent ()
eqVerification-sound rollbackRequested rollbackRequested _ = refl
eqVerification-sound rollbackRequested unverified ()
eqVerification-sound rollbackRequested noOp ()
eqVerification-sound rollbackRequested unqualified ()
eqVerification-sound rollbackRequested scanFailed ()
eqVerification-sound rollbackRequested scanSkipped ()
eqVerification-sound rollbackRequested otherVerification ()
eqVerification-sound rollbackRequested missingVerification ()
eqVerification-sound unverified verified ()
eqVerification-sound unverified failed ()
eqVerification-sound unverified incomplete ()
eqVerification-sound unverified stillPresent ()
eqVerification-sound unverified rollbackRequested ()
eqVerification-sound unverified unverified _ = refl
eqVerification-sound unverified noOp ()
eqVerification-sound unverified unqualified ()
eqVerification-sound unverified scanFailed ()
eqVerification-sound unverified scanSkipped ()
eqVerification-sound unverified otherVerification ()
eqVerification-sound unverified missingVerification ()
eqVerification-sound noOp verified ()
eqVerification-sound noOp failed ()
eqVerification-sound noOp incomplete ()
eqVerification-sound noOp stillPresent ()
eqVerification-sound noOp rollbackRequested ()
eqVerification-sound noOp unverified ()
eqVerification-sound noOp noOp _ = refl
eqVerification-sound noOp unqualified ()
eqVerification-sound noOp scanFailed ()
eqVerification-sound noOp scanSkipped ()
eqVerification-sound noOp otherVerification ()
eqVerification-sound noOp missingVerification ()
eqVerification-sound unqualified verified ()
eqVerification-sound unqualified failed ()
eqVerification-sound unqualified incomplete ()
eqVerification-sound unqualified stillPresent ()
eqVerification-sound unqualified rollbackRequested ()
eqVerification-sound unqualified unverified ()
eqVerification-sound unqualified noOp ()
eqVerification-sound unqualified unqualified _ = refl
eqVerification-sound unqualified scanFailed ()
eqVerification-sound unqualified scanSkipped ()
eqVerification-sound unqualified otherVerification ()
eqVerification-sound unqualified missingVerification ()
eqVerification-sound scanFailed verified ()
eqVerification-sound scanFailed failed ()
eqVerification-sound scanFailed incomplete ()
eqVerification-sound scanFailed stillPresent ()
eqVerification-sound scanFailed rollbackRequested ()
eqVerification-sound scanFailed unverified ()
eqVerification-sound scanFailed noOp ()
eqVerification-sound scanFailed unqualified ()
eqVerification-sound scanFailed scanFailed _ = refl
eqVerification-sound scanFailed scanSkipped ()
eqVerification-sound scanFailed otherVerification ()
eqVerification-sound scanFailed missingVerification ()
eqVerification-sound scanSkipped verified ()
eqVerification-sound scanSkipped failed ()
eqVerification-sound scanSkipped incomplete ()
eqVerification-sound scanSkipped stillPresent ()
eqVerification-sound scanSkipped rollbackRequested ()
eqVerification-sound scanSkipped unverified ()
eqVerification-sound scanSkipped noOp ()
eqVerification-sound scanSkipped unqualified ()
eqVerification-sound scanSkipped scanFailed ()
eqVerification-sound scanSkipped scanSkipped _ = refl
eqVerification-sound scanSkipped otherVerification ()
eqVerification-sound scanSkipped missingVerification ()
eqVerification-sound otherVerification verified ()
eqVerification-sound otherVerification failed ()
eqVerification-sound otherVerification incomplete ()
eqVerification-sound otherVerification stillPresent ()
eqVerification-sound otherVerification rollbackRequested ()
eqVerification-sound otherVerification unverified ()
eqVerification-sound otherVerification noOp ()
eqVerification-sound otherVerification unqualified ()
eqVerification-sound otherVerification scanFailed ()
eqVerification-sound otherVerification scanSkipped ()
eqVerification-sound otherVerification otherVerification _ = refl
eqVerification-sound otherVerification missingVerification ()
eqVerification-sound missingVerification verified ()
eqVerification-sound missingVerification failed ()
eqVerification-sound missingVerification incomplete ()
eqVerification-sound missingVerification stillPresent ()
eqVerification-sound missingVerification rollbackRequested ()
eqVerification-sound missingVerification unverified ()
eqVerification-sound missingVerification noOp ()
eqVerification-sound missingVerification unqualified ()
eqVerification-sound missingVerification scanFailed ()
eqVerification-sound missingVerification scanSkipped ()
eqVerification-sound missingVerification otherVerification ()
eqVerification-sound missingVerification missingVerification _ = refl
eqBucket : Bucket → Bucket → Bool
eqBucket creditSuccess creditSuccess = true
eqBucket creditFailure creditFailure = true
eqBucket creditFalsePositive creditFalsePositive = true
eqBucket ignored ignored = true
eqBucket _ _ = false

eqBucket-sound : ∀ a b → eqBucket a b ≡ true → a ≡ b
eqBucket-sound creditSuccess creditSuccess _ = refl
eqBucket-sound creditSuccess creditFailure ()
eqBucket-sound creditSuccess creditFalsePositive ()
eqBucket-sound creditSuccess ignored ()
eqBucket-sound creditFailure creditSuccess ()
eqBucket-sound creditFailure creditFailure _ = refl
eqBucket-sound creditFailure creditFalsePositive ()
eqBucket-sound creditFailure ignored ()
eqBucket-sound creditFalsePositive creditSuccess ()
eqBucket-sound creditFalsePositive creditFailure ()
eqBucket-sound creditFalsePositive creditFalsePositive _ = refl
eqBucket-sound creditFalsePositive ignored ()
eqBucket-sound ignored creditSuccess ()
eqBucket-sound ignored creditFailure ()
eqBucket-sound ignored creditFalsePositive ()
eqBucket-sound ignored ignored _ = refl
eqStage : Stage → Stage → Bool
eqStage nascent nascent = true
eqStage adolescent adolescent = true
eqStage mature mature = true
eqStage veteran veteran = true
eqStage _ _ = false

eqStage-sound : ∀ a b → eqStage a b ≡ true → a ≡ b
eqStage-sound nascent nascent _ = refl
eqStage-sound nascent adolescent ()
eqStage-sound nascent mature ()
eqStage-sound nascent veteran ()
eqStage-sound adolescent nascent ()
eqStage-sound adolescent adolescent _ = refl
eqStage-sound adolescent mature ()
eqStage-sound adolescent veteran ()
eqStage-sound mature nascent ()
eqStage-sound mature adolescent ()
eqStage-sound mature mature _ = refl
eqStage-sound mature veteran ()
eqStage-sound veteran nascent ()
eqStage-sound veteran adolescent ()
eqStage-sound veteran mature ()
eqStage-sound veteran veteran _ = refl
eqTier : Tier → Tier → Bool
eqTier reportOnly reportOnly = true
eqTier review review = true
eqTier autoExecute autoExecute = true
eqTier _ _ = false

eqTier-sound : ∀ a b → eqTier a b ≡ true → a ≡ b
eqTier-sound reportOnly reportOnly _ = refl
eqTier-sound reportOnly review ()
eqTier-sound reportOnly autoExecute ()
eqTier-sound review reportOnly ()
eqTier-sound review review _ = refl
eqTier-sound review autoExecute ()
eqTier-sound autoExecute reportOnly ()
eqTier-sound autoExecute review ()
eqTier-sound autoExecute autoExecute _ = refl
_&&_ : Bool → Bool → Bool
true && b = b
false && _ = false

&&-true : ∀ {a b} → (a && b) ≡ true → (a ≡ true) × (b ≡ true)
&&-true {true} {b} p = refl , p
&&-true {false} {b} ()

nat-sound : ∀ a b → (a == b) ≡ true → a ≡ b
nat-sound zero zero _ = refl
nat-sound zero (suc b) ()
nat-sound (suc a) zero ()
nat-sound (suc a) (suc b) p = cong suc (nat-sound a b p)

eqFleet : FleetResult → FleetResult → Bool
eqFleet (qualifiedRow o v) (qualifiedRow o′ v′) = eqOutcome o o′ && eqVerification v v′
eqFleet (auditRow o v) (auditRow o′ v′) = eqOutcome o o′ && eqVerification v v′
eqFleet skipRow skipRow = true
eqFleet _ _ = false

cong₂ : ∀ {A B C : Set} (f : A → B → C) {a a′ b b′} → a ≡ a′ → b ≡ b′ → f a b ≡ f a′ b′
cong₂ f refl refl = refl

eqFleet-sound : ∀ a b → eqFleet a b ≡ true → a ≡ b
eqFleet-sound (qualifiedRow o v) (qualifiedRow o′ v′) p with &&-true {eqOutcome o o′} {eqVerification v v′} p
... | p₁ , p₂ = cong₂ qualifiedRow (eqOutcome-sound o o′ p₁) (eqVerification-sound v v′ p₂)
eqFleet-sound (qualifiedRow _ _) (auditRow _ _) ()
eqFleet-sound (qualifiedRow _ _) skipRow ()
eqFleet-sound (auditRow _ _) (qualifiedRow _ _) ()
eqFleet-sound (auditRow o v) (auditRow o′ v′) p with &&-true {eqOutcome o o′} {eqVerification v v′} p
... | p₁ , p₂ = cong₂ auditRow (eqOutcome-sound o o′ p₁) (eqVerification-sound v v′ p₂)
eqFleet-sound (auditRow _ _) skipRow ()
eqFleet-sound skipRow (qualifiedRow _ _) ()
eqFleet-sound skipRow (auditRow _ _) ()
eqFleet-sound skipRow skipRow _ = refl

------------------------------------------------------------------------
-- Tables: (input , what the running Elixir answered), and "the model agrees".

GateTable : Set
GateTable = List (Row × Bucket)

FleetTable : Set
FleetTable = List ((Ctx × Verdict × FOutcome × Hit × Hit × Hit) × FleetResult)

StageTable : Set
StageTable = List (Nat × Stage × Tier)

GateAgrees : GateTable → Set
GateAgrees [] = ⊤
GateAgrees ((r , e) ∷ t) = (classify r ≡ e) × GateAgrees t

FleetAgrees : FleetTable → Set
FleetAgrees [] = ⊤
FleetAgrees (((c , vd , fo , e , b , a) , res) ∷ t) = (classifyFleet c vd fo e b a ≡ res) × FleetAgrees t

StageAgrees : StageTable → Set
StageAgrees [] = ⊤
StageAgrees ((n , s , t) ∷ rest) = (stageOf n ≡ s) × (tierOf (stageOf n) ≡ t) × StageAgrees rest

checkGate : GateTable → Bool
checkGate [] = true
checkGate ((r , e) ∷ t) = eqBucket (classify r) e && checkGate t

checkFleet : FleetTable → Bool
checkFleet [] = true
checkFleet (((c , vd , fo , e , b , a) , res) ∷ t) = eqFleet (classifyFleet c vd fo e b a) res && checkFleet t

checkStage : StageTable → Bool
checkStage [] = true
checkStage ((n , s , t) ∷ rest) = (eqStage (stageOf n) s && eqTier (tierOf (stageOf n)) t) && checkStage rest

checkGate-sound : ∀ t → checkGate t ≡ true → GateAgrees t
checkGate-sound [] _ = tt
checkGate-sound ((r , e) ∷ t) p with &&-true {eqBucket (classify r) e} {checkGate t} p
... | p₁ , p₂ = eqBucket-sound (classify r) e p₁ , checkGate-sound t p₂

checkFleet-sound : ∀ t → checkFleet t ≡ true → FleetAgrees t
checkFleet-sound [] _ = tt
checkFleet-sound (((c , vd , fo , e , b , a) , res) ∷ t) p
  with &&-true {eqFleet (classifyFleet c vd fo e b a) res} {checkFleet t} p
... | p₁ , p₂ = eqFleet-sound (classifyFleet c vd fo e b a) res p₁ , checkFleet-sound t p₂

checkStage-sound : ∀ t → checkStage t ≡ true → StageAgrees t
checkStage-sound [] _ = tt
checkStage-sound ((n , s , t) ∷ rest) p with &&-true {eqStage (stageOf n) s && eqTier (tierOf (stageOf n)) t} {checkStage rest} p
... | p₁ , p₂ with &&-true {eqStage (stageOf n) s} {eqTier (tierOf (stageOf n)) t} p₁
...   | q₁ , q₂ = (eqStage-sound (stageOf n) s q₁ , eqTier-sound (tierOf (stageOf n)) t q₂ , checkStage-sound rest p₂)
