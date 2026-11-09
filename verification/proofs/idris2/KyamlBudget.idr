-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia.Kyaml's node budget (`within_budget?/2`, lib/hypatia/kyaml.ex):
-- the count that refuses a document whose aliases expand past kyq's
-- NODE_BUDGET (the "billion laughs" document), modelled step for step.
--
--   count(_, n, limit) when n > limit -> n
--   count([], n, _)                   -> n
--   count([alias | rest], n, limit)   -> count([target | rest], n, limit)
--   count([map  | rest], n, limit)    -> count(values ++ rest, n + 1, limit)
--   count([seq  | rest], n, limit)    -> count(items  ++ rest, n + 1, limit)
--   count([_    | rest], n, limit)    -> count(rest, n + 1, limit)
--
--   within_budget?(node, limit) = count([node], 0, limit) <= limit
--
-- A mapping is modelled by its values (the count never visits a key), so a
-- map and a sequence are both `Coll`. An alias holds its target, so a
-- shared subtree is counted once per alias, as the Elixir code counts it.
-- The model takes an explicit number of steps (`fuel`); `Nothing` means
-- the steps ran out.
--
-- Theorems:
--
--   * `withinBudgetSound`: a total within the limit is the exact size of
--     the document with every alias read through. `within_budget?` never
--     passes a document that expands past the limit.
--   * `withinBudgetComplete`: when the count returns a total, a document
--     whose expanded size is within the limit is passed, with that size.
--     It assumes the count returned; it does not show that it does (with
--     too little fuel it returns `Nothing`).
--   * `countBounded`: when no alias names another alias (the reader refuses
--     an anchored alias, ALIAS_DECORATED), the count answers within
--     2 * (limit + 1) + 2 calls of `count/3`, however far the aliases
--     expand. That bounds the calls, the work that grows with alias
--     expansion; it does not bound the work inside a call (copying a
--     collection's children onto the stack) or allocation.
--   * `withinBudgetPasses`: the two composed. On a document with no alias
--     chain, given the calls `countBounded` allows, a document whose
--     expanded size is within the limit is passed, with that size; nothing
--     is assumed about the count returning.
--
-- This is a model of the algorithm, not a refinement proof of the Elixir
-- function. The function is held to the model by test/kyaml_test.exs and
-- test/workflow_test.exs (the anchor bomb refused at its first line).

module KyamlBudget

import public Data.Nat
import Data.Maybe

%default total

||| A node of a parsed document: a scalar, a collection (a sequence's items
||| or a mapping's values), or an alias to an earlier node.
public export
data Node = Leaf | Coll (List Node) | Alias Node

mutual
  ||| The size of a node with every alias read through.
  public export
  size : Node -> Nat
  size Leaf = 1
  size (Coll cs) = S (sizes cs)
  size (Alias t) = size t

  public export
  sizes : List Node -> Nat
  sizes [] = 0
  sizes (x :: xs) = size x + sizes xs

mutual
  ||| `count/3`, one step per call.
  public export
  count : (fuel : Nat) -> (limit : Nat) -> (stack : List Node) -> (n : Nat) -> Maybe Nat
  count Z _ _ _ = Nothing
  count (S f) limit stack n = stepCount f limit stack n (lte n limit)

  ||| One clause of `count/3`, given whether the total so far is within the limit.
  public export
  stepCount : Nat -> Nat -> List Node -> Nat -> Bool -> Maybe Nat
  stepCount f limit stack n False = Just n
  stepCount f limit [] n True = Just n
  stepCount f limit (Leaf :: r) n True = count f limit r (S n)
  stepCount f limit (Coll cs :: r) n True = count f limit (cs ++ r) (S n)
  stepCount f limit (Alias t :: r) n True = count f limit (t :: r) n

-- ─── What the count reports ────────────────────────────────────────────

justInj : Just a = Just b -> a = b
justInj Refl = Refl

lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero _ = LTEZero
lteTrans (LTESucc x) (LTESucc y) = LTESucc (lteTrans x y)

sizesAppend : (xs, ys : List Node) -> sizes (xs ++ ys) = sizes xs + sizes ys
sizesAppend [] ys = Refl
sizesAppend (x :: xs) ys =
  rewrite sizesAppend xs ys in plusAssociative (size x) (sizes xs) (sizes ys)

||| A total that failed the limit check does not fit under the limit.
overLimit : (n, limit : Nat) -> lte n limit = False -> LTE n limit -> Void
overLimit Z _ prf _ = absurd prf
overLimit (S n) Z _ bad = absurd bad
overLimit (S n) (S limit) prf (LTESucc le) = overLimit n limit prf le

||| A total within the limit is what was counted so far plus the expanded
||| size of everything still on the stack.
export
countSound : (f, limit, n : Nat) -> (stack : List Node) ->
             count f limit stack n = Just k -> LTE k limit ->
             k = n + sizes stack
countSound Z _ _ _ prf _ = absurd prf
countSound (S f) limit n stack prf within with (lte n limit) proof check
  countSound (S f) limit n [] prf within | True =
    rewrite plusZeroRightNeutral n in sym (justInj prf)
  countSound (S f) limit n (Leaf :: r) prf within | True =
    rewrite countSound f limit (S n) r prf within in
      plusSuccRightSucc n (sizes r)
  countSound (S f) limit n (Coll cs :: r) prf within | True =
    rewrite countSound f limit (S n) (cs ++ r) prf within in
    rewrite sizesAppend cs r in
      plusSuccRightSucc n (sizes cs + sizes r)
  countSound (S f) limit n (Alias t :: r) prf within | True =
    countSound f limit n (t :: r) prf within
  countSound (S f) limit n stack prf within | False =
    absurd (overLimit n limit check (rewrite justInj prf in within))

||| Whatever the count reports never exceeds the full total.
export
countAtMost : (f, limit, n : Nat) -> (stack : List Node) ->
              count f limit stack n = Just k -> LTE k (n + sizes stack)
countAtMost Z _ _ _ prf = absurd prf
countAtMost (S f) limit n stack prf with (lte n limit) proof check
  countAtMost (S f) limit n [] prf | True =
    rewrite sym (justInj prf) in lteAddRight n
  countAtMost (S f) limit n (Leaf :: r) prf | True =
    rewrite sym (plusSuccRightSucc n (sizes r)) in countAtMost f limit (S n) r prf
  countAtMost (S f) limit n (Coll cs :: r) prf | True =
    rewrite sym (plusSuccRightSucc n (sizes cs + sizes r)) in
    rewrite sym (sizesAppend cs r) in
      countAtMost f limit (S n) (cs ++ r) prf
  countAtMost (S f) limit n (Alias t :: r) prf | True =
    countAtMost f limit n (t :: r) prf
  countAtMost (S f) limit n stack prf | False =
    rewrite sym (justInj prf) in lteAddRight n

||| `within_budget?`: a total within the limit is the document's exact
||| expanded size.
export
withinBudgetSound : (f, limit : Nat) -> (doc : Node) ->
                    count f limit [doc] 0 = Just k -> LTE k limit ->
                    k = size doc
withinBudgetSound f limit doc prf within =
  rewrite countSound f limit 0 [doc] prf within in plusZeroRightNeutral (size doc)

||| `within_budget?`: if the count returns a total, a document that expands
||| to no more than the limit is passed, and its total is its size. The
||| count returning is a premise here; `withinBudgetPasses` discharges it.
export
withinBudgetComplete : (f, limit : Nat) -> (doc : Node) ->
                       LTE (size doc) limit ->
                       count f limit [doc] 0 = Just k ->
                       (LTE k limit, k = size doc)
withinBudgetComplete f limit doc small prf =
  let atMost = replace {p = LTE k} (plusZeroRightNeutral (size doc))
                 (countAtMost f limit 0 [doc] prf)
      within = lteTrans atMost small
  in (within, withinBudgetSound f limit doc prf within)

-- ─── Bounded work ──────────────────────────────────────────────────────

||| A node that is not an alias.
public export
data NotAlias : Node -> Type where
  LeafIsNot : NotAlias Leaf
  CollIsNot : NotAlias (Coll cs)

mutual
  ||| No alias names another alias, at any depth (the reader's ALIAS_DECORATED).
  public export
  data NoChain : Node -> Type where
    LeafOk  : NoChain Leaf
    CollOk  : Each cs -> NoChain (Coll cs)
    AliasOk : NotAlias t -> NoChain t -> NoChain (Alias t)

  ||| Every node of a list.
  public export
  data Each : List Node -> Type where
    Nil  : Each []
    (::) : NoChain x -> Each xs -> Each (x :: xs)

eachAppend : Each xs -> Each ys -> Each (xs ++ ys)
eachAppend [] ys = ys
eachAppend (x :: xs) ys = x :: eachAppend xs ys

||| One extra step is owed while an alias sits on top of the stack.
public export
aliasOnTop : List Node -> Nat
aliasOnTop (Alias _ :: _) = 1
aliasOnTop _ = 0

aliasOnTopBit : (s : List Node) -> LTE (aliasOnTop s) 1
aliasOnTopBit (Alias _ :: _) = LTESucc LTEZero
aliasOnTopBit [] = LTEZero
aliasOnTopBit (Leaf :: _) = LTEZero
aliasOnTopBit (Coll _ :: _) = LTEZero

notAliasOnTop : NotAlias t -> aliasOnTop (t :: r) = 0
notAliasOnTop LeafIsNot = Refl
notAliasOnTop CollIsNot = Refl

||| The steps owed with `m` counts left before the limit is passed.
public export
steps : (m : Nat) -> List Node -> Nat
steps m stack = S (m + m + aliasOnTop stack)

lteSuccFalse : (l : Nat) -> lte (S l) l = False
lteSuccFalse Z = Refl
lteSuccFalse (S l) = lteSuccFalse l

||| With no counts left, the total is past the limit.
noRoom : (n, limit : Nat) -> n + 0 = S limit -> lte n limit = True -> Void
noRoom n limit room check =
  let nIs = trans (sym (plusZeroRightNeutral n)) room
      isFalse : (lte n limit = False) = rewrite nIs in lteSuccFalse limit
  in absurd (trans (sym check) isFalse)

||| A counting step pays for itself and for an alias it may expose.
afterCount : (m, a, f : Nat) -> LTE a 1 -> LTE (S m + S m + 0) f ->
             LTE (S (m + m + a)) f
afterCount m a f bit enough =
  lteTrans (LTESucc (replace {p = LTE (m + m + a)} (plusCommutative (m + m) 1)
                         (plusLteMonotoneLeft (m + m) a 1 bit)))
    (replace {p = \x => LTE x f}
       (trans (plusZeroRightNeutral (S m + S m)) (cong S (sym (plusSuccRightSucc m m))))
       enough)

||| An alias step pays for itself and leaves no alias on top.
afterAlias : (m, f : Nat) -> LTE (m + m + 1) f -> LTE (S (m + m + 0)) f
afterAlias m f enough =
  rewrite plusZeroRightNeutral (m + m) in
    replace {p = \x => LTE x f} (plusCommutative (m + m) 1) enough

||| With `m` counts left (`n + m = S limit`), the count answers within
||| `steps m stack` steps.
countFinishes : (m, n, limit : Nat) -> n + m = S limit ->
                (stack : List Node) -> Each stack ->
                (f : Nat) -> LTE (steps m stack) f ->
                IsJust (count f limit stack n)
countFinishes m n limit room stack ok Z enough = absurd enough
countFinishes m n limit room stack ok (S f) enough with (lte n limit) proof check
  countFinishes m n limit room [] ok (S f) enough | True = ItIsJust
  countFinishes Z n limit room (_ :: _) ok (S f) enough | True =
    absurd (noRoom n limit room check)
  countFinishes (S m) n limit room (Leaf :: r) (_ :: ok) (S f) enough | True =
    countFinishes m (S n) limit (trans (plusSuccRightSucc n m) room) r ok f
      (afterCount m (aliasOnTop r) f (aliasOnTopBit r) (fromLteSucc enough))
  countFinishes (S m) n limit room (Coll cs :: r) (CollOk inner :: ok) (S f) enough | True =
    countFinishes m (S n) limit (trans (plusSuccRightSucc n m) room) (cs ++ r)
      (eachAppend inner ok) f
      (afterCount m (aliasOnTop (cs ++ r)) f (aliasOnTopBit (cs ++ r)) (fromLteSucc enough))
  countFinishes m n limit room (Alias t :: r) (AliasOk notAlias inner :: ok) (S f) enough | True =
    countFinishes m n limit room (t :: r) (inner :: ok) f
      (rewrite notAliasOnTop {r} notAlias in afterAlias m f (fromLteSucc enough))
  countFinishes m n limit room stack ok (S f) enough | False = ItIsJust

||| `within_budget?` on a document without alias chains answers within
||| 2 * (limit + 1) + 2 calls of `count/3` (units of fuel),
||| however far its aliases expand. The work inside a call is not
||| counted.
export
countBounded : (limit : Nat) -> (doc : Node) -> NoChain doc ->
               IsJust (count (S (S limit + S limit + 1)) limit [doc] 0)
countBounded limit doc ok =
  countFinishes (S limit) 0 limit Refl [doc] [ok] (S (S limit + S limit + 1))
    (LTESucc (plusLteMonotoneLeft (S limit + S limit) (aliasOnTop [doc]) 1
                (aliasOnTopBit [doc])))

-- ─── Completeness without a premise on the count ───────────────────────

isJustValue : (m : Maybe a) -> IsJust m -> (k ** m = Just k)
isJustValue (Just k) ItIsJust = (k ** Refl)

||| `within_budget?` on a document without alias chains, given the steps
||| `countBounded` allows: a document whose expanded size is within the
||| limit is passed, and its total is its size. Unlike
||| `withinBudgetComplete`, nothing is assumed about the count returning:
||| `countBounded` supplies that.
export
withinBudgetPasses : (limit : Nat) -> (doc : Node) -> NoChain doc ->
                     LTE (size doc) limit ->
                     count (S (S limit + S limit + 1)) limit [doc] 0 = Just (size doc)
withinBudgetPasses limit doc ok small with (isJustValue _ (countBounded limit doc ok))
  withinBudgetPasses limit doc ok small | (k ** prf) =
    rewrite sym (snd (withinBudgetComplete (S (S limit + S limit + 1))
                        limit doc small prf)) in prf
