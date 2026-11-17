-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Hypatia ABI — RSR capability-profile resolution (engineering property)
--
-- A repository declares its RSR capability profile in one of two carriers:
-- a top-level `(rsr-profile …)` clause of its chora deed, or the legacy
-- `<machine tree>/rsr-profile.a2ml`.  This module is a model of how
-- lib/rules/rsr_conformance.ex resolves the two, and of the two places that
-- read the result: the scorecard's `profile_present` field and the detector
-- for criterion 3.2.2 (profile present).
--
-- Mirrored Elixir (Hypatia.Rules.RsrConformance, on main after bffd76a):
--   `declared_capabilities/1`      ~ `declaredCapabilities`
--   `deed_profile_capabilities/1`  ~ `resolve` (with `profileBodies`,
--                                    `readBodies`)
--   `clause_capabilities/1`        ~ `readClause` (with `capLists`,
--                                    `keepCaps`)
--   `a2ml_capabilities/1`          ~ `a2mlCapabilities`
--   `profile_present/0` (3.2.2)    ~ `criterion322` (= `verdictOf` of
--                                    `declaredCapabilities`)
--   `deed_clause?/2`               ~ not used by 3.2.2 since bffd76a; before
--                                    it, 3.2.2 passed on any clause, which is
--                                    the defect `deedDecidedAgrees` excludes
--   `Scorecard.profile_present`    ~ `scorecardProfilePresent`
--                                    (`caps != :none`)
--
-- What the model abstracts: a deed that is missing, unreadable or does not
-- parse as a `repo-deed` is `Nothing` (all give `:absent`); a parsed deed
-- is its list of top-level items, so a clause nested inside another one is an
-- `OtherClause` and is never read; a field key is only "capabilities or
-- not"; an a2ml carrier is missing, present but declaring no list (does not
-- parse, has no `[rsr-profile]`/`[profile]` section, or has no
-- `capabilities` list), or present with its string list.
--
-- What is proved here (Idris2, %default total, no proof-bypass primitives):
--   * deed precedence: exactly one well-formed clause decides, whatever the
--     a2ml says (`deedPrecedence`, `a2mlIgnoredUnlessAbsent`);
--   * fail-closed: two or more clauses, or one clause without a
--     capabilities list, declare nothing and fail 3.2.2, whatever the a2ml
--     says (`duplicateDeclaresNothing`, `listlessDeclaresNothing`,
--     `duplicateFailsCriterion`, `listlessFailsCriterion`);
--   * fallback: with no clause the result is the a2ml reading
--     (`fallbackNoDeed`, `fallbackNoClause`, `fallbackCriterionNoClause`);
--   * consistency: 3.2.2 passes iff the scorecard says `profile_present`,
--     for every source (`profilePresentAgrees`).  It holds by construction:
--     3.2.2 is the verdict of the same capability reader.  The bffd76a fix
--     is the deed-decided instance (`deedDecidedAgrees`).  The case that
--     diverged before the 3.2.2 fix (no deed clause; an rsr-profile.a2ml
--     that exists but yields no capabilities list, which passed 3.2.2 on its
--     presence while the scorecard reported no profile) now fails
--     (`emptyA2mlFails`).
--
-- What is NOT proved here: that the Elixir behaves as this model.  The
-- Elixir is not extracted from this module; the correspondence is by review
-- and by the ExUnit cases in test/rules/rsr_conformance_test.exs.  The
-- theorems are properties of the model, not verified code.

module Hypatia.ABI.RsrProfile

%default total
%unbound_implicits off

------------------------------------------------------------------------
-- Section 1: The two carriers
------------------------------------------------------------------------

||| A capability name.  The deed reader keeps strings and symbols alike by
||| their text, so both become a `Capability`.
public export
Capability : Type
Capability = String

||| An element of a deed list value: a string, a symbol, or anything else
||| (number, boolean, uuid, nested list), which the reader drops.
public export
data Atom = StrAtom String | SymAtom String | OtherAtom

||| A field value: a list, or anything that is not a list.
public export
data Value = ListValue (List Atom) | ScalarValue

||| A field key, as far as the reader distinguishes keys.
public export
data Key = KCapabilities | KOther

||| An item in the body of an `(rsr-profile …)` clause.
public export
data BodyItem = BodyField Key Value | BodyClause

||| A top-level item of a parsed repo deed.  An `(rsr-profile …)` clause
||| nested inside another clause is part of that `OtherClause`.
public export
data TopItem = ProfileClause (List BodyItem) | OtherClause | OtherTopField

||| The legacy `rsr-profile.a2ml`, as the two Elixir readers of it see it.
public export
data A2mlState = A2mlMissing | A2mlDeclaresNothing | A2mlDeclares (List Capability)

||| What a repository offers: its deed (`Nothing` when there is no
||| parseable repo deed) and its legacy a2ml.
public export
record ProfileSource where
  constructor MkProfileSource
  deed : Maybe (List TopItem)
  a2ml : A2mlState

------------------------------------------------------------------------
-- Section 2: Reading the deed (`deed_profile_capabilities/1`)
------------------------------------------------------------------------

||| What the deed says about the profile.
public export
data Resolution = Absent | Declared (List Capability) | DeclaresNothing

||| The text of the strings and symbols in a list; everything else dropped.
public export
keepCaps : List Atom -> List Capability
keepCaps [] = []
keepCaps (StrAtom s :: vs) = s :: keepCaps vs
keepCaps (SymAtom s :: vs) = s :: keepCaps vs
keepCaps (OtherAtom :: vs) = keepCaps vs

||| Every `:capabilities` field of a clause body whose value is a list, in
||| order.
public export
capLists : List BodyItem -> List (List Atom)
capLists [] = []
capLists (BodyField KCapabilities (ListValue vs) :: rest) = vs :: capLists rest
capLists (_ :: rest) = capLists rest

||| The first capabilities list, or nothing.
public export
firstList : List (List Atom) -> Resolution
firstList [] = DeclaresNothing
firstList (vs :: _) = Declared (keepCaps vs)

||| `clause_capabilities/1`.
public export
readClause : List BodyItem -> Resolution
readClause body = firstList (capLists body)

||| The bodies of the top-level `(rsr-profile …)` clauses, in order.
public export
profileBodies : List TopItem -> List (List BodyItem)
profileBodies [] = []
profileBodies (ProfileClause b :: rest) = b :: profileBodies rest
profileBodies (_ :: rest) = profileBodies rest

||| None: absent.  One: that clause.  Two or more: declares nothing.
public export
readBodies : List (List BodyItem) -> Resolution
readBodies [] = Absent
readBodies [b] = readClause b
readBodies (_ :: _ :: _) = DeclaresNothing

||| `deed_profile_capabilities/1`.
public export
resolve : Maybe (List TopItem) -> Resolution
resolve Nothing = Absent
resolve (Just items) = readBodies (profileBodies items)

------------------------------------------------------------------------
-- Section 3: The final capability set and its two readers
------------------------------------------------------------------------

||| The Elixir `caps`: `:none` or a list.
public export
data Caps = CapsNone | CapsList (List Capability)

||| `a2ml_capabilities/1`.
public export
a2mlCapabilities : A2mlState -> Caps
a2mlCapabilities A2mlMissing = CapsNone
a2mlCapabilities A2mlDeclaresNothing = CapsNone
a2mlCapabilities (A2mlDeclares cs) = CapsList cs

||| The `case` of `declared_capabilities/1`.
public export
fromResolution : Resolution -> A2mlState -> Caps
fromResolution Absent a = a2mlCapabilities a
fromResolution (Declared cs) _ = CapsList cs
fromResolution DeclaresNothing _ = CapsNone

||| `declared_capabilities/1`.
public export
declaredCapabilities : ProfileSource -> Caps
declaredCapabilities src = fromResolution (resolve (deed src)) (a2ml src)

||| The scorecard field: `profile_present: caps != :none`.
public export
scorecardProfilePresent : Caps -> Bool
scorecardProfilePresent CapsNone = False
scorecardProfilePresent (CapsList _) = True

||| A criterion verdict, as far as 3.2.2 produces one.
public export
data CriterionVerdict = Pass | Fail

||| Whether a verdict passes.
public export
passes : CriterionVerdict -> Bool
passes Pass = True
passes Fail = False

||| The verdict of `profile_present/0`: pass exactly when the capability
||| reader declares a profile (`declared_capabilities(repo) == :none` fails).
public export
verdictOf : Caps -> CriterionVerdict
verdictOf CapsNone = Fail
verdictOf (CapsList _) = Pass

||| Criterion 3.2.2, `profile_present/0`.
public export
criterion322 : ProfileSource -> CriterionVerdict
criterion322 src = verdictOf (declaredCapabilities src)

------------------------------------------------------------------------
-- Section 4: Lemmas on the reading steps
------------------------------------------------------------------------

public export
fromDeclared : (a : A2mlState) -> (r : Resolution) -> {cs : List Capability}
            -> r = Declared cs -> fromResolution r a = CapsList cs
fromDeclared _ _ Refl = Refl

public export
fromNothing : (a : A2mlState) -> (r : Resolution)
           -> r = DeclaresNothing -> fromResolution r a = CapsNone
fromNothing _ _ Refl = Refl

public export
fromAbsent : (a : A2mlState) -> (r : Resolution)
          -> r = Absent -> fromResolution r a = a2mlCapabilities a
fromAbsent _ _ Refl = Refl

||| One clause: the reading is that clause's.
public export
oneClause : (bs : List (List BodyItem)) -> (body : List BodyItem)
         -> bs = [body] -> readBodies bs = readClause body
oneClause _ _ Refl = Refl

||| Two or more clauses: the deed declares nothing.
public export
twoClauses : (bs : List (List BodyItem))
          -> {b1, b2 : List BodyItem} -> {rest : List (List BodyItem)}
          -> bs = b1 :: b2 :: rest -> readBodies bs = DeclaresNothing
twoClauses _ Refl = Refl

||| No clause: the deed is absent.
public export
noClause : (bs : List (List BodyItem)) -> bs = [] -> readBodies bs = Absent
noClause _ Refl = Refl

||| A non-empty list of lists reads as its first list.
public export
firstCons : (vs : List Atom) -> (more : List (List Atom)) -> (ls : List (List Atom))
         -> ls = vs :: more -> firstList ls = Declared (keepCaps vs)
firstCons _ _ _ Refl = Refl

||| An empty list of lists declares nothing.
public export
firstNil : (ls : List (List Atom)) -> ls = [] -> firstList ls = DeclaresNothing
firstNil _ Refl = Refl

||| A clause with a capabilities list declares its first list.
public export
listedClause : (body : List BodyItem) -> (vs : List Atom) -> (more : List (List Atom))
            -> capLists body = vs :: more -> readClause body = Declared (keepCaps vs)
listedClause body vs more prf = firstCons vs more (capLists body) prf

||| A clause without a capabilities list declares nothing.
public export
listlessClause : (body : List BodyItem) -> capLists body = []
              -> readClause body = DeclaresNothing
listlessClause body prf = firstNil (capLists body) prf

------------------------------------------------------------------------
-- Section 5: Deed precedence
------------------------------------------------------------------------

||| Exactly one top-level clause with a capabilities list: the deed's list
||| is the profile, for every a2ml state.
public export
deedPrecedence : (items : List TopItem) -> (a : A2mlState)
              -> (body : List BodyItem) -> profileBodies items = [body]
              -> (vs : List Atom) -> (more : List (List Atom))
              -> capLists body = vs :: more
              -> declaredCapabilities (MkProfileSource (Just items) a)
                 = CapsList (keepCaps vs)
deedPrecedence items a body one vs more listed =
  fromDeclared a (readBodies (profileBodies items))
    (trans (oneClause (profileBodies items) body one)
           (listedClause body vs more listed))

||| Whenever the deed decides (anything but `Absent`), the a2ml is not read.
public export
a2mlIgnoredUnlessAbsent : (r : Resolution) -> Not (r = Absent)
                       -> (a, b : A2mlState)
                       -> fromResolution r a = fromResolution r b
a2mlIgnoredUnlessAbsent Absent notAbsent _ _ = void (notAbsent Refl)
a2mlIgnoredUnlessAbsent (Declared _) _ _ _ = Refl
a2mlIgnoredUnlessAbsent DeclaresNothing _ _ _ = Refl

------------------------------------------------------------------------
-- Section 6: Fail-closed
------------------------------------------------------------------------

||| Two or more top-level clauses declare nothing, for every a2ml state.
public export
duplicateDeclaresNothing : (items : List TopItem) -> (a : A2mlState)
                        -> {b1, b2 : List BodyItem} -> {rest : List (List BodyItem)}
                        -> profileBodies items = b1 :: b2 :: rest
                        -> declaredCapabilities (MkProfileSource (Just items) a) = CapsNone
duplicateDeclaresNothing items a dup =
  fromNothing a (readBodies (profileBodies items))
    (twoClauses (profileBodies items) dup)

||| One clause without a capabilities list declares nothing, for every a2ml
||| state.
public export
listlessDeclaresNothing : (items : List TopItem) -> (a : A2mlState)
                       -> (body : List BodyItem) -> profileBodies items = [body]
                       -> capLists body = []
                       -> declaredCapabilities (MkProfileSource (Just items) a) = CapsNone
listlessDeclaresNothing items a body one listless =
  fromNothing a (readBodies (profileBodies items))
    (trans (oneClause (profileBodies items) body one)
           (listlessClause body listless))

||| Two or more top-level clauses fail 3.2.2, for every a2ml state.
public export
duplicateFailsCriterion : (items : List TopItem) -> (a : A2mlState)
                       -> {b1, b2 : List BodyItem} -> {rest : List (List BodyItem)}
                       -> profileBodies items = b1 :: b2 :: rest
                       -> criterion322 (MkProfileSource (Just items) a) = Fail
duplicateFailsCriterion items a dup = cong verdictOf (duplicateDeclaresNothing items a dup)

||| One clause without a capabilities list fails 3.2.2, for every a2ml
||| state.
public export
listlessFailsCriterion : (items : List TopItem) -> (a : A2mlState)
                      -> (body : List BodyItem) -> profileBodies items = [body]
                      -> capLists body = []
                      -> criterion322 (MkProfileSource (Just items) a) = Fail
listlessFailsCriterion items a body one listless =
  cong verdictOf (listlessDeclaresNothing items a body one listless)

------------------------------------------------------------------------
-- Section 7: Fallback to the a2ml
------------------------------------------------------------------------

||| No parseable deed: the profile is the a2ml reading.
public export
fallbackNoDeed : (a : A2mlState)
              -> declaredCapabilities (MkProfileSource Nothing a) = a2mlCapabilities a
fallbackNoDeed _ = Refl

||| A deed without a top-level clause: the profile is the a2ml reading.
public export
fallbackNoClause : (items : List TopItem) -> (a : A2mlState)
                -> profileBodies items = []
                -> declaredCapabilities (MkProfileSource (Just items) a) = a2mlCapabilities a
fallbackNoClause items a none =
  fromAbsent a (readBodies (profileBodies items)) (noClause (profileBodies items) none)

||| A deed without a top-level clause: 3.2.2 is the verdict of the a2ml
||| reading, so an .a2ml that exists but yields no list fails.
public export
fallbackCriterionNoClause : (items : List TopItem) -> (a : A2mlState)
                         -> profileBodies items = []
                         -> criterion322 (MkProfileSource (Just items) a) = verdictOf (a2mlCapabilities a)
fallbackCriterionNoClause items a none = cong verdictOf (fallbackNoClause items a none)

------------------------------------------------------------------------
-- Section 8: 3.2.2 and the scorecard agree
------------------------------------------------------------------------

||| The verdict passes exactly when the scorecard field is true.
public export
verdictAgrees : (c : Caps) -> passes (verdictOf c) = scorecardProfilePresent c
verdictAgrees CapsNone = Refl
verdictAgrees (CapsList _) = Refl

||| 3.2.2 passes iff the scorecard says the profile is present, for every
||| source: both read the same capability set.
public export
profilePresentAgrees : (src : ProfileSource)
                    -> passes (criterion322 src) = scorecardProfilePresent (declaredCapabilities src)
profilePresentAgrees src = verdictAgrees (declaredCapabilities src)

||| When the deed decides, 3.2.2 passes iff the scorecard says the profile
||| is present.  This is the property bffd76a restored: a duplicated or
||| list-less clause neither passes 3.2.2 nor counts as a profile.  It is
||| now an instance of `profilePresentAgrees`.
public export
deedDecidedAgrees : (src : ProfileSource) -> Not (resolve (deed src) = Absent)
                 -> passes (criterion322 src) = scorecardProfilePresent (declaredCapabilities src)
deedDecidedAgrees src _ = profilePresentAgrees src

||| The case that diverged before the 3.2.2 fix: no deed, and an
||| rsr-profile.a2ml that exists but yields no capabilities list.  It used
||| to pass 3.2.2 on the file's presence while the scorecard reported no
||| profile; it now fails 3.2.2 (tests/reject/EmptyA2mlPassesProfileCriterion
||| is the regression guard).
public export
emptyA2mlFails : criterion322 (MkProfileSource Nothing A2mlDeclaresNothing) = Fail
emptyA2mlFails = Refl
