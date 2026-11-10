-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (RSR capability-profile resolution).  The cases of
-- test/rules/rsr_conformance_test.exs that exercise the profile reader,
-- stated against the model in src/Hypatia/ABI/RsrProfile.idr and accepted
-- by evaluation, plus the module's theorems applied to them.
-- scripts/check-verdicts.sh requires Idris2 to ACCEPT this module; the
-- RSR-profile cases in tests/reject/ are the other half.

module RsrProfileResolution

import Hypatia.ABI.RsrProfile

%default total
%unbound_implicits off

-- `(rsr-profile :version "1.0.0" :capabilities ("container" rust))`, after the
-- deed's own top-level fields.
deedEra : List TopItem
deedEra =
  [ OtherTopField, OtherTopField
  , ProfileClause [ BodyField KOther ScalarValue
                  , BodyField KCapabilities (ListValue [StrAtom "container", SymAtom "rust"]) ]
  ]

-- Strings and symbols alike, in order.
deedEraDeclares : declaredCapabilities (MkProfileSource (Just RsrProfileResolution.deedEra) A2mlMissing)
                  = CapsList ["container", "rust"]
deedEraDeclares = Refl

bashBody : List BodyItem
bashBody = [BodyField KOther ScalarValue, BodyField KCapabilities (ListValue [StrAtom "bash"])]

bashDeed : List TopItem
bashDeed = [OtherTopField, ProfileClause RsrProfileResolution.bashBody]

-- The deed wins over a legacy .a2ml beside it.
deedWins : declaredCapabilities (MkProfileSource (Just RsrProfileResolution.bashDeed) (A2mlDeclares ["container"]))
           = CapsList ["bash"]
deedWins = deedPrecedence bashDeed (A2mlDeclares ["container"]) bashBody Refl [StrAtom "bash"] [] Refl

-- Two clauses declare nothing, and the .a2ml is not consulted.
dupDeed : List TopItem
dupDeed =
  [ ProfileClause [BodyField KCapabilities (ListValue [StrAtom "bash"])]
  , ProfileClause [BodyField KCapabilities (ListValue [StrAtom "rust"])]
  ]

dupDeclaresNothing : declaredCapabilities (MkProfileSource (Just RsrProfileResolution.dupDeed) (A2mlDeclares ["bash"]))
                     = CapsNone
dupDeclaresNothing = duplicateDeclaresNothing dupDeed (A2mlDeclares ["bash"]) Refl

dupFails : criterion322 (MkProfileSource (Just RsrProfileResolution.dupDeed) (A2mlDeclares ["bash"])) = Fail
dupFails = duplicateFailsCriterion dupDeed (A2mlDeclares ["bash"]) Refl

-- `(rsr-profile :version "1.0.0")`: one clause, no list.
listlessBody : List BodyItem
listlessBody = [BodyField KOther ScalarValue]

listlessDeed : List TopItem
listlessDeed = [ProfileClause RsrProfileResolution.listlessBody]

listlessNothing : declaredCapabilities (MkProfileSource (Just RsrProfileResolution.listlessDeed) (A2mlDeclares ["bash"]))
                  = CapsNone
listlessNothing = listlessDeclaresNothing listlessDeed (A2mlDeclares ["bash"]) listlessBody Refl Refl

listlessFails : criterion322 (MkProfileSource (Just RsrProfileResolution.listlessDeed) (A2mlDeclares ["bash"])) = Fail
listlessFails = listlessFailsCriterion listlessDeed (A2mlDeclares ["bash"]) listlessBody Refl Refl

-- A capabilities field that is not a list is skipped; a later list is read.
scalarThenList : declaredCapabilities
                   (MkProfileSource
                     (Just [ProfileClause [ BodyField KCapabilities ScalarValue
                                          , BodyField KCapabilities (ListValue [StrAtom "a"]) ]])
                     A2mlMissing)
                 = CapsList ["a"]
scalarThenList = Refl

-- An empty list is a declared (empty) profile, not `:none`.
emptyListDeclared : scorecardProfilePresent
                      (declaredCapabilities
                        (MkProfileSource (Just [ProfileClause [BodyField KCapabilities (ListValue [])]])
                                         A2mlMissing))
                    = True
emptyListDeclared = Refl

-- `(notes (rsr-profile :capabilities ("container")))`: a nested clause is not
-- the profile, so the reader falls back to the (missing) .a2ml.
nestedDeed : List TopItem
nestedDeed = [OtherTopField, OtherClause]

nestedFallsBack : declaredCapabilities (MkProfileSource (Just RsrProfileResolution.nestedDeed) A2mlMissing)
                  = CapsNone
nestedFallsBack = fallbackNoClause nestedDeed A2mlMissing Refl

-- The legacy carrier on its own.
a2mlOnly : declaredCapabilities (MkProfileSource Nothing (A2mlDeclares ["container", "rust"]))
           = CapsList ["container", "rust"]
a2mlOnly = fallbackNoDeed (A2mlDeclares ["container", "rust"])

-- Empty repo: no deed, no .a2ml; 3.2.2 fails and no profile is present.
emptyRepoFails : criterion322 (MkProfileSource Nothing A2mlMissing) = Fail
emptyRepoFails = Refl

-- 3.2.2 agrees with the scorecard on the cases bffd76a fixed.
nothingIsNotAbsent : Not (DeclaresNothing = Absent)
nothingIsNotAbsent Refl impossible

dupAgrees : passes (criterion322 (MkProfileSource (Just RsrProfileResolution.dupDeed) (A2mlDeclares ["bash"])))
            = scorecardProfilePresent
                (declaredCapabilities (MkProfileSource (Just RsrProfileResolution.dupDeed) (A2mlDeclares ["bash"])))
dupAgrees = deedDecidedAgrees (MkProfileSource (Just dupDeed) (A2mlDeclares ["bash"])) nothingIsNotAbsent

-- The residual case: no deed clause and an .a2ml that exists but yields no
-- capabilities list.  3.2.2 passes on the file's presence; the scorecard
-- reports no profile.
residualPasses : criterion322 (MkProfileSource Nothing A2mlDeclaresNothing) = Pass
residualPasses = fst residualDivergence

residualNotPresent : scorecardProfilePresent (declaredCapabilities (MkProfileSource Nothing A2mlDeclaresNothing))
                     = False
residualNotPresent = snd residualDivergence
