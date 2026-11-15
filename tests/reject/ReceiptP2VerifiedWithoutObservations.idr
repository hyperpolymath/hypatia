-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (E0-10).  scripts/check-verdicts.sh passes only if
-- Idris2 refuses this module, and only at the declaration `invalid`.  Every
-- other declaration must typecheck, so that the refusal is for the stated
-- reason and not for a missing import or a typo.
--
-- P2: a verified receipt needs an observation. One with none cannot be built.
--
-- EXPECT: Mismatch between: ?x :: ?xs and [].

module ReceiptP2VerifiedWithoutObservations

import Data.List
import Data.List.Elem
import Data.Maybe
import Hypatia.ABI.Receipt

%default total
%unbound_implicits off

noBody : Body
noBody _ = Nothing

notVerified : Not (QFailed = QVerified)
notVerified Refl impossible

-- A first receipt of a failed row: not `verified`, so no evidence is demanded.
failedReceipt : Nat -> Receipt
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP2VerifiedWithoutObservations.noBody (\e => absurd (ReceiptP2VerifiedWithoutObservations.notVerified e))


invalid : Receipt
invalid = credit 1 QVerified CSuccess (Just 7) [] ReceiptP2VerifiedWithoutObservations.noBody (\_ => (ItIsJust, IsNonEmpty))
