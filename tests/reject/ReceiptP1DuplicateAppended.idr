-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (E0-10).  scripts/check-verdicts.sh passes only if
-- Idris2 refuses this module, and only at the declaration `invalid`.  Every
-- other declaration must typecheck, so that the refusal is for the stated
-- reason and not for a missing import or a typo.
--
-- P1: a ledger that already holds event_id 1 refuses a second receipt with it. Claiming that the append succeeds must not typecheck.
--
-- EXPECT: Mismatch between: Right [r1, r1] and Left DuplicateEventId.

module ReceiptP1DuplicateAppended

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP1DuplicateAppended.noBody (\e => absurd (ReceiptP1DuplicateAppended.notVerified e))


r1 : Receipt
r1 = failedReceipt 1

invalid : append ReceiptP1DuplicateAppended.r1 [ReceiptP1DuplicateAppended.r1] = Right [ReceiptP1DuplicateAppended.r1, ReceiptP1DuplicateAppended.r1]
invalid = Refl
