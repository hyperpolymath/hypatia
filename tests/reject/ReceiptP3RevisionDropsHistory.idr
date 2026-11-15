-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (E0-10).  scripts/check-verdicts.sh passes only if
-- Idris2 refuses this module, and only at the declaration `invalid`.  Every
-- other declaration must typecheck, so that the refusal is for the stated
-- reason and not for a missing import or a typo.
--
-- P3: a revision appended to a ledger keeps every receipt of it. Claiming that the ledger after the revision is only the revision must not typecheck.
--
-- EXPECT: Mismatch between: [] and [original].

module ReceiptP3RevisionDropsHistory

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP3RevisionDropsHistory.noBody (\e => absurd (ReceiptP3RevisionDropsHistory.notVerified e))


extra : Body
extra HeadSha = Just 5
extra _ = Nothing

original : Receipt
original = failedReceipt 1

revised : Receipt
revised = reviseFill ReceiptP3RevisionDropsHistory.original 2 ReceiptP3RevisionDropsHistory.extra

invalid : append ReceiptP3RevisionDropsHistory.revised [ReceiptP3RevisionDropsHistory.original] = Right [ReceiptP3RevisionDropsHistory.revised]
invalid = Refl
