-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (E0-10).  Counterpart of the rejections under
-- tests/reject/Receipt*.idr and others for the same property of
-- docs/design/e0-receipt.adoc, section 8.  The model covers the Idris2 types,
-- not the Elixir run time.
--
-- P3: a revision appended to a ledger keeps the receipt it revises.
--

module ReceiptP3RevisionKeepsHistory

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP3RevisionKeepsHistory.noBody (\e => absurd (ReceiptP3RevisionKeepsHistory.notVerified e))


extra : Body
extra HeadSha = Just 5
extra _ = Nothing

original : Receipt
original = failedReceipt 1

revised : Receipt
revised = reviseFill ReceiptP3RevisionKeepsHistory.original 2 ReceiptP3RevisionKeepsHistory.extra

kept : append ReceiptP3RevisionKeepsHistory.revised [ReceiptP3RevisionKeepsHistory.original] = Right [ReceiptP3RevisionKeepsHistory.revised, ReceiptP3RevisionKeepsHistory.original]
kept = Refl

stillThere : Elem ReceiptP3RevisionKeepsHistory.original [ReceiptP3RevisionKeepsHistory.revised, ReceiptP3RevisionKeepsHistory.original]
stillThere = There Here

named : parents ReceiptP3RevisionKeepsHistory.revised = [eventId ReceiptP3RevisionKeepsHistory.original]
named = Refl
