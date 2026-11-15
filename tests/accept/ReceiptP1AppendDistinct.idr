-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (E0-10).  Counterpart of the rejections under
-- tests/reject/Receipt*.idr and others for the same property of
-- docs/design/e0-receipt.adoc, section 8.  The model covers the Idris2 types,
-- not the Elixir run time.
--
-- P1: appending a receipt with a new event_id to a ledger succeeds and the ids stay distinct.
--

module ReceiptP1AppendDistinct

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP1AppendDistinct.noBody (\e => absurd (ReceiptP1AppendDistinct.notVerified e))


r1 : Receipt
r1 = failedReceipt 1

r2 : Receipt
r2 = failedReceipt 2

appended : append ReceiptP1AppendDistinct.r2 [ReceiptP1AppendDistinct.r1] = Right [ReceiptP1AppendDistinct.r2, ReceiptP1AppendDistinct.r1]
appended = Refl

refused : append ReceiptP1AppendDistinct.r1 [ReceiptP1AppendDistinct.r1] = Left DuplicateEventId
refused = Refl
