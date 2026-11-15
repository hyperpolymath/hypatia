-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (E0-10).  Counterpart of the rejections under
-- tests/reject/Receipt*.idr and others for the same property of
-- docs/design/e0-receipt.adoc, section 8.  The model covers the Idris2 types,
-- not the Elixir run time.
--
-- P4: a fill keeps a present field and fills a gap.
--

module ReceiptP4FillOnlyGaps

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP4FillOnlyGaps.noBody (\e => absurd (ReceiptP4FillOnlyGaps.notVerified e))


old : Body
old AfterHits = Just 3
old _ = Nothing

extra : Body
extra AfterHits = Just 9
extra HeadSha = Just 5
extra _ = Nothing

keeps : fillBody ReceiptP4FillOnlyGaps.old ReceiptP4FillOnlyGaps.extra AfterHits = Just 3
keeps = Refl

fills : fillBody ReceiptP4FillOnlyGaps.old ReceiptP4FillOnlyGaps.extra HeadSha = Just 5
fills = Refl

gapWasNamed : Elem HeadSha (missing ReceiptP4FillOnlyGaps.old)
gapWasNamed = snd (missingIff ReceiptP4FillOnlyGaps.old HeadSha) Refl
