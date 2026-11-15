-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (E0-10).  scripts/check-verdicts.sh passes only if
-- Idris2 refuses this module, and only at the declaration `invalid`.  Every
-- other declaration must typecheck, so that the refusal is for the stated
-- reason and not for a missing import or a typo.
--
-- P4: a fill never overwrites a present field. Claiming that the fill replaced after_hits Z by S Z must not typecheck.
--
-- EXPECT: Mismatch between: 1 and 0.

module ReceiptP4FillOverwrites

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP4FillOverwrites.noBody (\e => absurd (ReceiptP4FillOverwrites.notVerified e))


old : Body
old AfterHits = Just Z
old _ = Nothing

extra : Body
extra AfterHits = Just (S Z)
extra _ = Nothing

invalid : fillBody ReceiptP4FillOverwrites.old ReceiptP4FillOverwrites.extra AfterHits = Just (S Z)
invalid = Refl
