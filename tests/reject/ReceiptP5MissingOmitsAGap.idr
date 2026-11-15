-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (E0-10).  scripts/check-verdicts.sh passes only if
-- Idris2 refuses this module, and only at the declaration `invalid`.  Every
-- other declaration must typecheck, so that the refusal is for the stated
-- reason and not for a missing import or a typo.
--
-- P5: missing_evidence is exact. A receipt whose body has gaps and whose missing_evidence is empty cannot be built.
--
-- EXPECT: Mismatch between: AfterHits :: missingOf noBody

module ReceiptP5MissingOmitsAGap

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP5MissingOmitsAGap.noBody (\e => absurd (ReceiptP5MissingOmitsAGap.notVerified e))


invalid : Receipt
invalid = MkReceipt 1 [] QFailed CFailure CreditedFailure Nothing [] ReceiptP5MissingOmitsAGap.noBody [] (Right Refl)
                    (\e => absurd (ReceiptP5MissingOmitsAGap.notVerified e)) Refl
