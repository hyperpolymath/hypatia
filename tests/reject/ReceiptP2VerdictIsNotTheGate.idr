-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (E0-10).  scripts/check-verdicts.sh passes only if
-- Idris2 refuses this module, and only at the declaration `invalid`.  Every
-- other declaration must typecheck, so that the refusal is for the stated
-- reason and not for a missing import or a typo.
--
-- P2: the verdict of a receipt is the gate's answer. A still_present row claimed as a success is audit_only; a receipt that credits it cannot be built.
--
-- EXPECT: Mismatch between: AuditOnly and CreditedSuccess.

module ReceiptP2VerdictIsNotTheGate

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP2VerdictIsNotTheGate.noBody (\e => absurd (ReceiptP2VerdictIsNotTheGate.notVerified e))


stillPresentNotVerified : Not (QStillPresent = QVerified)
stillPresentNotVerified Refl impossible

invalid : Receipt
invalid = MkReceipt 1 [] QStillPresent CSuccess CreditedSuccess Nothing [] ReceiptP2VerdictIsNotTheGate.noBody
                    (missing ReceiptP2VerdictIsNotTheGate.noBody) (Right Refl)
                    (\e => absurd (ReceiptP2VerdictIsNotTheGate.stillPresentNotVerified e)) Refl
