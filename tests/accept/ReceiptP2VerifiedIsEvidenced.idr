-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (E0-10).  Counterpart of the rejections under
-- tests/reject/Receipt*.idr and others for the same property of
-- docs/design/e0-receipt.adoc, section 8.  The model covers the Idris2 types,
-- not the Elixir run time.
--
-- P2: a verified receipt with a row digest and an observation is built, its verdict is the gate's, and a credited success is evidenced.
--

module ReceiptP2VerifiedIsEvidenced

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP2VerifiedIsEvidenced.noBody (\e => absurd (ReceiptP2VerifiedIsEvidenced.notVerified e))


verified : Receipt
verified = credit 1 QVerified CSuccess (Just 7) [3] ReceiptP2VerifiedIsEvidenced.noBody (\_ => (ItIsJust, IsNonEmpty))

isCredited : verdict ReceiptP2VerifiedIsEvidenced.verified = CreditedSuccess
isCredited = Refl

evidenced : (IsJust (rowDigest ReceiptP2VerifiedIsEvidenced.verified), NonEmpty (observations ReceiptP2VerifiedIsEvidenced.verified))
evidenced = creditedSuccessIsEvidenced ReceiptP2VerifiedIsEvidenced.verified Refl
