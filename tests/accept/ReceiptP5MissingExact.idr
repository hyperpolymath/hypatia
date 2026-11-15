-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (E0-10).  Counterpart of the rejections under
-- tests/reject/Receipt*.idr and others for the same property of
-- docs/design/e0-receipt.adoc, section 8.  The model covers the Idris2 types,
-- not the Elixir run time.
--
-- P5: missing_evidence names exactly the absent fields, and is empty exactly when all are present.
--

module ReceiptP5MissingExact

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
failedReceipt n = credit n QFailed CFailure Nothing [] ReceiptP5MissingExact.noBody (\e => absurd (ReceiptP5MissingExact.notVerified e))


partial' : Body
partial' AfterHits = Just 1
partial' ExitCode = Just 0
partial' _ = Nothing

named : missing ReceiptP5MissingExact.partial' = [ BeforeHits, EventIdF, HeadSha, MatchKind, PatternId, Rescan
                                , ScanDigest, SubjectId, SubjectIdentity, TestDigest, ToolVersion, ObsVerdict ]
named = Refl

complete : Body
complete _ = Just 1

none : missing ReceiptP5MissingExact.complete = []
none = Refl
