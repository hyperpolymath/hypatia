-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (E0-10).  Pins all 24 entries of the gate table of
-- src/Hypatia/ABI/Receipt.idr to the rule `verdict_for` of schema/receipt.ncl
-- (the one-row case of OutcomeTracker.qualified_outcome_counts/1).  The
-- theorems of the model hold of any table that never answers `Withdrawn` and
-- credits a success only for a verified, claimed success; this module is what
-- fixes the table itself.  test/receipt_model_mirror_test.exs compares the same
-- 24 entries with the Elixir run time.

module ReceiptP2GateTable

import Hypatia.ABI.Receipt

%default total
%unbound_implicits off

qverified_csuccess : gate QVerified CSuccess = CreditedSuccess
qverified_csuccess = Refl

qverified_cfailure : gate QVerified CFailure = CreditedFailure
qverified_cfailure = Refl

qverified_cfalsepositive : gate QVerified CFalsePositive = CreditedFalsePositive
qverified_cfalsepositive = Refl

qverified_cunknown : gate QVerified CUnknown = AuditOnly
qverified_cunknown = Refl

qfailed_csuccess : gate QFailed CSuccess = AuditOnly
qfailed_csuccess = Refl

qfailed_cfailure : gate QFailed CFailure = CreditedFailure
qfailed_cfailure = Refl

qfailed_cfalsepositive : gate QFailed CFalsePositive = AuditOnly
qfailed_cfalsepositive = Refl

qfailed_cunknown : gate QFailed CUnknown = AuditOnly
qfailed_cunknown = Refl

qincomplete_csuccess : gate QIncomplete CSuccess = AuditOnly
qincomplete_csuccess = Refl

qincomplete_cfailure : gate QIncomplete CFailure = CreditedFailure
qincomplete_cfailure = Refl

qincomplete_cfalsepositive : gate QIncomplete CFalsePositive = AuditOnly
qincomplete_cfalsepositive = Refl

qincomplete_cunknown : gate QIncomplete CUnknown = AuditOnly
qincomplete_cunknown = Refl

qstillpresent_csuccess : gate QStillPresent CSuccess = AuditOnly
qstillpresent_csuccess = Refl

qstillpresent_cfailure : gate QStillPresent CFailure = AuditOnly
qstillpresent_cfailure = Refl

qstillpresent_cfalsepositive : gate QStillPresent CFalsePositive = CreditedFalsePositive
qstillpresent_cfalsepositive = Refl

qstillpresent_cunknown : gate QStillPresent CUnknown = AuditOnly
qstillpresent_cunknown = Refl

qrollbackrequested_csuccess : gate QRollbackRequested CSuccess = AuditOnly
qrollbackrequested_csuccess = Refl

qrollbackrequested_cfailure : gate QRollbackRequested CFailure = AuditOnly
qrollbackrequested_cfailure = Refl

qrollbackrequested_cfalsepositive : gate QRollbackRequested CFalsePositive = CreditedFalsePositive
qrollbackrequested_cfalsepositive = Refl

qrollbackrequested_cunknown : gate QRollbackRequested CUnknown = AuditOnly
qrollbackrequested_cunknown = Refl

qunqualified_csuccess : gate QUnqualified CSuccess = AuditOnly
qunqualified_csuccess = Refl

qunqualified_cfailure : gate QUnqualified CFailure = AuditOnly
qunqualified_cfailure = Refl

qunqualified_cfalsepositive : gate QUnqualified CFalsePositive = AuditOnly
qunqualified_cfalsepositive = Refl

qunqualified_cunknown : gate QUnqualified CUnknown = AuditOnly
qunqualified_cunknown = Refl
