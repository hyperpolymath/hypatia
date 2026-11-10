-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (secret-alerts line contract).  scripts/check-verdicts.sh
-- passes only if Idris2 refuses this module, and only at `invalid`.
--
-- Must fail: an error line names why the alerts could not be read.  An
-- empty reason does not typecheck.
--
-- EXPECT: Mismatch between: ?x :: ?xs and [].

module SecretAlertsEmptyReason

import Data.List
import Hypatia.ABI.SecretAlerts

%default total

invalid : AlertLine Error
invalid = ErrLine "acme" "demo" "" (MkNonEmptyString IsNonEmpty)
