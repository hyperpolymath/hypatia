-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (secret-alerts line contract).  scripts/check-verdicts.sh
-- passes only if Idris2 refuses this module, and only at `invalid`.
--
-- Must fail: a line that could not read the alerts is Unread, never Clean.
-- A sweep must not count "could not look" as "no alerts".
--
-- EXPECT: Mismatch between: Clean and Unread.

module SecretAlertsErrorIsClean

import Data.List
import Hypatia.ABI.SecretAlerts

%default total

errLine : AlertLine Error
errLine = ErrLine "acme" "demo" "HTTP 403"
            (MkNonEmptyString IsNonEmpty)

invalid : outcome SecretAlertsErrorIsClean.errLine = Clean
invalid = Refl
