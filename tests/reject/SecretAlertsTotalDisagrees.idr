-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (secret-alerts line contract).  scripts/check-verdicts.sh
-- passes only if Idris2 refuses this module, and only at `invalid`.
--
-- Must fail: an ok line's `total` is the number of findings.  A line
-- claiming two findings while carrying one does not typecheck.
--
-- EXPECT: Mismatch between: 0 and 1.

module SecretAlertsTotalDisagrees

import Data.Vect
import Hypatia.ABI.Verdict
import Hypatia.ABI.SecretAlerts

%default total

token : EvidenceToken
token = MkEvidenceToken "secret_scanning_alerts" "SSA001" "0.1.0"
          (MkSpan "github_personal_access_token" 0 0) 1

finding : SecretFinding
finding = MkSecretFinding (MkVerdict "open secret-scanning alert" [token] Pattern) Refl

invalid : AlertLine Ok
invalid = OkLine "acme" "demo" 2 [SecretAlertsTotalDisagrees.finding]
