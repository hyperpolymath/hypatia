-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (secret-alerts line contract).  The counterparts of
-- tests/reject/SecretAlerts*.idr: a well-formed ok line with one
-- pattern-supported finding, and a well-formed error line, are accepted;
-- they classify as WithAlerts (exit 0) and Unread (exit 3); the finding is
-- never "sound".  test/cli_test.exs ("secret-alerts") checks the Elixir
-- producer on the same shapes.

module SecretAlertsLines

import Data.List
import Data.Vect
import Hypatia.ABI.Verdict
import Hypatia.ABI.SecretAlerts

%default total

token : EvidenceToken
token = MkEvidenceToken "secret_scanning_alerts" "SSA001" "0.1.0"
          (MkSpan "github_personal_access_token" 0 0) 1

finding : SecretFinding
finding = MkSecretFinding (MkVerdict "open secret-scanning alert" [token] Pattern) Refl

okLine : AlertLine Ok
okLine = OkLine "acme" "demo" 1 [finding]

errLine : AlertLine Error
errLine = ErrLine "acme" "demo" "HTTP 404"
            (MkNonEmptyString IsNonEmpty)

okHasAlerts : outcome SecretAlertsLines.okLine = WithAlerts
okHasAlerts = Refl

okExits0 : exitOf SecretAlertsLines.okLine = 0
okExits0 = Refl

errUnread : outcome SecretAlertsLines.errLine = Unread
errUnread = Refl

errExits3 : exitOf SecretAlertsLines.errLine = 3
errExits3 = Refl

findingIsEpi : level (verdict SecretAlertsLines.finding) = LvEpi
findingIsEpi = Refl

findingNotSound : Not (level (verdict SecretAlertsLines.finding) = LvSound)
findingNotSound = findingNeverSound SecretAlertsLines.finding
