-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (ADR-009 egress).  scripts/check-verdicts.sh passes only
-- if Idris2 refuses this module, and only at the declaration `invalid`.
--
-- Must fail: the job reads TOKEN, which the declaration does not list. The
-- only membership offered for it points at the declared SMTP_PASS, which is a
-- different secret, so the evidence that every read secret is declared
-- cannot be built.
--
-- EXPECT: Mismatch between: "SMTP_PASS" and "TOKEN".

module UndeclaredSecretPasses

import Data.List.Elem
import Data.List.Quantifiers
import Hypatia.ABI.Egress

%default total

invalid : Clean (MkJob ["SMTP_PASS", "TOKEN"] False (Just Block)) (Just (MkDecl ["SMTP_PASS"] None))
invalid = Declared ([Here, Here], [Here]) ClaimsNothing
