-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (ADR-009 egress).  scripts/check-verdicts.sh passes only
-- if Idris2 refuses this module, and only at the declaration `invalid`.
--
-- Must fail: a hardener that only audits is offered as satisfying a
-- `harden-runner-block` claim. `Blocking` is indexed by `Just Block`, so it
-- does not unify with the job's `Just Audit` (the #975 shape, where a
-- `name: |` text was read as the hardener's input).
--
-- EXPECT: Mismatch between: Block and Audit.

module AuditPassesAsBlock

import Data.List.Elem
import Data.List.Quantifiers
import Hypatia.ABI.Egress

%default total

same : SameSecrets ["SMTP_PASS"] ["SMTP_PASS"]
same = ([Here], [Here])

invalid : Clean (MkJob ["SMTP_PASS"] False (Just Audit)) (Just (MkDecl ["SMTP_PASS"] HardenRunnerBlock))
invalid = Declared same Blocking
