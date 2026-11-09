-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POSITIVE CONTROL (ADR-009 egress).  The counterpart of the three egress
-- rejections: hypatia's own push-email-notify job, declared in
-- hypatia_chora.deed with the secrets it reads and harden-runner-block, run
-- with a blocking hardener, is accepted; and the decision procedure says so
-- by evaluation. test/egress_decision_mirror_test.exs runs the Elixir rule
-- on the same decision.

module DeclaredJobIsClean

import Data.List.Elem
import Data.List.Quantifiers
import Decidable.Decidable
import Hypatia.ABI.Egress

%default total

notify : Job
notify = MkJob ["SMTP_HOST", "SMTP_PASS", "SMTP_PORT", "SMTP_USER"] False (Just Block)

declaration : Decl
declaration = MkDecl ["SMTP_HOST", "SMTP_PORT", "SMTP_USER", "SMTP_PASS"] HardenRunnerBlock

-- The order of the list does not matter: each read secret is found in the
-- declaration, and each declared one among the reads.
clean : Clean DeclaredJobIsClean.notify (Just DeclaredJobIsClean.declaration)
clean = Declared ([Here, There (There (There Here)), There Here, There (There Here)],
                  [Here, There (There Here), There (There (There Here)), There Here])
                 Blocking

-- The decision procedure agrees, by evaluation.
decides : isYes (decClean DeclaredJobIsClean.notify (Just DeclaredJobIsClean.declaration)) = True
decides = Refl

-- And it refuses the same job running its hardener in audit mode.
auditing : Job
auditing = MkJob ["SMTP_HOST", "SMTP_PASS", "SMTP_PORT", "SMTP_USER"] False (Just Audit)

refusesAudit : isYes (decClean DeclaredJobIsClean.auditing (Just DeclaredJobIsClean.declaration)) = False
refusesAudit = Refl
