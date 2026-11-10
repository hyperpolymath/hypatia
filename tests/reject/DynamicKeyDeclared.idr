-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- EXPECTED REJECTION (ADR-009 egress).  scripts/check-verdicts.sh passes only
-- if Idris2 refuses this module, and only at the declaration `invalid`.
--
-- Must fail: the job computes a secret's key at run time (`secrets[env.X]`),
-- which no fixed list can name, yet a declaration is offered as covering it.
-- `Declared` is indexed by a job whose `dynamic` is `False`.
--
-- EXPECT: Mismatch between: False and True.

module DynamicKeyDeclared

import Data.List.Elem
import Data.List.Quantifiers
import Hypatia.ABI.Egress

%default total

same : SameSecrets ["DEPLOY_KEY"] ["DEPLOY_KEY"]
same = ([Here], [Here])

invalid : Clean (MkJob ["DEPLOY_KEY"] True (Just Block)) (Just (MkDecl ["DEPLOY_KEY"] None))
invalid = Declared same ClaimsNothing
