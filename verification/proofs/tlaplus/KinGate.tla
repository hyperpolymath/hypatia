-------------------------------- MODULE KinGate --------------------------------
\* SPDX-License-Identifier: PMPL-1.0-or-later
\* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
\*
\* H8: Kin-gate atomicity (mutual exclusion of bot actions on the same repo).
\* Corresponds to lib/kin/gate.ex in hypatia.
\*
\* The Kin Gate is a GenServer that brokers every gitbot-fleet action. Its
\* per-repo lock guarantees that at most one bot operates on any given repo
\* at any one time (reentrant for the same bot). This spec models the three
\* serialisable transitions the GenServer exposes -- Acquire (on :review
\* approval), Release (on :release_lock cast), and Expire (on the 30-minute
\* stale-lock cleanup timer) -- and states the safety property that any
\* interleaving of bot requests preserves mutual exclusion.
\*
\* Two observer variables complement the raw lock map:
\*   - botView: each bot's belief about which repos it holds. Two bots
\*     believing they hold the same repo would be a real bug, and it is
\*     this strong mutual-exclusion that MutualExclusionBotView asserts.
\*   - grantHistory: an append-only log of (bot, repo) grants used to
\*     state the non-trivial "no interleaved re-grant without release"
\*     safety property NoReGrantWithoutRelease.

EXTENDS Naturals, FiniteSets, Sequences, TLC

CONSTANTS
    Bots,          \* finite set of bot identifiers
    Repos,         \* finite set of repository names
    MaxRequests    \* bound on pending-request queue length (for model-checking)

ASSUME Cardinality(Bots) >= 2       \* at least two bots -- otherwise mutex is trivial
ASSUME Cardinality(Repos) >= 1
ASSUME MaxRequests \in Nat

NONE == "NONE"

VARIABLES
    locks,         \* [Repos -> Bots \cup {NONE}] -- Gate's authoritative state
    botView,       \* [Bots -> SUBSET Repos] -- each bot's current belief
    requests,      \* Seq(Bots \X Repos) -- pending review() calls
    grantHistory   \* Seq(<<Bots, Repos, STATE>>) -- audit log for safety property

vars == <<locks, botView, requests, grantHistory>>

TypeOK ==
    /\ locks \in [Repos -> Bots \cup {NONE}]
    /\ botView \in [Bots -> SUBSET Repos]
    /\ requests \in Seq(Bots \X Repos)
    /\ Len(requests) <= MaxRequests
    /\ grantHistory \in Seq(Bots \X Repos \X {"GRANT", "RELEASE", "EXPIRE"})

Init ==
    /\ locks = [r \in Repos |-> NONE]
    /\ botView = [b \in Bots |-> {}]
    /\ requests = << >>
    /\ grantHistory = << >>

--------------------------------------------------------------------------------
\* Bot submits a review() request. The Gate queues it but does not commit.
\* Modelled as pure enqueue so that Process encodes the atomic transition.
--------------------------------------------------------------------------------
Request(bot, repo) ==
    /\ Len(requests) < MaxRequests
    /\ requests' = Append(requests, <<bot, repo>>)
    /\ UNCHANGED <<locks, botView, grantHistory>>

--------------------------------------------------------------------------------
\* The Gate processes the head of the request queue. This is the one step the
\* Elixir GenServer serialises, so it is atomic here too: either the bot is
\* granted the lock (locks[repo] = NONE or already = bot) or the request is
\* deferred (dropped from the queue in this abstraction).
--------------------------------------------------------------------------------
Process ==
    /\ Len(requests) > 0
    /\ LET req  == Head(requests)
           bot  == req[1]
           repo == req[2]
       IN  \/ \* Approval: lock free or reentrant (same bot)
              /\ locks[repo] \in {NONE, bot}
              /\ locks' = [locks EXCEPT ![repo] = bot]
              /\ botView' = [botView EXCEPT ![bot] = @ \cup {repo}]
              /\ requests' = Tail(requests)
              /\ grantHistory' = Append(grantHistory, <<bot, repo, "GRANT">>)
           \/ \* Deferral: another bot holds the lock; the request drops.
              /\ locks[repo] \notin {NONE, bot}
              /\ UNCHANGED <<locks, botView>>
              /\ requests' = Tail(requests)
              /\ UNCHANGED grantHistory

--------------------------------------------------------------------------------
\* Bot finishes its action and casts release_lock.
--------------------------------------------------------------------------------
Release(bot, repo) ==
    /\ locks[repo] = bot
    /\ repo \in botView[bot]
    /\ locks' = [locks EXCEPT ![repo] = NONE]
    /\ botView' = [botView EXCEPT ![bot] = @ \ {repo}]
    /\ UNCHANGED requests
    /\ grantHistory' = Append(grantHistory, <<bot, repo, "RELEASE">>)

--------------------------------------------------------------------------------
\* The 60s cleanup_locks timer evicts a lock that has been held too long.
\* Abstracted: any held lock may be expired non-deterministically. This also
\* updates the holder's botView: after expiry the bot no longer believes it
\* holds the repo (modelling the inconsistency the Elixir code tolerates but
\* which our safety property must accommodate).
--------------------------------------------------------------------------------
Expire(repo) ==
    /\ locks[repo] /= NONE
    /\ LET b == locks[repo]
       IN  /\ locks' = [locks EXCEPT ![repo] = NONE]
           /\ botView' = [botView EXCEPT ![b] = @ \ {repo}]
           /\ UNCHANGED requests
           /\ grantHistory' = Append(grantHistory, <<b, repo, "EXPIRE">>)

Next ==
    \/ \E b \in Bots, r \in Repos: Request(b, r)
    \/ Process
    \/ \E b \in Bots, r \in Repos: Release(b, r)
    \/ \E r \in Repos: Expire(r)

Spec == Init /\ [][Next]_vars /\ WF_vars(Process)

--------------------------------------------------------------------------------
\* Safety properties
--------------------------------------------------------------------------------

\* I1. locks is a function -- at most one bot per repo by type construction.
MutualExclusionByType ==
    \A r \in Repos:
        LET v == locks[r] IN v = NONE \/ v \in Bots

\* I2. Stronger claim: two distinct bots never *both believe* they hold the
\*     same repo. This is the real mutex property, because it guarantees no
\*     bot will proceed past the Gate with a stale-belief lock.
MutualExclusionBotView ==
    \A b1, b2 \in Bots, r \in Repos:
        (r \in botView[b1] /\ r \in botView[b2]) => (b1 = b2)

\* I3. Coherence: the Gate's authoritative locks agree with the holder's view.
\*     (botView is a subset of the lock map, interpreted as a set of pairs.)
ViewCoherentWithLocks ==
    \A b \in Bots, r \in Repos:
        (r \in botView[b]) => (locks[r] = b)

\* I4. No repo shows up in any bot's view if it is unlocked in the gate.
NoOrphanView ==
    \A r \in Repos:
        (locks[r] = NONE) => (\A b \in Bots: r \notin botView[b])

\* I5. The grantHistory audit log is consistent with the invariant: between
\*     two "GRANT" entries for the same (bot, repo) there must be an
\*     intervening "RELEASE" or "EXPIRE" for that repo. (Temporal form
\*     of mutex -- useful for liveness and no-double-book-keeping.)
\*     Phrased as a safety invariant over the log tail.
GrantsAreWellBracketed ==
    \A i, j \in 1..Len(grantHistory):
        LET ei == grantHistory[i]
            ej == grantHistory[j]
        IN  (i < j /\ ei[3] = "GRANT" /\ ej[3] = "GRANT" /\ ei[2] = ej[2])
              => \E k \in (i+1)..(j-1):
                   /\ grantHistory[k][2] = ei[2]
                   /\ grantHistory[k][3] \in {"RELEASE", "EXPIRE"}

\* The composite safety invariant: all of the above at every reachable state.
KinGateSafe ==
    /\ TypeOK
    /\ MutualExclusionByType
    /\ MutualExclusionBotView
    /\ ViewCoherentWithLocks
    /\ NoOrphanView

--------------------------------------------------------------------------------
\* Liveness
--------------------------------------------------------------------------------

\* Any queued request eventually gets processed (granted or deferred).
EveryRequestProcessed ==
    \A i \in 1..MaxRequests:
        (Len(requests) >= i) ~> (Len(requests) < i)

THEOREM Safety      == Spec => []KinGateSafe
THEOREM GrantsLog   == Spec => []GrantsAreWellBracketed
THEOREM Liveness    == Spec => EveryRequestProcessed

================================================================================
