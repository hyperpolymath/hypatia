-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- RateLimiting.idr — the sliding-window rate limiter never exceeds its bound.
--
-- Re-homed from verification/proofs/lean4/RateLimiting.lean (PROOF-POLICY
-- section 3: interface and engineering statements live in Idris2). The
-- statements are the Lean file's, theorem for theorem; the Lean proofs rested
-- on the kernel axioms propext and Quot.sound, which Idris2 does not have.
--
-- Models lib/safety/rate_limiter.ex, which enforces
--   per-bot 50 / 60 s, global 200 / 60 s, burst 10 / 5 s.
-- Over Nat timestamps and counts. The model does not show that the Elixir
-- GenServer follows it, and the GenServer is not on any dispatch path
-- (CLAUDE.md Known Gap 2).

module RateLimiting

import Control.Relation
import Data.Nat
import Data.List
import Data.List.Quantifiers

%default total

------------------------------------------------------------------------
-- Section 1: window-based counter model
------------------------------------------------------------------------

||| A rate-limiter configuration: the most events allowed in a window.
public export
record RateLimitConfig where
  constructor MkRateLimitConfig
  maxEvents      : Nat
  windowSize     : Nat
  windowPositive : LT 0 windowSize

||| A timestamped event.
public export
record Event where
  constructor MkEvent
  timestamp : Nat

||| The limiter's state: its configuration and the events it has accepted.
public export
record RateLimiterState where
  constructor MkRateLimiterState
  config : RateLimitConfig
  events : List Event

||| Is the event inside a window that starts at `windowStart`?
public export
inWindow : (windowStart : Nat) -> Event -> Bool
inWindow windowStart e = windowStart <= e.timestamp

||| Events within [windowStart, now].
public export
countEventsInWindow : List Event -> (windowStart : Nat) -> Nat
countEventsInWindow es windowStart = length (filter (inWindow windowStart) es)

||| The start of the window that ends at `now`.
public export
windowStart : RateLimiterState -> (now : Nat) -> Nat
windowStart state now = minus now state.config.windowSize

||| The in-window count at `now`.
public export
currentCount : RateLimiterState -> (now : Nat) -> Nat
currentCount state now = countEventsInWindow state.events (windowStart state now)

||| Accept the event only if the in-window count is below the maximum.
public export
tryAccept : RateLimiterState -> Event -> (RateLimiterState, Bool)
tryAccept (MkRateLimiterState cfg evs) e
  with (isLT (countEventsInWindow evs (minus e.timestamp cfg.windowSize)) cfg.maxEvents)
  _ | Yes _ = (MkRateLimiterState cfg (e :: evs), True)
  _ | No _  = (MkRateLimiterState cfg evs, False)

||| Drop the events outside the window (garbage collection).
public export
prune : RateLimiterState -> (now : Nat) -> RateLimiterState
prune state@(MkRateLimiterState cfg evs) now = MkRateLimiterState cfg (filter (inWindow (windowStart state now)) evs)

------------------------------------------------------------------------
-- Section 2: the invariant
------------------------------------------------------------------------

||| The in-window count never exceeds maxEvents.
public export
InvariantHolds : RateLimiterState -> (now : Nat) -> Type
InvariantHolds state now = LTE (currentCount state now) state.config.maxEvents

||| An empty limiter satisfies the invariant.
export
emptySatisfiesInvariant : (config : RateLimitConfig) -> (now : Nat) ->
                          InvariantHolds (MkRateLimiterState config []) now
emptySatisfiesInvariant _ _ = LTEZero

||| Filtering never lengthens a list.
export
filterLengthLTE : (p : a -> Bool) -> (xs : List a) -> LTE (length (filter p xs)) (length xs)
filterLengthLTE p [] = LTEZero
filterLengthLTE p (x :: xs) with (p x)
  _ | True  = LTESucc (filterLengthLTE p xs)
  _ | False = lteSuccRight (filterLengthLTE p xs)

------------------------------------------------------------------------
-- Section 3: tryAccept preserves the invariant
------------------------------------------------------------------------

||| One more event at the front raises the in-window count by at most one.
export
countConsLTE : (e : Event) -> (es : List Event) -> (ws : Nat) ->
               LTE (countEventsInWindow (e :: es) ws) (S (countEventsInWindow es ws))
countConsLTE e es ws with (inWindow ws e)
  _ | True  = reflexive
  _ | False = lteSuccRight reflexive

||| After tryAccept the invariant still holds: a rejected event leaves the
||| state unchanged, and an accepted one was let in below the maximum.
export
tryAcceptPreservesInvariant : (state : RateLimiterState) -> (e : Event) ->
                              InvariantHolds state e.timestamp ->
                              InvariantHolds (fst (tryAccept state e)) e.timestamp
tryAcceptPreservesInvariant (MkRateLimiterState cfg evs) e h
  with (isLT (countEventsInWindow evs (minus e.timestamp cfg.windowSize)) cfg.maxEvents)
  _ | Yes below =
        transitive (countConsLTE e evs (minus e.timestamp cfg.windowSize)) below
  _ | No _ = h

------------------------------------------------------------------------
-- Section 4: prune preserves the invariant
------------------------------------------------------------------------

||| Pruning only removes events, so the count can only fall.
export
prunePreservesInvariant : (state : RateLimiterState) -> (now : Nat) ->
                          InvariantHolds state now ->
                          InvariantHolds (prune state now) now
prunePreservesInvariant state@(MkRateLimiterState cfg evs) now h =
  transitive (filterLengthLTE (inWindow (windowStart state now))
                              (filter (inWindow (windowStart state now)) evs)) h

------------------------------------------------------------------------
-- Section 5: a sequence of events
------------------------------------------------------------------------

||| Offer each event in turn; keep the state tryAccept returns.
public export
processEvents : RateLimiterState -> List Event -> RateLimiterState
processEvents state [] = state
processEvents state (e :: es) = processEvents (fst (tryAccept state e)) es

||| The invariant holds after any sequence of simultaneous events (the
||| worst-case burst), if it held at the start.
export
processEventsPreservesInvariant : (state : RateLimiterState) -> (es : List Event) ->
                                  (t : Nat) -> All (\e => e.timestamp = t) es ->
                                  InvariantHolds state t ->
                                  InvariantHolds (processEvents state es) t
processEventsPreservesInvariant state [] t [] h = h
processEventsPreservesInvariant state (e :: es) t (prf :: prfs) h =
  processEventsPreservesInvariant (fst (tryAccept state e)) es t prfs step
  where
    step : InvariantHolds (fst (tryAccept state e)) t
    step = rewrite sym prf in
             tryAcceptPreservesInvariant state e (rewrite prf in h)

------------------------------------------------------------------------
-- Section 6: the deployed configurations
------------------------------------------------------------------------

||| Per-bot: 50 events per 60 s.
public export
perBotConfig : RateLimitConfig
perBotConfig = MkRateLimitConfig 50 60 (LTESucc LTEZero)

||| Global: 200 events per 60 s.
public export
globalConfig : RateLimitConfig
globalConfig = MkRateLimitConfig 200 60 (LTESucc LTEZero)

||| Burst: 10 events per 5 s.
public export
burstConfig : RateLimitConfig
burstConfig = MkRateLimitConfig 10 5 (LTESucc LTEZero)

export
perBotInitialValid : (now : Nat) -> InvariantHolds (MkRateLimiterState RateLimiting.perBotConfig []) now
perBotInitialValid = emptySatisfiesInvariant perBotConfig

export
globalInitialValid : (now : Nat) -> InvariantHolds (MkRateLimiterState RateLimiting.globalConfig []) now
globalInitialValid = emptySatisfiesInvariant globalConfig

export
burstInitialValid : (now : Nat) -> InvariantHolds (MkRateLimiterState RateLimiting.burstConfig []) now
burstInitialValid = emptySatisfiesInvariant burstConfig

------------------------------------------------------------------------
-- Section 7: the rejection guarantee
------------------------------------------------------------------------

||| At the limit, the next event is rejected.
export
atLimitRejects : (state : RateLimiterState) -> (e : Event) ->
                 currentCount state e.timestamp = state.config.maxEvents ->
                 snd (tryAccept state e) = False
atLimitRejects (MkRateLimiterState cfg evs) e full
  with (isLT (countEventsInWindow evs (minus e.timestamp cfg.windowSize)) cfg.maxEvents)
  _ | Yes below = absurd (succNotLTEpred (replace {p = \m => LT (countEventsInWindow evs (minus e.timestamp cfg.windowSize)) m} (sym full) below))
  _ | No _ = Refl

||| From an empty limiter, any burst of simultaneous offers keeps the count
||| within the bound.
export
countBoundedAfterAccepts : (config : RateLimitConfig) -> (es : List Event) -> (t : Nat) ->
                           All (\e => e.timestamp = t) es ->
                           InvariantHolds (processEvents (MkRateLimiterState config []) es) t
countBoundedAfterAccepts config es t times =
  processEventsPreservesInvariant (MkRateLimiterState config []) es t times
    (emptySatisfiesInvariant config t)
