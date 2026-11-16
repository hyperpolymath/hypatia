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
||| The model keeps an event at exactly `windowStart`. The deployed limiter
||| (`lib/safety/rate_limiter.ex`, `now - ts < window`) drops an event that
||| is exactly one window old, so the two differ at that one boundary.
public export
inWindow : (windowStart : Nat) -> Event -> Bool
inWindow windowStart e = windowStart <= e.timestamp

||| Count events at or after windowStart, including future timestamps;
||| no upper time bound is applied. The runtime does the same: it records
||| events at System.system_time/1, a wall clock that can step back, and
||| `now - ts < window` holds for a future `ts`, so it counts them too.
||| Counting them only makes the limiter stricter; the invariant below is
||| an upper bound and is unaffected.
public export
countEventsInWindow : List Event -> (windowStart : Nat) -> Nat
countEventsInWindow es windowStart = length (filter (inWindow windowStart) es)

||| Subtract the configured window size from `now`, saturating at zero.
public export
windowStart : RateLimiterState -> (now : Nat) -> Nat
windowStart state now = minus now state.config.windowSize

||| The in-window count at `now`.
public export
currentCount : RateLimiterState -> (now : Nat) -> Nat
currentCount state now = countEventsInWindow state.events (windowStart state now)

||| Accept the event only if the count at its timestamp is below the maximum.
||| Return the updated state and True on acceptance, or the original state
||| and False on rejection. Counting includes events with later timestamps.
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

||| An empty per-bot limiter satisfies the count bound at any timestamp.
export
perBotInitialValid : (now : Nat) -> InvariantHolds (MkRateLimiterState RateLimiting.perBotConfig []) now
perBotInitialValid = emptySatisfiesInvariant perBotConfig

||| An empty global limiter satisfies the count bound at any timestamp.
export
globalInitialValid : (now : Nat) -> InvariantHolds (MkRateLimiterState RateLimiting.globalConfig []) now
globalInitialValid = emptySatisfiesInvariant globalConfig

||| An empty burst limiter satisfies the count bound at any timestamp.
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

------------------------------------------------------------------------
-- Section 8: events over time (the sliding window)
------------------------------------------------------------------------

-- Sections 5 and 7 cover events that share one timestamp. This section
-- covers a sequence whose timestamps never go backwards, which is how the
-- limiter sees real traffic. The key fact is that the bound only gets
-- easier with time: a later window starts later, so it holds a subset of
-- the events an earlier window held.

||| Nat's Bool `<=` agrees with LTE, one way.
export
leqTrueToLTE : (a, b : Nat) -> (a <= b) = True -> LTE a b
leqTrueToLTE Z _ _ = LTEZero
leqTrueToLTE (S a) Z prf = absurd prf
leqTrueToLTE (S a) (S b) prf = LTESucc (leqTrueToLTE a b prf)

||| ... and the other.
export
lteToLeqTrue : (a, b : Nat) -> LTE a b -> (a <= b) = True
lteToLeqTrue Z Z _ = Refl
lteToLeqTrue Z (S _) _ = Refl
lteToLeqTrue (S a) Z prf = absurd prf
lteToLeqTrue (S a) (S b) (LTESucc prf) = lteToLeqTrue a b prf

||| An event inside a later window is inside every earlier one.
export
inWindowEarlier : (ws1, ws2 : Nat) -> (e : Event) -> LTE ws1 ws2 ->
                  inWindow ws2 e = True -> inWindow ws1 e = True
inWindowEarlier ws1 ws2 e le prf =
  lteToLeqTrue ws1 e.timestamp
    (transitive le (leqTrueToLTE ws2 e.timestamp prf))

||| A window that starts later holds at most as many events.
export
countAntitone : (es : List Event) -> (ws1, ws2 : Nat) -> LTE ws1 ws2 ->
                LTE (countEventsInWindow es ws2) (countEventsInWindow es ws1)
countAntitone [] _ _ _ = LTEZero
countAntitone (e :: es) ws1 ws2 le with (inWindow ws2 e) proof p2
  _ | True with (inWindow ws1 e) proof p1
    _ | True = LTESucc (countAntitone es ws1 ws2 le)
    _ | False = absurd (trans (sym p1) (inWindowEarlier ws1 ws2 e le p2))
  _ | False with (inWindow ws1 e)
    _ | True = lteSuccRight (countAntitone es ws1 ws2 le)
    _ | False = countAntitone es ws1 ws2 le

||| If the bound holds at one time, it holds at every later time.
export
invariantLater : (state : RateLimiterState) -> (t1, t2 : Nat) -> LTE t1 t2 ->
                 InvariantHolds state t1 -> InvariantHolds state t2
invariantLater (MkRateLimiterState cfg evs) t1 t2 le h =
  transitive (countAntitone evs (minus t1 cfg.windowSize) (minus t2 cfg.windowSize)
                            (minusLteMonotone le)) h

||| Events whose timestamps never go backwards, starting no earlier than t.
public export
data Chronological : (t : Nat) -> List Event -> Type where
  Done : Chronological t []
  Next : {e : Event} -> LTE t e.timestamp -> Chronological e.timestamp es ->
         Chronological t (e :: es)

||| The timestamp of the last event, or t when there is none.
public export
lastTime : (t : Nat) -> List Event -> Nat
lastTime t [] = t
lastTime _ (e :: es) = lastTime e.timestamp es

||| tryAccept keeps the configuration.
export
tryAcceptConfig : (state : RateLimiterState) -> (e : Event) ->
                  (fst (tryAccept state e)).config = state.config
tryAcceptConfig (MkRateLimiterState cfg evs) e
  with (isLT (countEventsInWindow evs (minus e.timestamp cfg.windowSize)) cfg.maxEvents)
  _ | Yes _ = Refl
  _ | No _ = Refl

||| Offering events in time order keeps the bound at each event's time, so
||| it holds at the time of the last one.
export
processChronological : (state : RateLimiterState) -> (es : List Event) -> (t : Nat) ->
                       Chronological t es -> InvariantHolds state t ->
                       InvariantHolds (processEvents state es) (lastTime t es)
processChronological state [] t Done h = h
processChronological state (e :: es) t (Next le rest) h =
  processChronological (fst (tryAccept state e)) es e.timestamp rest
    (tryAcceptPreservesInvariant state e (invariantLater state t e.timestamp le h))

||| The sliding-window bound. From an empty limiter, offer any events in
||| time order. At every time from the last offer onward, the accepted
||| events in the window [now - windowSize, now] number at most maxEvents.
||| Every accepted event is at or before the last offer, so the count the
||| invariant takes (events at or after the window start) is exactly the
||| window's count. Applied to each prefix, the bound holds at every point
||| of the run.
export
slidingWindowBound : (config : RateLimitConfig) -> (es : List Event) ->
                     Chronological 0 es -> (now : Nat) -> LTE (lastTime 0 es) now ->
                     InvariantHolds (processEvents (MkRateLimiterState config []) es) now
slidingWindowBound config es order now late =
  invariantLater _ (lastTime 0 es) now late
    (processChronological (MkRateLimiterState config []) es 0 order
      (emptySatisfiesInvariant config 0))
