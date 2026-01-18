-- SPDX-License-Identifier: PLMP-1.0-or-later
-- PipelineState.idr - Type-safe pipeline state machine using proven patterns
--
-- Integrates SafeStateMachine from proven library for CI/CD pipeline states.
-- Only valid state transitions are representable at the type level.

module PipelineState

import Proven.SafeStateMachine
import Data.Vect

%default total

-- | Pipeline execution states
public export
data PipelineState : Type where
  Pending    : PipelineState
  Queued     : PipelineState
  Running    : PipelineState
  Paused     : PipelineState
  Succeeded  : PipelineState
  Failed     : PipelineState
  Cancelled  : PipelineState
  Skipped    : PipelineState

-- | Proof that a transition is valid
public export
data ValidTransition : PipelineState -> PipelineState -> Type where
  -- From Pending
  PendingToQueued    : ValidTransition Pending Queued
  PendingToCancelled : ValidTransition Pending Cancelled
  PendingToSkipped   : ValidTransition Pending Skipped

  -- From Queued
  QueuedToRunning    : ValidTransition Queued Running
  QueuedToCancelled  : ValidTransition Queued Cancelled

  -- From Running
  RunningToPaused    : ValidTransition Running Paused
  RunningToSucceeded : ValidTransition Running Succeeded
  RunningToFailed    : ValidTransition Running Failed
  RunningToCancelled : ValidTransition Running Cancelled

  -- From Paused
  PausedToRunning    : ValidTransition Paused Running
  PausedToCancelled  : ValidTransition Paused Cancelled

-- | A pipeline with its current state tracked at the type level
public export
record Pipeline (state : PipelineState) where
  constructor MkPipeline
  pipelineId   : String
  name         : String
  repository   : String
  branch       : String
  commit       : String
  startedAt    : Maybe Integer
  completedAt  : Maybe Integer
  logs         : List String

-- | Create a new pipeline in Pending state
export
newPipeline : String -> String -> String -> String -> String -> Pipeline Pending
newPipeline pid name repo branch commit = MkPipeline pid name repo branch commit Nothing Nothing []

-- | Transition a pipeline to a new state with proof of validity
export
transition : {from, to : PipelineState}
          -> ValidTransition from to
          -> Pipeline from
          -> Pipeline to
transition _ (MkPipeline pid name repo branch commit started completed logs) =
  MkPipeline pid name repo branch commit started completed logs

-- | Queue a pending pipeline
export
queue : Pipeline Pending -> Pipeline Queued
queue = transition PendingToQueued

-- | Start a queued pipeline
export
start : Pipeline Queued -> Integer -> Pipeline Running
start (MkPipeline pid name repo branch commit _ completed logs) now =
  MkPipeline pid name repo branch commit (Just now) completed logs

-- | Pause a running pipeline
export
pause : Pipeline Running -> Pipeline Paused
pause = transition RunningToPaused

-- | Resume a paused pipeline
export
resume : Pipeline Paused -> Pipeline Running
resume = transition PausedToRunning

-- | Mark a pipeline as succeeded
export
succeed : Pipeline Running -> Integer -> Pipeline Succeeded
succeed (MkPipeline pid name repo branch commit started _ logs) now =
  MkPipeline pid name repo branch commit started (Just now) logs

-- | Mark a pipeline as failed
export
fail : Pipeline Running -> Integer -> String -> Pipeline Failed
fail (MkPipeline pid name repo branch commit started _ logs) now reason =
  MkPipeline pid name repo branch commit started (Just now) (logs ++ [reason])

-- | Cancel a pipeline from any cancellable state
export
cancelPending : Pipeline Pending -> Pipeline Cancelled
cancelPending = transition PendingToCancelled

export
cancelQueued : Pipeline Queued -> Pipeline Cancelled
cancelQueued = transition QueuedToCancelled

export
cancelRunning : Pipeline Running -> Pipeline Cancelled
cancelRunning = transition RunningToCancelled

export
cancelPaused : Pipeline Paused -> Pipeline Cancelled
cancelPaused = transition PausedToCancelled

-- | Skip a pending pipeline
export
skip : Pipeline Pending -> String -> Pipeline Skipped
skip (MkPipeline pid name repo branch commit started completed logs) reason =
  MkPipeline pid name repo branch commit started completed (logs ++ ["Skipped: " ++ reason])

-- | Check if a state is terminal (no further transitions possible)
export
isTerminal : PipelineState -> Bool
isTerminal Succeeded = True
isTerminal Failed = True
isTerminal Cancelled = True
isTerminal Skipped = True
isTerminal _ = False

-- | Check if a state is active (currently executing)
export
isActive : PipelineState -> Bool
isActive Running = True
isActive Paused = True
isActive _ = False

-- | Get string representation of state
export
Show PipelineState where
  show Pending = "pending"
  show Queued = "queued"
  show Running = "running"
  show Paused = "paused"
  show Succeeded = "succeeded"
  show Failed = "failed"
  show Cancelled = "cancelled"
  show Skipped = "skipped"

-- | Example: Type-safe pipeline workflow
-- This will NOT compile if transitions are invalid
export
exampleWorkflow : Pipeline Succeeded
exampleWorkflow =
  let p0 = newPipeline "pipe-001" "CI Build" "hyperpolymath/cicd-hyper-a" "main" "abc123"
      p1 = queue p0
      p2 = start p1 1705600000
      p3 = succeed p2 1705600300
  in p3

-- | This would NOT compile - invalid transition from Pending to Running:
-- badWorkflow : Pipeline Running
-- badWorkflow =
--   let p0 = newPipeline "pipe-001" "CI Build" "repo" "main" "abc123"
--   in transition ??? p0  -- No valid transition exists!
