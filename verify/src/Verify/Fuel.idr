-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Verify/Fuel.idr - Fuel-based termination for proof verification
--
-- Integrates fuel-based termination pattern from proven-malbolge-toolchain.
-- Guarantees that verification will terminate within bounded time/steps.

module Verify.Fuel

%default total

-- | Fuel is a natural number representing remaining computation steps
public export
Fuel : Type
Fuel = Nat

-- | Result of a fuel-limited computation
public export
data FuelResult : Type -> Type where
  -- | Computation completed successfully with remaining fuel
  Complete : (result : a) -> (remaining : Fuel) -> FuelResult a
  -- | Computation ran out of fuel before completing
  Exhausted : (partialResult : Maybe a) -> (stepsUsed : Fuel) -> FuelResult a
  -- | Computation encountered an error
  FuelError : (message : String) -> FuelResult a

-- | Functor instance for FuelResult
export
Functor FuelResult where
  map f (Complete r remaining) = Complete (f r) remaining
  map f (Exhausted partial used) = Exhausted (map f partial) used
  map f (FuelError msg) = FuelError msg

-- | Applicative instance for FuelResult
export
Applicative FuelResult where
  pure x = Complete x 0
  (Complete f _) <*> (Complete x remaining) = Complete (f x) remaining
  (Complete _ _) <*> (Exhausted partial used) = Exhausted Nothing used
  (Complete _ _) <*> (FuelError msg) = FuelError msg
  (Exhausted _ used) <*> _ = Exhausted Nothing used
  (FuelError msg) <*> _ = FuelError msg

-- | Monad instance for FuelResult
export
Monad FuelResult where
  (Complete x remaining) >>= f = f x
  (Exhausted partial used) >>= f = Exhausted Nothing used
  (FuelError msg) >>= f = FuelError msg

-- | A computation that consumes fuel
public export
record FuelComputation a where
  constructor MkFuelComputation
  runWithFuel : Fuel -> FuelResult a

-- | Run a single step, consuming 1 unit of fuel
export
step : a -> FuelComputation a
step x = MkFuelComputation $ \fuel =>
  case fuel of
    Z => Exhausted (Just x) 1
    S n => Complete x n

-- | Run a step that might fail
export
stepMaybe : Maybe a -> String -> FuelComputation a
stepMaybe (Just x) _ = step x
stepMaybe Nothing msg = MkFuelComputation $ \_ => FuelError msg

-- | Consume multiple units of fuel
export
burn : (n : Nat) -> a -> FuelComputation a
burn n x = MkFuelComputation $ \fuel =>
  case isLTE n fuel of
    Yes prf => Complete x (minus fuel n)
    No _ => Exhausted (Just x) fuel

-- | Check if we have enough fuel remaining
export
checkFuel : (required : Nat) -> FuelComputation Bool
checkFuel required = MkFuelComputation $ \fuel =>
  Complete (fuel >= required) fuel

-- | Get remaining fuel without consuming any
export
remainingFuel : FuelComputation Fuel
remainingFuel = MkFuelComputation $ \fuel => Complete fuel fuel

-- | Functor for FuelComputation
export
Functor FuelComputation where
  map f (MkFuelComputation run) = MkFuelComputation $ \fuel => map f (run fuel)

-- | Applicative for FuelComputation
export
Applicative FuelComputation where
  pure x = MkFuelComputation $ \fuel => Complete x fuel
  (MkFuelComputation runF) <*> (MkFuelComputation runX) = MkFuelComputation $ \fuel =>
    case runF fuel of
      Complete f remaining => map f (runX remaining)
      Exhausted _ used => Exhausted Nothing used
      FuelError msg => FuelError msg

-- | Monad for FuelComputation
export
Monad FuelComputation where
  (MkFuelComputation run) >>= f = MkFuelComputation $ \fuel =>
    case run fuel of
      Complete x remaining => runWithFuel (f x) remaining
      Exhausted partial used => Exhausted Nothing used
      FuelError msg => FuelError msg

-- | Loop with fuel limit
export
fuelLoop : (state -> FuelComputation (Either state result))
        -> state
        -> FuelComputation result
fuelLoop body initial = MkFuelComputation $ \fuel => go initial fuel
  where
    go : state -> Fuel -> FuelResult result
    go st Z = Exhausted Nothing 0
    go st (S n) =
      case runWithFuel (body st) (S n) of
        Complete (Left newState) remaining => go newState remaining
        Complete (Right result) remaining => Complete result remaining
        Exhausted _ used => Exhausted Nothing used
        FuelError msg => FuelError msg

-- | Repeat a computation n times with fuel
export
fuelRepeat : (n : Nat) -> FuelComputation a -> FuelComputation (List a)
fuelRepeat Z _ = pure []
fuelRepeat (S k) comp = do
  x <- comp
  xs <- fuelRepeat k comp
  pure (x :: xs)

-- | Standard fuel amounts for different verification levels
export
quickVerifyFuel : Fuel
quickVerifyFuel = 1000

export
standardVerifyFuel : Fuel
standardVerifyFuel = 100000

export
exhaustiveVerifyFuel : Fuel
exhaustiveVerifyFuel = 10000000

-- | Show instance for FuelResult
export
Show a => Show (FuelResult a) where
  show (Complete result remaining) =
    "Complete(" ++ show result ++ ", " ++ show remaining ++ " fuel remaining)"
  show (Exhausted partial used) =
    "Exhausted after " ++ show used ++ " steps" ++
    maybe "" (\p => ", partial: " ++ show p) partial
  show (FuelError msg) = "FuelError: " ++ msg

-- | Example: Proof step simulation
export
record ProofStep where
  constructor MkProofStep
  hypothesis : String
  justification : String
  isValid : Bool

export
verifyProofStep : ProofStep -> FuelComputation Bool
verifyProofStep ps = do
  -- Consume fuel proportional to complexity (simplified)
  _ <- burn 10 ()
  -- In real implementation, would do actual verification
  pure (isValid ps)

export
verifyProofChain : List ProofStep -> FuelComputation Bool
verifyProofChain [] = pure True
verifyProofChain (p :: ps) = do
  valid <- verifyProofStep p
  if valid
    then verifyProofChain ps
    else pure False

-- | Run verification with timeout
export
runVerification : Fuel -> FuelComputation a -> FuelResult a
runVerification fuel comp = runWithFuel comp fuel
