---------------------------------------------------------------------------------
-- |
-- Module      :  Math.LinearEquationSolver
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  stable
--
-- (The linear equation solver library is hosted at <http://github.com/LeventErkok/linearEqSolver>.
-- Comments, bug reports, and patches are always welcome.)
--
-- Solvers for linear equations over integers and rationals. Both single solution and all
-- solution variants are supported.
---------------------------------------------------------------------------------

module Math.LinearEquationSolver (
       -- * Available SMT solvers
       z3, cvc4, Solver(..)
       -- * Solutions over Integers
    ,  solveIntegerLinearEqs
    ,  solveIntegerLinearEqsAll
       -- * Solutions over Rationals
    ,  solveRationalLinearEqs
    ,  solveRationalLinearEqsAll
    ) where

import           Data.SBV hiding (z3, cvc4)
import qualified Data.SBV as SBV (z3, cvc4)

-- | Interface to the SMT solver, via the SBV library. Unless you need to create
-- custom solvers, or tweak the existing ones, you should not need to use this type: Simply
-- use the values 'z3' and 'cvc4' to access the respective SMT solvers.
newtype Solver = Solver SMTConfig

-- | The Z3 SMT solver from Microsoft: <http://z3.codeplex.com/>.
z3 :: Solver
z3 = Solver SBV.z3

-- | The CVC4 SMT solver from New York University and the University of Iowa: <http://cvc4.cs.nyu.edu>.
cvc4 :: Solver
cvc4 = Solver SBV.cvc4

-- | Solve a system of linear integer equations. The first argument is
-- the matrix of coefficients, known as @A@, of size @mxn@. The second argument
-- is the vector of results, known as @b@, of size @mx1@. The result will be
-- either `Nothing`, if there is no solution, or @Just x@ -- such that @Ax = b@ holds.
-- (Naturally, the result @x@ will be a vector of size @nx1@ in this case.)
--
-- Here's an example call, to solve the following system of equations:
--
-- @
--     2x + 3y + 4z = 20
--     6x - 3y + 9z = -6
--     2x      +  z = 8
-- @
--
-- >>> solveIntegerLinearEqs z3 [[2, 3, 4],[6, -3, 9],[2, 0, 1]] [20, -6, 8]
-- Just [5,6,-2]
--
-- The first argument picks the SMT solver to use. Valid values are 'z3' and
-- 'cvc4'. Naturally, you should have the chosen solver installed on your system.
--
-- In case there are no solutions, we will get `Nothing`:
--
-- >>> solveIntegerLinearEqs z3 [[1], [1]] [2, 3]
-- Nothing
--
-- Note that there are no solutions to this second system as it stipulates the unknown is
-- equal to both 2 and 3. (Overspecified.)
solveIntegerLinearEqs :: Solver                -- ^ SMT Solver to use, pass one of 'z3' or 'cvc4'
                      -> [[Integer]]           -- ^ Coefficient matrix (A)
                      -> [Integer]             -- ^ Result vector (b)
                      -> IO (Maybe [Integer])  -- ^ A solution to @Ax = b@, if any
solveIntegerLinearEqs (Solver cfg) coeffs res = extractModel `fmap` satWith cfg cs
  where cs = buildConstraints "solveIntegerLinearEqs" coeffs res

-- | Similar to `solveIntegerLinearEqs`, except returns all possible solutions.
-- Note that there might be an infinite number of solutions if the system
-- is underspecified, in which case the result will be a lazy list of solutions
-- that the caller can consume as much as needed.
--
-- Here's an example call, where we underspecify the system and hence there are
-- multiple (in this case an infinite number of) solutions. Here, we only take the first 3 elements,
-- for testing purposes, but all such results can be computed lazily. Our system is:
--
-- @
--     2x + 3y + 4z = 20
--     6x - 3y + 9z = -6
-- @
--
-- We have:
--
-- >>> take 3 `fmap` solveIntegerLinearEqsAll z3 [[2, 3, 4],[6, -3, 9]] [20, -6]
-- [[5,6,-2],[-8,4,6],[18,8,-10]]
solveIntegerLinearEqsAll :: Solver          -- ^ SMT Solver to use, pass one of 'z3' or 'cvc4'
                         -> [[Integer]]     -- ^ Coefficient matrix (A)
                         -> [Integer]       -- ^ Result vector (b)
                         -> IO [[Integer]]  -- ^ All solutions to @Ax = b@
solveIntegerLinearEqsAll (Solver cfg) coeffs res = extractModels `fmap` allSatWith cfg cs
  where cs = buildConstraints "solveIntegerLinearEqsAll" coeffs res

-- | Solve a system of linear equations over rationals. Same as the integer
-- version `solveIntegerLinearEqs`, except it takes rational coefficients
-- and returns rational results.
--
-- Here's an example call, to solve the following system of equations:
--
-- @
--     2.4x + 3.6y = 12
--     7.2x - 5y   = -8.5
-- @
--
-- >>> solveRationalLinearEqs z3 [[2.4, 3.6],[7.2, -5]] [12, -8.5]
-- Just [245 % 316,445 % 158]
solveRationalLinearEqs :: Solver                  -- ^ SMT Solver to use, pass one of 'z3' or 'cvc4'
                       -> [[Rational]]            -- ^ Coefficient matrix (A)
                       -> [Rational]              -- ^ Result vector (b)
                       -> IO (Maybe [Rational])   -- ^ A solution to @Ax = b@, if any
solveRationalLinearEqs (Solver cfg) coeffs res = (fmap from . extractModel) `fmap` satWith cfg cs
  where to   = map (fromRational :: Rational -> AlgReal)
        from = map (toRational   :: AlgReal -> Rational)
        cs   = buildConstraints "solveRationalLinearEqs" (map to coeffs) (to res)

-- | Solve a system of linear equations over rationals.  Similar to `solveRationalLinearEqs`,
-- except it returns all solutions lazily.
--
-- Example system:
--
-- @
--     2.4x + 3.6y = 12
-- @
--
-- In this case, the system has infinitely many solutions. We can compute three of them as follows:
--
-- >>> take 3 `fmap` solveRationalLinearEqsAll z3 [[2.4, 3.6]] [12]
-- [[5 % 1,0 % 1],[0 % 1,10 % 3],[3 % 2,7 % 3]]
solveRationalLinearEqsAll :: Solver             -- ^ SMT Solver to use, pass one of 'z3' or 'cvc4'
                          -> [[Rational]]       -- ^ Coefficient matrix (A)
                          -> [Rational]         -- ^ Result vector (b)
                          -> IO [[Rational]]    -- ^ All solutions to @Ax = b@
solveRationalLinearEqsAll (Solver cfg) coeffs res = (map from . extractModels) `fmap` allSatWith cfg cs
  where to   = map (fromRational :: Rational -> AlgReal)
        from = map (toRational   :: AlgReal -> Rational)
        cs   = buildConstraints "solveRationalLinearEqsAll" (map to coeffs) (to res)

-- | Build the constraints as given by the coefficient matrix and the resulting vector
buildConstraints :: (Num a, SymWord a) => String -> [[a]] -> [a] -> Symbolic SBool
buildConstraints f coeffs res
  | m == 0 || any (/= n) ns || m /= length res
  = error $ f ++ ": received ill-formed input."
  | True
  = do xs <- mkFreeVars n
       let rowEq row r = sum (zipWith (*) xs row) .== r
       solve $ zipWith rowEq (map (map literal) coeffs) (map literal res)
 where m    = length coeffs
       n:ns = map length coeffs
