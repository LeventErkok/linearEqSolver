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
       -- $solverInfo
       Solver(..)
       -- * Solutions over Integers
    ,  solveIntegerLinearEqs
    ,  solveIntegerLinearEqsAll
       -- * Solutions over Rationals
    ,  solveRationalLinearEqs
    ,  solveRationalLinearEqsAll
    ) where

import Data.SBV

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
-- >>> solveIntegerLinearEqs Z3 [[2, 3, 4],[6, -3, 9],[2, 0, 1]] [20, -6, 8]
-- Just [5,6,-2]
--
-- The first argument picks the SMT solver to use. Valid values are 'z3' and
-- 'cvc4'. Naturally, you should have the chosen solver installed on your system.
--
-- In case there are no solutions, we will get `Nothing`:
--
-- >>> solveIntegerLinearEqs Z3 [[1], [1]] [2, 3]
-- Nothing
--
-- Note that there are no solutions to this second system as it stipulates the unknown is
-- equal to both 2 and 3. (Overspecified.)
solveIntegerLinearEqs :: Solver                -- ^ SMT Solver to use
                      -> [[Integer]]           -- ^ Coefficient matrix (A)
                      -> [Integer]             -- ^ Result vector (b)
                      -> IO (Maybe [Integer])  -- ^ A solution to @Ax = b@, if any
solveIntegerLinearEqs cfg coeffs res = extractModel `fmap` satWith (defaultSolverConfig cfg) cs
  where cs = buildConstraints "solveIntegerLinearEqs" coeffs res

-- | Similar to `solveIntegerLinearEqs`, except in case the system has an infinite
-- number of solutions, then it will return the number of solutions requested. (Note
-- that if the system is underspecified, then there are an infinite number of
-- solutions.) So, the result can be empty, a singleton, or precisely the number requested, last of
-- which indicates there are an infinite number of solutions.
--
-- Here's an example call, where we underspecify the system and hence there are
-- multiple (in this case an infinite number of) solutions. Here, we ask for the first
-- 3 elements for testing purposes.
--
-- @
--     2x + 3y + 4z = 20
--     6x - 3y + 9z = -6
-- @
--
-- We have:
--
-- >>> solveIntegerLinearEqsAll Z3 3 [[2, 3, 4],[6, -3, 9]] [20, -6]
-- [[-8,4,6],[-21,2,14],[-34,0,22]]
--
-- The solutions you get might differ, depending on what the solver returns. (Though they'll be correct!)
solveIntegerLinearEqsAll :: Solver          -- ^ SMT Solver to use
                          -> Int                -- ^ Maximum number of solutions to return, in case infinite
                         -> [[Integer]]     -- ^ Coefficient matrix (A)
                         -> [Integer]       -- ^ Result vector (b)
                         -> IO [[Integer]]  -- ^ All solutions to @Ax = b@
solveIntegerLinearEqsAll s maxNo coeffs res = extractModels `fmap` allSatWith cfg cs
  where cs = buildConstraints "solveIntegerLinearEqsAll" coeffs res
        cfg = (defaultSolverConfig s) {allSatMaxModelCount = Just maxNo}

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
-- >>> solveRationalLinearEqs Z3 [[2.4, 3.6],[7.2, -5]] [12, -8.5]
-- Just [245 % 316,445 % 158]
solveRationalLinearEqs :: Solver                  -- ^ SMT Solver to use
                       -> [[Rational]]            -- ^ Coefficient matrix (A)
                       -> [Rational]              -- ^ Result vector (b)
                       -> IO (Maybe [Rational])   -- ^ A solution to @Ax = b@, if any
solveRationalLinearEqs cfg coeffs res = (fmap from . extractModel) `fmap` satWith (defaultSolverConfig cfg) cs
  where to   = map (fromRational :: Rational -> AlgReal)
        from = map (toRational   :: AlgReal -> Rational)
        cs   = buildConstraints "solveRationalLinearEqs" (map to coeffs) (to res)

-- | Solve a system of linear equations over rationals.  Similar to `solveRationalLinearEqs`,
-- except if the system is underspecified, then returns the number of solutions requested.
--
-- Example system:
--
-- @
--     2.4x + 3.6y = 12
-- @
--
-- In this case, the system has infinitely many solutions. We can compute three of them as follows:
--
-- >>> solveRationalLinearEqsAll Z3 3 [[2.4, 3.6]] [12]
-- [[5 % 1,0 % 1],[13 % 2,(-1) % 1],[8 % 1,(-2) % 1]]
--
-- The solutions you get might differ, depending on what the solver returns. (Though they'll be correct!)
solveRationalLinearEqsAll :: Solver             -- ^ SMT Solver to use
                          -> Int                -- ^ Maximum number of solutions to return, in case infinite
                          -> [[Rational]]       -- ^ Coefficient matrix (A)
                          -> [Rational]         -- ^ Result vector (b)
                          -> IO [[Rational]]    -- ^ All solutions to @Ax = b@
solveRationalLinearEqsAll s maxNo coeffs res = (map from . extractModels) `fmap` allSatWith cfg cs
  where to   = map (fromRational :: Rational -> AlgReal)
        from = map (toRational   :: AlgReal -> Rational)
        cs   = buildConstraints "solveRationalLinearEqsAll" (map to coeffs) (to res)
        cfg  = (defaultSolverConfig s) {allSatMaxModelCount = Just maxNo}

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

{- $solverInfo
Note that while we allow all SMT-solvers supported by SBV to be used, not all will work. In particular,
the backend solver will need to understand unbounded integers and rationals. Currently, the following
solvers provide the required capability: 'Z3', 'CVC4', and 'MathSAT'. Passing other instances will result
in an "unsupported" error, though this can of course change as the SBV package itself evolves.
-}
