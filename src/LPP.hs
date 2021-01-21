{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module LPP where

import GHC.Generics
import Control.Monad.State
import Data.Aeson

data Action = Maximize | Minimize deriving (Show, Eq, Generic)
data Restriction = Unstricted | Positive | Negative deriving (Show, Generic)
data Sign = Lte | Gte | Eq deriving (Show, Generic) -- TODO: add other two: lt and gt
data Feasibility = Feasible | InFeasible deriving (Eq, Show, Generic)
data Optimality = Optimal | NonOptimal deriving (Eq, Show, Generic)
type Status = (Feasibility, Optimality)
data Constraint = Constraint
    {
        lhs :: [Rational]
      , sign:: Sign
      , rhs :: Rational
    } deriving (Show, Generic)

type Solution = (Status, [(String, Rational)], Rational)

data LPP = LPP
    {
      decisionVariables :: [String]
    , objectiveAction :: Action
    , objectiveFunction :: [Rational]
    , constraints :: [Constraint]
    , signRestrictions :: [Restriction]
    } deriving (Show, Generic)

instance ToJSON Restriction
instance FromJSON Restriction

instance ToJSON Sign
instance FromJSON Sign

instance ToJSON Constraint
instance FromJSON Constraint

instance ToJSON Action
instance FromJSON Action

instance ToJSON LPP
instance FromJSON LPP

data LPPState = LPPState
    {
        currentMatrix :: [[Rational]]
      , currentZRow :: [Rational]
      , action :: Action
      , basicVariables :: [String]
      , allVariables :: [String]
      , status :: Status
      , zValue :: Rational
    } deriving (Show, Generic)

instance ToJSON Optimality
instance FromJSON Optimality

instance ToJSON Feasibility
instance FromJSON Feasibility

instance ToJSON LPPState
instance FromJSON LPPState



getMostAndIndex :: [a] -> (a -> a -> Bool) -> (a, Int)
getMostAndIndex [] _ = error "Empty list"
getMostAndIndex (x: xs) cmpF = foldr (\(e, i) (a, i') -> if cmpF e a == True then (e, i) else (a, i')) (x, 0) (zip xs [1..])

minExceptLast :: [Rational] -> (Rational, Int)
minExceptLast [] = error "Empty list"
minExceptLast (_:[]) = error "Only one element"
minExceptLast xs = getMostAndIndex (take (length xs - 1) xs) (<)

maxExceptLast :: [Rational] -> (Rational, Int)
maxExceptLast [] = error "Empty list"
maxExceptLast (_:[]) = error "Only one element"
maxExceptLast (_: xs) = getMostAndIndex (take (length xs - 1) xs) (>)

insertAt :: Int -> [a] -> a -> [a]
insertAt n xs x = take (n) xs ++ [x] ++ drop (n+1) xs


getCol :: [[a]] -> Int -> [a]
getCol mat ind = map (!!ind) mat

rowWithLeastRatio :: [[Rational]] -> Int -> Int
rowWithLeastRatio mat colInd = let solCol = getCol mat (length (mat !! 0) - 1)
                                   pivotCol = getCol mat colInd
                                   (_, ind) = getMostAndIndex (zip solCol pivotCol) compareFunction
                                   in ind
                              where compareFunction (s, p) (s', p') = case (p <= 0, p' <= 0) of
                                                            (True, True) -> True
                                                            (False, True) -> True
                                                            (True, False) -> False
                                                            (False, False) -> s/p < s'/p'

getPivot :: LPPState -> (Int, Int)
getPivot (LPPState {..})
  | action == Maximize = let (_, colIndex) = minExceptLast currentZRow in (colIndex, rowWithLeastRatio currentMatrix colIndex)
  | action == Minimize = let (_, colIndex) = maxExceptLast currentZRow in (colIndex, rowWithLeastRatio currentMatrix colIndex)


rowOperation :: Int -> [Rational] -> [Rational] -> [Rational]
rowOperation col pivotRow row = zipWith func pivotRow row
    where func p r = r - (rElem * p / pivElem)
          pivElem = pivotRow !! col
          rElem = row !! col

scaleRow :: [Rational] -> Rational -> [Rational]
scaleRow row f = map (*f) row

getInitialState :: LPP -> LPPState
getInitialState (LPP {..}) = LPPState constraintsRows negatedZ objectiveAction basicVariables allVars stat zVal
    where constraintsCount = length constraints
          negatedZ = map (*(-1)) $ objectiveFunction ++ map (const 0) constraints ++ [0] -- negated Objective coeffs + slack vars + solution which is 0 initially
          constraintsRows = map constraintToRow (zip [0..] constraints)
          constraintToRow (i, Constraint a s b) = a ++ (map (\x -> if x == i then 1 else 0) [0..constraintsCount-1]) ++ [b]
          basicVariables = map (\(i, _) -> "s"++(show i)) (zip [1..] constraints)
          allVars = decisionVariables ++ basicVariables
          zVal = 0
          stat = case objectiveAction of  -- TODO: check for infeasibililty
                     Maximize -> if all (>=0) negatedZ then (Feasible, Optimal) else (Feasible, NonOptimal)
                     Minimize -> if all (<=0) negatedZ then (Feasible, Optimal) else (Feasible, NonOptimal)


next :: State LPPState Solution
next = state $ \lppState -> let (col, row) = getPivot lppState
                                entering = allVariables lppState !! col
                                basicVars = basicVariables lppState
                                newBasicVars = insertAt row basicVars entering
                                pivotRow = currentMatrix lppState !! row
                                pivotElement = pivotRow !! col
                                currZRow = currentZRow lppState
                                newZRow = rowOperation col pivotRow currZRow
                                currMatrix = currentMatrix lppState
                                newMatrixRaw = map (rowOperation col pivotRow) currMatrix -- this contains zero for pivot row
                                newMatrix = insertAt row newMatrixRaw (scaleRow pivotRow (1/pivotElement))
                                newStatus = case action lppState of  -- TODO: check for infeasibililty
                                     Maximize -> if all (>=0) newZRow then (Feasible, Optimal) else (Feasible, NonOptimal)
                                     Minimize -> if all (<=0) newZRow then (Feasible, Optimal) else (Feasible, NonOptimal)
                                newZVal = newZRow !! (length newZRow -1)
                            in ((newStatus, zip newBasicVars (getCol newMatrix (length pivotRow - 1)), newZVal),
                                lppState {
                                        basicVariables = newBasicVars
                                      , currentZRow = newZRow
                                      , currentMatrix = newMatrix
                                      , status = newStatus
                                      , zValue = newZVal
                                }
                            )

solveLPP :: State LPPState Solution
solveLPP = do
    (status, soln, z) <- next
    if status == (Feasible, Optimal)
       then return (status, soln, z)
       else solveLPP

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = x: if p x then takeWhile' p xs else []

accumulateStates :: LPPState -> [LPPState]
accumulateStates s = takeWhile' ((/= (Feasible, Optimal)) . status) $ scanl (\a _ -> execState next a) s [1..20] -- TODO: fix this, max iter 20
