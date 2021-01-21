import Control.Monad.State.Lazy
import Data.Ratio
import Test.Hspec

import LPP

simpleMaximization :: LPP
simpleMaximization = LPP
    {
        decisionVariables = ["x1", "x2", "x3", "x4"]
      , objectiveAction = Maximize
      , objectiveFunction = [7.7, 10.4, 5.2, 12.3]
      , constraints = [
            Constraint [1,1,1,1] Lte 120000
          , Constraint [0, 0, 1, 1] Lte 40000
          , Constraint [1, 0, 1, 0] Lte 80000
          , Constraint [0, 1, 0, 1] Lte 50000
         ]
          , signRestrictions = map (const Positive) [1..4]
    }

simpleMaximizationStatus = (Feasible,Optimal)
simpleMaximizationZValue = 1135000
simpleMaximizationBasicVars = ["x1","x4","s3","x2"]
simpleMaximizationSolutions = [("x1",70000 % 1),("x4",40000 % 1),("s3",10000 % 1),("x2",10000 % 1)]


main :: IO ()
main = hspec $ do
    describe "Testing LPP solutions" $ do
        it "solves 'simpleMaximization': maximization problem with initial feasible but unoptimal conditions" $ do
            let lppState = getInitialState simpleMaximization
                solved = execState solveLPP lppState
                matrix = currentMatrix solved
                l = length $ matrix !! 0
            status solved `shouldBe` simpleMaximizationStatus
            zValue solved `shouldBe` simpleMaximizationZValue
            basicVariables solved `shouldBe` simpleMaximizationBasicVars
            getCol matrix (l-1) `shouldBe` map snd simpleMaximizationSolutions
