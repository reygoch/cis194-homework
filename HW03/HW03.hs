module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = Plus | Minus | Times | Divide | Gt | Ge | Lt | Le | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------
-- This is really cool trick for simulating variables with functions
extend :: State -> String -> Int -> State
extend state var val = state'
  where
    state' :: State
    state' key
      | key == var = val
      | otherwise = state key

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
-- Simple stuff
evalE _ (Val num) = num
evalE state (Var name) = state name
-- Binary operators
evalE state (Op exp1 Plus exp2) = evalE state exp1 + evalE state exp2
evalE state (Op exp1 Minus exp2) = evalE state exp1 - evalE state exp2

evalE state (Op exp1 Times exp2) = evalE state exp1 * evalE state exp2
evalE state (Op exp1 Divide exp2) = evalE state exp1 `div` evalE state exp2
-- Comparison
evalE state (Op exp1 Gt exp2) = if evalE state exp1 > evalE state exp2 then 1 else 0
evalE state (Op exp1 Ge exp2) = if evalE state exp1 >= evalE state exp2 then 1 else 0
evalE state (Op exp1 Lt exp2) = if evalE state exp1 < evalE state exp2 then 1 else 0
evalE state (Op exp1 Le exp2) = if evalE state exp1 <= evalE state exp2 then 1 else 0
evalE state (Op exp1 Eql exp2) = if evalE state exp1 == evalE state exp2 then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement =
    DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var expr) = DAssign var expr
desugar (Incr var) = DAssign var (Op (Var var) Plus (Val 1))
desugar (If expr st1 st2) = DIf expr (desugar st1) (desugar st2)
desugar (While expr st1) = DWhile expr (desugar st1)
desugar (For st0 expr st1 st2) = desugar $ Sequence st0 $ While expr $ Sequence st1 st2
-- DWhile expr $ desugar $ Sequence st0 $ Sequence st1 st2
desugar (Sequence st1 st2) = DSequence (desugar st1) (desugar st2)
desugar (Skip) = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign name expr) = extend state name $ evalE state expr
evalSimple state (DIf expr st1 st2) =
  if evalE state expr == 1
  then evalSimple state st1
  else evalSimple state st2
evalSimple state while@(DWhile expr st1) =
  if evalE state expr == 1
  then evalSimple state (DSequence st1 while)
  else state
evalSimple state (DSequence st1 st2) = evalSimple (evalSimple state st1) st2
evalSimple state DSkip = state

run :: State -> Statement -> State
run state statement = evalSimple state $ desugar statement

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input
   MY IMPLEMENTATION WAS CORRECT ALL ALONG, THIS GODDAMN SHITTY EXAMPLE IS WRONG
   IT SHOULD BE "In > 1" instead of "In > 0" and we should add 1 to In since --X
   operation isn't supported. AAAARRRRRGGGGHHHH!!!!!!

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Sequence (Assign "Out" (Val 1)) (Incr "In"))
                (Op (Var "In") Gt (Val 1))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]

fact :: State
fact = run (extend empty "In" 4) factorial

sqrr :: State
sqrr = run (extend empty "A" 9) squareRoot

fibo :: State
fibo = run (extend empty "In" 4) fibonacci