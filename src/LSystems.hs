module LSystems ( LSystem(LSystem), ColouredLine, Command(..)
                , angle, axiom, rules, lookupChar
                , expandOne, expand, move, parse, trace1, trace2
                , expandLSystem ) where

import IC.Colour

-- Part 1 - Warmup
----------------------------------------------------------
type Rules a = [(Char, [a])]
data LSystem = LSystem Float [Char] (Rules Char)

-- Functions for working with systems. Skeleton optional...

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (LSystem angleValue _ _) = angleValue

-- Returns the axiom string for the given system.
axiom :: LSystem -> [Char]
axiom (LSystem _ axiomValue _) = axiomValue

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules Char
rules (LSystem _ _ rulesValue) = rulesValue

--
-- Pre: the character has a binding in the Rules list
--
lookupChar :: Rules a -> Char -> [a]
lookupChar ((ruleChar, values):rest) searchChar
  | ruleChar == searchChar = values
  | otherwise = lookupChar rest searchChar

-- Part 2: Expansion and Parsing
data Command = F | L | R | B [Command] deriving Show

--
-- Expand command string s once using rule table r
--
expandOne :: Rules Char -> [Char] -> [Char]
expandOne ruleList axiomValue = concat [lookupChar ruleList axiomChar | axiomChar <- axiomValue]

--
-- Expand command string s n times using rule table r
--
expand :: Rules Char -> [Char] -> Int -> [Char]
expand ruleList axiomValue 0 = axiomValue
expand ruleList axiomValue n =
  expand ruleList (expandOne ruleList axiomValue) (n - 1)

parse :: [Char] -> [Command]
parse input = fst (parseHelper input)
  where
    parseHelper :: [Char] -> ([Command], [Char])
    parseHelper [] = ([], [])
    parseHelper (']':rest) = ([], rest)
    parseHelper ('[':rest) =
      (B branchCommands : nextCommands, remainingInput)
      where
        (branchCommands, afterBranch) = parseHelper rest
        (nextCommands, remainingInput) = parseHelper afterBranch
    parseHelper (char:rest) =
      (lookupChar commandMap char ++ nextCommands, remainingInput)
      where
        (nextCommands, remainingInput) = parseHelper rest

expandLSystem :: LSystem -> Int -> [Command]
expandLSystem lsystem n =
  parse (expand (rules lsystem) (axiom lsystem) n)

-- Part 3: Turtles
type Vertex = (Float, Float)
type TurtleState = (Vertex, Float)
type ColouredLine = (Vertex, Vertex, Colour)

initialState :: TurtleState
initialState = ((0, 0), 90)

-- Move a turtle.
--
-- F moves distance 1 in the current direction.
-- L rotates left according to the given angle.
-- R rotates right according to the given angle.
move :: Command -> Float -> TurtleState -> TurtleState
move = L angleValue ((x, y), orientation) = ((x, y), orientation + angleValue)

move R angleValue ((x, y), orientation) = ((x, y), orientation - angleValue)

move F _ ((x, y), orientation) =
  ((x + cos (degreesToRadians orientation)
    , y + sin (degreesToRadians orientation)
    )
  , orientation
  )

trace1 :: [Command] -> Float -> Colour -> [ColouredLine]
trace1 commands angleValue colour = go commands initialState
  where
    go :: [Command] -> TurtleState -> [ColouredLine]
    go [] _ = []

    go (F:rest) currentState =
      (startPoint, endPoint, colour) : go rest nextState
      where
        nextState = move F angleValue currentState
        (startPoint, _) = currentState
        (endPoint, _) = nextState

    go (L:rest) currentState =
      go rest (move L angleValue currentState)

    go (R:rest) currentState =
      go rest (move R angleValue currentState)

    go (B branchCommands:rest) currentState =
      go branchCommands currentState ++ go rest currentState

-- This version uses an explicit stack of residual commands and turtle states
trace2 :: [Command] -> Float -> Colour -> [ColouredLine]
trace2 commands angleValue colour = go commands initialState []
  where
    go :: [Command] -> TurtleState
       -> [([Command], TurtleState)] -> [ColouredLine]

    go [] _ [] = []

    go [] _ ((savedCommands, savedState):stack) =
      go savedCommands savedState stack

    go (F:rest) currentState stack =
      (startPoint, endPoint, colour) : go rest nextState stack
      where
        nextState = move F angleValue currentState
        (startPoint, _) = currentState
        (endPoint, _) = nextState

    go (L:rest) currentState stack =
      go rest (move L angleValue currentState) stack

    go (R:rest) currentState stack =
      go rest (move R angleValue currentState) stack

    go (B branchCommands:rest) currentState stack =
      go branchCommands currentState ((rest, currentState):stack)

-- Provided Helper Functions
------------------------------------------------------------------------------

degreesToRadians :: Float -> Float
degreesToRadians x = (x / 180) * pi

commandMap :: Rules Command
commandMap = [ ('M', [F])
             , ('N', [F])
             , ('X', [])
             , ('Y', [])
             , ('A', [])
             , ('+', [L])
             , ('-', [R])
             ]
