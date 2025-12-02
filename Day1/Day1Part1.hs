import System.IO
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- Data types
data Direction
    = L
    | R
    deriving (Read, Show, Eq)

data Instruction = Instruction
    { direction :: Direction
    , units     :: Int
    } deriving (Show, Eq) 

getDirection :: Char -> Maybe Direction
getDirection c =
    case c of
        'L' -> Just L
        'R' -> Just R
        _ -> Nothing

extractInstruction :: String -> Maybe Instruction 
extractInstruction [] = Nothing
extractInstruction (dir : units) = do
    d <- getDirection dir
    u <- readMaybe units
    return (Instruction d u)

constructInstructionList :: [String] -> [Instruction]
constructInstructionList = mapMaybe extractInstruction

turnDial
    :: Int -- initial dial pos
    -> Instruction
    -> Int -- final dial pos
turnDial pos (Instruction L u) = wrap(pos - u)
turnDial pos (Instruction R u) = wrap(pos + u)

wrap :: Int -> Int
wrap x = (x `mod` 100 + 100) `mod` 100

executeInstructions
    :: [Instruction] -- instruction list
    -> Int -- dial position
    -> Int -- accumulator for 0's
    -> Int -- final output of 0's counted
executeInstructions [] _ acc = acc
executeInstructions (instr : rest) pos acc =
    case pos' of
        0 -> executeInstructions rest pos' (acc + 1)
        _ -> executeInstructions rest pos' acc
    where
        pos' = turnDial pos instr

-- Main
main :: IO ()
main = do
    contents <- readFile "input"
    let instructions = constructInstructionList $ lines contents
    let zeroCount = executeInstructions instructions 50 0
    print zeroCount