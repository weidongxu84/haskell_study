import Data.List
import Control.Monad
import qualified Numeric.Probability.Distribution as Distribution
import qualified Numeric.Probability.Transition as Transition

data Door = A | B | C deriving (Show, Eq, Ord, Enum)

doors :: [Door]
doors = [A .. C]

data State = Doors {prize :: Door, chosen :: Door, opened :: Door} deriving (Show, Eq, Ord)

start :: State
start = Doors {prize=u, chosen=u, opened=u} where u = undefined

hide :: Transition.T Float State
hide s = Distribution.uniform [s {prize=d} | d <- doors]

choose :: Transition.T Float State
choose s = Distribution.uniform [s {chosen=d} | d <- doors]

open :: Transition.T Float State
open s = Distribution.uniform [s {opened=d} | d <- doors \\ [prize s, chosen s]]

type Strategy = Transition.T Float State

switch :: Strategy
switch s = Distribution.uniform [s {chosen=d} | d <- doors \\ [chosen s, opened s]]

stay :: Strategy
stay = Transition.id

game :: Strategy -> Transition.T Float State
game s = foldl (>=>) Transition.id [hide, choose, open, s]

data Outcome = Win | Lose deriving (Show, Eq, Ord, Enum)

result :: State -> Outcome
result s = if chosen s == prize s then Win else Lose

eval :: Strategy -> Distribution.T Float Outcome
eval s = fmap result (game s start)
