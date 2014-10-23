module Dist where
import System.Environment
import System.Console.GetOpt
import qualified Data.Text as T

data Passenger = Passenger { name :: String
                           , distance :: Float}
               | FractionPassenger { name :: String
                                   , distance :: Float
                                   , wayFraction :: Float }
               deriving (Eq, Show, Read, Ord)

sumRouteDistance :: [Passenger] -> Float
sumRouteDistance [] = 0
sumRouteDistance xs = sum $ map distance xs

calculatePartOfJourney :: Float -> [Passenger] -> [Passenger] -> [Passenger]
calculatePartOfJourney _ ([]) accum = accum
calculatePartOfJourney dist (y:ys) accum = calculatePartOfJourney dist ys (FractionPassenger (name y) (distance y) (distance y / dist):accum)
                                       
calculate :: [Passenger] -> [Passenger]
calculate [] = []
calculate xs = let sumDist = sumRouteDistance xs
               in
                calculatePartOfJourney sumDist xs []

data Flag = Input String

options :: [OptDescr Flag]
options = [
  Option "c" ["cost"] (ReqArg Input "STRING") "Total cost of the trip.",
  Option "p" ["passenger"] (ReqArg Input "STRING") "Passenger as part of the trip."
  ]

parsePassenger :: String -> [Passenger]
parsePassenger s = case T.splitOn (T.pack ",") (T.pack s) of
  n:d:_ -> [Passenger ( T.unpack n )  ( read $ T.unpack d :: Float )]
  _ -> []

parseOpts :: ([OptDescr Flag], [String]) -> (Float, [Passenger]) -> (Float, [Passenger])
parseOpts ([], []) (x, y) = (x, y)
parseOpts ([], []) _ = (0, [])
parseOpts (x:xs, y:ys) (c, pass) = case x of
  Option "c" _ _ _ -> parseOpts (xs, ys) (read y :: Float, pass)
  Option "p" _ _ _ -> parseOpts (xs, ys) (c, parsePassenger y ++ pass)
  _ -> (c, pass)

main :: IO [Float]
main = do
  args <- getArgs
  let header = "Usage"
  let received = case getOpt RequireOrder options args of
        (opts, values, []) -> Just (opts, values)
        (_, _, errors) -> Nothing
  case received of
   Just a -> print $ length $ fst a
   Nothing -> print "Error on usage"
  return [1.0]
