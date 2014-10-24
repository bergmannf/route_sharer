module Dist where
import Data.Either (Either)
import qualified Data.Text as T
import System.Environment
import System.Console.GetOpt

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

data Flag = Cost String | Person String deriving (Eq, Show)

options :: [OptDescr Flag]
options = [
  Option "c" ["cost"] (ReqArg Cost "STRING") "Total cost of the trip.",
  Option "p" ["passenger"] (ReqArg Person "STRING") "Passenger as part of the trip."
  ]

parsePassenger :: String -> Either String Passenger
parsePassenger s = case T.splitOn (T.pack ",") (T.pack s) of
  n:d:_ -> case reads $ T.unpack d :: [(Float, String)] of
    [(f, "")] -> Right $ Passenger ( T.unpack n ) f
    _ -> Left "Failed when parsing the float"
  _ -> Left "Not enough (or too little) fields"

parseOpts :: [Flag] -> (Float, [Passenger]) -> (Float, [Passenger])
parseOpts [] (x, y) = (x, y)
parseOpts (x:xs) (c, pass) = case x of
   Cost x -> parseOpts xs (read x :: Float, pass)
   Person x -> case parsePassenger x of
     Right p -> parseOpts xs (c, p : pass)
     Left _ -> parseOpts xs (c, pass)

main :: IO [Float]
main = do
  args <- getArgs
  let received = case getOpt RequireOrder options args of
        (opts, values, []) -> Just (opts, values)
        (_, _, errors) -> Nothing
  (cost, passengers) <- case received of
   Just (flags, args) -> do
     let (c, p) = parseOpts flags (0, [])
     print (c, p)
     return (c, p)
   Nothing -> return (0, [])
  let rel_cost = map (\p -> wayFraction p * cost) $ calculate passengers
  return rel_cost
