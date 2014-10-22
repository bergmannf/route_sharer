module Dist where

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

main :: IO [Float]
main =
  let passenger_1 = Passenger "A" 360
      passenger_2 = Passenger "B" 360
      passenger_3 = Passenger "C" 70
      costs = 45.5
      res = calculate [passenger_1, passenger_2, passenger_3]
  in
   return $ map wayFraction res
