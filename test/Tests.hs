import Dist
import Test.HUnit

test_route_dist = TestCase (assertEqual "" 2 $ maxRouteDistance [(Passenger "John" 1),
                                                                 (Passenger "Jim" 2)])

test_route_sum = TestCase (assertEqual "" 30 $ sumRouteDistance [(Passenger "John" 10),
                                                                 (Passenger "Jim" 20)])
