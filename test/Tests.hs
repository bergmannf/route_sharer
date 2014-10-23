import Dist
import Test.HUnit

test_route_sum = TestCase (assertEqual "" 30 $ sumRouteDistance [(Passenger "John" 10),
                                                                 (Passenger "Jim" 20)])
