import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework')
import codewars_test as test
from icecream import ic

def bouncing_ball(h, bounce, window):
    if (not h > 0) or (not (bounce > 0 and bounce < 1)) or (not (window < h)): 
        return -1
    count = []
    count.append(h)
    while count[-1] > window:
        count.append(count[-1] * bounce)
    return (len(count)-2)*2+1
        
@test.describe('Tests')
def fixed_tests():
    def testing(h, bounce, window, exp):
        actual = bouncing_ball(h, bounce, window)
        test.assert_equals(actual, exp)
        
    @test.it('Fixed Tests')
    def tests():
        testing(2, 0.5, 1, 1)
        testing(3, 0.66, 1.5, 3)
        testing(30, 0.66, 1.5, 15)
        testing(30, 0.75, 1.5, 21)