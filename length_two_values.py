from math import ceil
import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
# sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic
from itertools import cycle, islice

# def alternate(n, first_value, second_value):
#     result = [first_value, second_value]*round(n/2)
#     if n % 2:
#         result.append(first_value)
#     return result

# def alternate(n, first_value, second_value):
#     return [first_value, second_value]*round(n/2)+[first_value]*(n%2)

def alternate(n, first_value, second_value):
    return list(islice((cycle([first_value,second_value])),n))


@test.describe("Tests")
def test_group():
    @test.it("Sample tests")
    def test_case():
        test.assert_equals(alternate(5, True, False), [True, False, True, False, True])
        test.assert_equals(alternate(20, "blue", "red"), ['blue', 'red', 'blue', 'red', 'blue', 'red', 'blue', 'red', 'blue', 'red', 'blue', 'red', 'blue', 'red', 'blue', 'red', 'blue', 'red', 'blue', 'red'])
        test.assert_equals(alternate(0, "lemons", "apples"), [])