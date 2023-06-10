import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework')
import codewars_test as test
from icecream import ic

from functools import reduce
# --- David's ----
# def persistence(n):
#     mDigits = reduce(lambda x,y: x*y, [int(d) for d in str(n)])
#     if mDigits < 10:
#         return 1
#     return 1 + persistence(mDigits)


# --------Ben's --------------------------------------------------
from numpy import prod
def persistence(n):
    count = 0
    result = n
    while result >= 10:
        count += 1
        result = prod(list(map(int, [x for x in str(result)])))
    return count


test.it("Basic tests")
test.assert_equals(persistence(39), 3)
test.assert_equals(persistence(4), 0)
test.assert_equals(persistence(25), 2)
test.assert_equals(persistence(999), 4)



