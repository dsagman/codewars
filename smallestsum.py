import platform
import sys
if platform.system() == 'Darwin':
    sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
else:
    sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic

import numpy as np

def solution(a):
    return np.gcd.reduce(a)*len(a)


test.describe('Example Tests')
test.assert_equals(solution ([9]), 9)
test.assert_equals(solution ([6, 9, 21]), 9)
test.assert_equals(solution ([1, 21, 55]), 3)