import platform

import sys
if platform.system() == 'Darwin':
    sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
else:
    sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic