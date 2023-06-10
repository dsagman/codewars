import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
# sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic

# In this example you have to validate if a user input string is alphanumeric. 
# The given string is not nil/null/NULL/None, so you don't have to check that.

# The string has the following conditions to be alphanumeric:

# At least one character ("" is not valid)
# Allowed characters are uppercase / lowercase latin letters and digits from 0 to 9
# No whitespaces / underscore
# import string
# def alphanumeric(password):
#     if len(password) == 0: return False
#     x = [c for c in password if c not in string.ascii_letters+string.digits]
#     return len(x) == 0

import re
# def alphanumeric(password):
#     if len(password) == 0: return False
#     if re.search("[^0-9a-zA-Z]",password) == None: return True
#     return False

def alphanumeric(string):
    return string.isalnum()


pattern = re.compile('^[0-9a-zA-Z]+$')

def alphanumeric(string):
  return pattern.match(string) is not None

@test.describe("Sample Tests")
def sample_tests():
    tests = [
        ("hello world_", False),
        ("PassW0rd", True),
        ("     ", False)
    ]
    for s, b in tests:
        @test.it('alphanumeric("' + s + '")')
        def sample_test():
            test.assert_equals(alphanumeric(s), b)