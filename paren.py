import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework')
import codewars_test as test

from icecream import ic

def valid_parentheses(string):
    ps = 0
    for c in string:
        if c == "(": 
            ps += 1
        if c == ")":
            ps -= 1
        if ps < 0:
            return False
    if ps == 0:
        return True
    return False
    


test.assert_equals(valid_parentheses(")"),False)
test.assert_equals(valid_parentheses("("),False)
test.assert_equals(valid_parentheses(""),True)
test.assert_equals(valid_parentheses("hi)("),False)
test.assert_equals(valid_parentheses("hi(hi)"),True)
test.assert_equals(valid_parentheses("("),False)
test.assert_equals(valid_parentheses("hi(hi)("),False)
test.assert_equals(valid_parentheses("((())()())"),True)
test.assert_equals(valid_parentheses("(c(b(a)))(d)"),True)
test.assert_equals(valid_parentheses("hi(hi))("),False)
test.assert_equals(valid_parentheses("())(()"),False)

