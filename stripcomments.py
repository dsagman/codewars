from shlex import join
import sys
# sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic

def solution(string,markers):
    if markers == []:
        return string
    return '\n'.join([s[:min([s.find(m) if s.find(m) >= 0 else len(s) for m in markers])].rstrip() for s in string.splitlines()])









test.assert_equals(solution("apples, pears # and bananas\ngrapes\nbananas !apples", ["#", "!"]), "apples, pears\ngrapes\nbananas")
test.assert_equals(solution("a #b\nc\nd $e f g", ["#", "$"]), "a\nc\nd")