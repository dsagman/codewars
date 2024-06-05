import sys                   
# sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test 
                             
def solution(args):          
    runs = [0]*(len(args))   
    pad = ([float('inf')] + args)[::-1]
    for i, n in enumerate(args[::-1]):
        if pad[i+1] == n - 1:
           runs[i] = runs[i-1]+1
                             
    runs = runs[::-1][1:] + [0]
                             
    flag = False             
    result = ''              
    for i, r in enumerate(runs):
        if r <= 1 and not flag:
            result += str(args[i])+','
        if r >= 2 and not flag:
            flag = True
            result += str(args[i])+'-'
        if r == 0 and flag:
            flag = False
            result += str(args[i])+','

    if result[-1] == ',':
        result = result[0:-1]

    return result








test.describe("Sample Test Cases")

test.it("Simple Tests")
test.assert_equals(solution([-6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]), '-6,-3-1,3-5,7-11,14,15,17-20')
test.assert_equals(solution([-3,-2,-1,2,10,15,16,18,19,20]), '-3--1,2,10,15,16,18-20')