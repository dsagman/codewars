
import sys
# sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic

# Tips and notes: it helps to start counting from 1 up to n, instead of the usual range 0..n-1; k will always be >=1.

# For example, with n=7 and k=3 josephus(7,3) should act this way.

# [1,2,3,4,5,6,7] - initial sequence
# [1,2,4,5,6,7] => 3 is counted out and goes into the result [3]
# [1,2,4,5,7] => 6 is counted out and goes into the result [3,6]
# [1,4,5,7] => 2 is counted out and goes into the result [3,6,2]
# [1,4,5] => 7 is counted out and goes into the result [3,6,2,7]
# [1,4] => 5 is counted out and goes into the result [3,6,2,7,5]
# [4] => 1 is counted out and goes into the result [3,6,2,7,5,1]
# [] => 4 is counted out and goes into the result [3,6,2,7,5,1,4]
# So our final result is:

# josephus([1,2,3,4,5,6,7],3)==[3,6,2,7,5,1,4]
# this direct method below works
# def josephus(items,k):
#     ic(items)
#     ring = len(items)
#     j, m = [], 0
#     while len(j) != len(items):
#         for n in range(1,k+1):
#             m += 1
#             while items[(m-1) % ring] == None:
#                 m += 1
#         j.append(items[(m-1) % ring])
#         items[(m-1) % ring] = None
#         ic((m-1)%ring,j)
#     return j



# this also works!
def josephus(items,k):
    ring = len(items)
    if ring == 0:
        return []
    if ring == 1:
        return items
    if k > ring:
        newk = ((k-1) % ring) + 1
    else:
        newk = k
    a,b,c = items[0:newk-1],[items[newk-1]],items[newk:]
    return [b+josephus(c+a,k)][0]



test.describe("Basic tests")
test.assert_equals(josephus([1,2,3,4,5,6,7,8,9,10],1),[1,2,3,4,5,6,7,8,9,10], "Should return the same exact list if k==1")
test.assert_equals(josephus([1,2,3,4,5,6,7,8,9,10],2),[2, 4, 6, 8, 10, 3, 7, 1, 9, 5])
test.assert_equals(josephus(["C","o","d","e","W","a","r","s"],4),['e', 's', 'W', 'o', 'C', 'd', 'r', 'a'],"Should work for values different from numbers too")
test.assert_equals(josephus(["C",0,"d",3,"W",4,"r",5],4),[3, 5, 'W', 0, 'C', 'd', 'r', 4],"Should work for values of different types too")
test.assert_equals(josephus([1,2,3,4,5,6,7],3),[3, 6, 2, 7, 5, 1, 4])
test.assert_equals(josephus([],3),[], "Should work for empty arrays too")
test.assert_equals(josephus([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50],11),[11, 22, 33, 44, 5, 17, 29, 41, 3, 16, 30, 43, 7, 21, 36, 50, 15, 32, 48, 14, 34, 1, 20, 39, 9, 28, 2, 25, 47, 24, 49, 27, 8, 38, 19, 6, 42, 35, 26, 23, 31, 40, 4, 18, 12, 13, 46, 37, 45, 10], "Should work for larger arrays too")
test.assert_equals(josephus([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15],40),[10, 7, 8, 13, 5, 4, 12, 11, 3, 15, 14, 9, 1, 6, 2], "Should work for larger ks too")
test.assert_equals(josephus([1],3),[1], "Should work for single-item arrays too")
test.assert_equals(josephus([True,False,True,False,True,False,True,False,True],9),[True, True, True, False, False, True, False, True, False],"Should work for values different from numbers too")

# test.describe("Random Tests")
# base=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50]

# from random import randint

# def josephusol(items,k):
#     i,res=0,[]
#     while items:
#         i=(i+k-1)%len(items)
#         res+=[items.pop(i)]
#     return res

# for i in range(40):
#     testitems=base[:randint(0,50)]
#     testk=randint(1,20)
#     test.it("Testing for josephus("+str(testitems)+", "+str(testk)+"]")
#     test.assert_equals(josephus(testitems[:],testk),josephusol(testitems,testk),"Should work for random inputs too")