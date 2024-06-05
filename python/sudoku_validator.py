import sys                   
sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
# sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test 
from icecream import ic
# import numpy as np

def valid_solution(board):
    return all([45 == sum(x) for x in board + list(zip(*board)) + [[board[n][m] for m in range(j,j+3) for n in range(i,i+3)] for i in range(0,9,3) for j in range(0,9,3)]])
    # return all([45==sum(x) for x in board+list(zip(*board))+[[board[n][m] for m in [j,j+1,j+2] for n in [i,i+1,i+2]] for i in [0,3,6] for j in [0,3,6]]])

    # return all(map((lambda x: 45 == sum(x)),(board + list(zip(*board)) + [[board[n][m] for m in range(j,j+3) for n in range(i,i+3)] for i in range(0,9,3) for j in range(0,9,3)])))
    
    # for row in board + list(zip(*board)) + [[board[n][m] for m in range(j,j+3) for n in range(i,i+3)] for i in range(0,9,3) for j in range(0,9,3)]:
    #     if sum(row) != 45: return False
    # return True

# def boxes(board):
#     npboard = np.array(board)
#     box = []
#     for i in range(9):
#         j = int(np.floor(i/3))*3
#         k = (i % 3) * 3
#         box.append(npboard[j:j+3,k:k+3].flatten())
#     return box

# def boxes2(board):
#     box = [[board[n][m] for m in range(j,j+3) for n in range(i,i+3)] for i in range(0,9,3) for j in range(0,9,3)]
#     return box



@test.describe('Testing...')
def _():
    @test.it('Sample tests')
    def _():

        test.assert_equals(valid_solution([[5, 3, 4, 6, 7, 8, 9, 1, 2], 
                                 [6, 7, 2, 1, 9, 5, 3, 4, 8],
                                 [1, 9, 8, 3, 4, 2, 5, 6, 7],
                                 [8, 5, 9, 7, 6, 1, 4, 2, 3],
                                 [4, 2, 6, 8, 5, 3, 7, 9, 1],
                                 [7, 1, 3, 9, 2, 4, 8, 5, 6],
                                 [9, 6, 1, 5, 3, 7, 2, 8, 4],
                                 [2, 8, 7, 4, 1, 9, 6, 3, 5],
                                 [3, 4, 5, 2, 8, 6, 1, 7, 9]]), True);

        test.assert_equals(valid_solution([[5, 3, 4, 6, 7, 8, 9, 1, 2], 
                                 [6, 7, 2, 1, 9, 0, 3, 4, 9],
                                 [1, 0, 0, 3, 4, 2, 5, 6, 0],
                                 [8, 5, 9, 7, 6, 1, 0, 2, 0],
                                 [4, 2, 6, 8, 5, 3, 7, 9, 1],
                                 [7, 1, 3, 9, 2, 4, 8, 5, 6],
                                 [9, 0, 1, 5, 3, 7, 2, 1, 4],
                                 [2, 8, 7, 4, 1, 9, 6, 3, 5],
                                 [3, 0, 0, 4, 8, 1, 1, 7, 9]]), False);

        test.assert_equals(valid_solution([[1, 3, 2, 5, 7, 9, 4, 6, 8]
                                ,[4, 9, 8, 2, 6, 1, 3, 7, 5]
                                ,[7, 5, 6, 3, 8, 4, 2, 1, 9]
                                ,[6, 4, 3, 1, 5, 8, 7, 9, 2]
                                ,[5, 2, 1, 7, 9, 3, 8, 4, 6]
                                ,[9, 8, 7, 4, 2, 6, 5, 3, 1]
                                ,[2, 1, 4, 9, 3, 5, 6, 8, 7]
                                ,[3, 6, 5, 8, 1, 7, 9, 2, 4]
                                ,[8, 7, 9, 6, 4, 2, 1, 5, 3]]), True);

        test.assert_equals(valid_solution([[1, 3, 2, 5, 7, 9, 4, 6, 8]
                                ,[4, 9, 8, 2, 6, 1, 3, 7, 5]
                                ,[7, 5, 6, 3, 8, 4, 2, 1, 9]
                                ,[6, 4, 3, 1, 5, 8, 7, 9, 2]
                                ,[5, 2, 1, 7, 9, 3, 8, 4, 6]
                                ,[9, 8, 7, 4, 2, 6, 5, 3, 1]
                                ,[2, 1, 4, 9, 3, 5, 6, 8, 7]
                                ,[3, 6, 5, 8, 1, 7, 9, 2, 4]
                                ,[8, 7, 9, 6, 4, 2, 1, 3, 5]]), False);

        test.assert_equals(valid_solution([[1, 3, 2, 5, 7, 9, 4, 6, 8]
                                ,[4, 9, 8, 2, 6, 0, 3, 7, 5]
                                ,[7, 0, 6, 3, 8, 0, 2, 1, 9]
                                ,[6, 4, 3, 1, 5, 0, 7, 9, 2]
                                ,[5, 2, 1, 7, 9, 0, 8, 4, 6]
                                ,[9, 8, 0, 4, 2, 6, 5, 3, 1]
                                ,[2, 1, 4, 9, 3, 5, 6, 8, 7]
                                ,[3, 6, 0, 8, 1, 7, 9, 2, 4]
                                ,[8, 7, 0, 6, 4, 2, 1, 3, 5]]), False); 

        test.assert_equals(valid_solution([[1, 2, 3, 4, 5, 6, 7, 8, 9]
                                 ,[2, 3, 4, 5, 6, 7, 8, 9, 1]
                                 ,[3, 4, 5, 6, 7, 8, 9, 1, 2]
                                 ,[4, 5, 6, 7, 8, 9, 1, 2, 3]
                                 ,[5, 6, 7, 8, 9, 1, 2, 3, 4]
                                 ,[6, 7, 8, 9, 1, 2, 3, 4, 5]
                                 ,[7, 8, 9, 1, 2, 3, 4, 5, 6]
                                 ,[8, 9, 1, 2, 3, 4, 5, 6, 7]
                                 ,[9, 1, 2, 3, 4, 5, 6, 7, 8]]), False);

                                # [[1, 7, 4, 6, 5, 8, 3, 9, 2], 
                                #  [9, 5, 6, 4, 2, 3, 1, 8, 7], 
                                #  [2, 8, 3, 7, 1, 9, 4, 6, 5], 

                                #  [6, 2, 7, 0, 8, 4, 5, 1, 3], 
                                #  [4, 1, 9, 2, 3, 5, 8, 7, 6], 
                                #  [8, 3, 5, 1, 7, 6, 9, 2, 4], 

                                #  [7, 6, 8, 3, 4, 1, 2, 5, 9], 
                                #  [5, 4, 2, 8, 9, 7, 6, 3, 1], 
                                #  [3, 9, 1, 5, 6, 2, 7, 4, 8]]