import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
# sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic
# Write a function that accepts a square matrix (N x N 2D array) and returns the determinant of the matrix.
# How to take the determinant of a matrix -- it is simplest to start with the smallest cases:
# A 1x1 matrix |a| has determinant a.
# A 2x2 matrix [ [a, b], [c, d] ] or
# |a  b|
# |c  d|
# has determinant: a*d - b*c.
# The determinant of an n x n sized matrix is calculated by reducing the 
# problem to the calculation of the determinants of n matrices ofn-1 x n-1 size.
# For the 3x3 case, [ [a, b, c], [d, e, f], [g, h, i] ] or
# |a b c|  
# |d e f|  
# |g h i|  
# the determinant is: a * det(a_minor) - b * det(b_minor) + c * det(c_minor) where det(a_minor) 
# refers to taking the determinant of the 2x2 matrix created by crossing out the row and column 
# in which the element a occurs
# |- - -|
# |- e f|
# |- h i|  
# Note the alternation of signs.
# The determinant of larger matrices are calculated analogously, 
# e.g. if M is a 4x4 matrix with first row [a, b, c, d], then:
# det(M) = a * det(a_minor) - b * det(b_minor) + c * det(c_minor) - d * det(d_minor)

def determinant(matrix):
    size = len(matrix)
    if size == 1:
        return matrix[0][0]
    det = 0
    for i in range(size):
        plusMinus = -1 if (i % 2) else 1
        subMatrix = [[matrix[r][c] for c in range(size) if c != i] for r in range(1,size)]
        det += plusMinus*matrix[0][i]*determinant(subMatrix)
    return det


m1 = [ [1, 3], [2,5]]
m2 = [ [2,5,3], [1,-2,-1], [1, 3, 4]]

test.assert_equals(determinant([[1]]), 1, "Determinant of a 1 x 1 matrix yields the value of the one element")
test.assert_equals(determinant(m1), -1, "Should return 1 * 5 - 3 * 2, i.e., -1 ")
test.expect(determinant(m2) == -20)