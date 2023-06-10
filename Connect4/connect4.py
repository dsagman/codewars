import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework')
import codewars_test as test
from icecream import ic
from itertools import product

def count(xs, x):
    # BUG we need to only count consecutive, not total!!!!!
    # fixed
    group = 0
    counts = [0]*len(xs)
    for i in xs:
        if i == x:
            counts[group] += 1
        else:
            group += 1
    return max(counts)

class Connect4():
    def __init__(self):
        self.player = 2
        self.cols = 7
        self.rows = 6
        self.board = [[9]*self.cols for i in range(self.rows)] # self.board[row][col]
        self.gameover = False

        self.diagonals = []
        allRowCol = list(product(range(self.rows),range(self.cols)))
        # for / facing diagonals long enough to have 4 items
        for i in range(3,9):
            self.diagonals.append([j for j in allRowCol if j[0]+j[1]==i])
        # for \ facing diagonals
        for i in range(4):
            self.diagonals.append([j for j in allRowCol if j[0]-j[1]==i])
            self.diagonals.append([j for j in allRowCol if j[1]-j[0]==i])

        ic(self.diagonals)

    def nextplayer(self):
        self.player = 1 if self.player == 2 else 2
        return

    def row(self, n):
        return self.board[n]

    def col(self, n):
        return [self.board[i][n] for i in range(self.rows)]

    def diags(self):
        return [count([self.board[dj[0]][dj[1]] 
                     for dj in di], self.player) 
                     for di in self.diagonals]

    def win(self, row, col):
            r = count(self.row(row),self.player)
            c = count(self.col(col),self.player)
            d = self.diags()
            if r >= 4 or c >= 4 or max(d) >= 4:
                return True
            return False 

    def play(self, col):
        self.nextplayer()

        if self.gameover:
            return "Game has finished!"

        num9s = count(self.col(col),9)
        if num9s > 0:
            row = num9s-1
            self.board[row][col] = self.player
        else:
            self.nextplayer()
            return "Column full!"
        ic(self.board)
        if self.win(row,col):
            self.gameover = True
            return f"Player {self.player} wins!" 

        return f"Player {self.player} has a turn" 



game = Connect4()
ic(game.play(4)) #1
ic(game.play(1))
ic(game.play(5)) #1
ic(game.play(2))
ic(game.play(3)) #1
ic(game.play(2))
ic(game.play(5)) #1
ic(game.play(3))
ic(game.play(3)) #1
ic(game.play(4))
ic(game.play(3)) #1
ic(game.play(4))
ic(game.play(4)) #1
ic(game.play(0))
ic(game.play(5)) #1
ic(game.play(0))
ic(game.play(2)) #1
ic(game.play(5)) 
ic(game.play(2)) #1
# ic(game.play(0)) # player 1
# ic(game.play(1)) # player 2
# ic(game.play(2)) # player 1
# ic(game.play(3)) # player 2
# ic(game.play(0)) # player 1
# ic(game.play(2)) # player 2
# ic(game.play(1)) # player 1
# ic(game.play(3)) # player 2
# ic(game.play(6)) # player 1
# ic(game.play(0)) # player 2
# ic(game.play(6)) # player 1
# ic(game.play(1)) # player 2
# ic(game.play(6)) # player 1
# ic(game.play(0)) # player 2
# ic(game.play(4)) # player 1
# ic(game.play(0)) # player 2
# ic(game.play(4))
# ic(game.play(4))
# ic(game.play(4))
# ic(game.play(5))
# ic(game.play(5))
# ic(game.play(6))
# ic(game.play(6))



# @test.describe('Example Tests')

# def example_tests():
#     game = Connect4()
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(0), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(0), "Player 2 has a turn")
    
#     game = Connect4()
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(0), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(1), "Player 2 has a turn")
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(0), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(1), "Player 2 has a turn")
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(0), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(1), "Player 2 has a turn")
#     @test.it("Should return: Player 1 wins!")
#     def example_test_case():
#         test.assert_equals(game.play(0), "Player 1 wins!")
    
#     game = Connect4()
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(4), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(4), "Player 2 has a turn")
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(4), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(4), "Player 2 has a turn")
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(4), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(4), "Player 2 has a turn")
#     @test.it("Should return: Column full!")
#     def example_test_case():
#         test.assert_equals(game.play(4), "Column full!")
    
#     game = Connect4()
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(1), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(1), "Player 2 has a turn")
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(2), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(2), "Player 2 has a turn")
#     @test.it("Should return: Player 1 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(3), "Player 1 has a turn")
#     @test.it("Should return: Player 2 has a turn")
#     def example_test_case():
#         test.assert_equals(game.play(3), "Player 2 has a turn")
#     @test.it("Should return: Player 1 wins!")
#     def example_test_case():
#         test.assert_equals(game.play(4), "Player 1 wins!")
#     @test.it("Should return: Game has finished!")
#     def example_test_case():
#         test.assert_equals(game.play(4), "Game has finished!")