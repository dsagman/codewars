import sys
# sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic


from dataclasses import dataclass
from typing import Union, List
import itertools

class Node:
    def __init__(self, L, R, n):
        self.left = L
        self.right = R
        self.value = n

# @dataclass
# class Node:
#     left  :  Union['Node', None]
#     right :  Union['Node', None]
#     value  :  int

def tree_by_levels(node : Node) -> List[int]:
    return list(itertools.chain(*[get_tree_level(node, h) for h in range(1, tree_height(node)+1)]))

def get_tree_level(node : Node, level : int) -> List[int]:
    if node == None:
        return []
    if level == 1:
        return [node.value]
    return [*get_tree_level(node.left, level - 1), *get_tree_level(node.right, level - 1)]

def tree_height(node : Node) -> int:
    if node == None:
        return 0
    height_left = tree_height(node.left) + 1
    height_right = tree_height(node.right) + 1
    if height_left > height_right:
        return height_left
    else:
        return height_right



test.assert_equals(tree_by_levels(None), [])
test.assert_equals(tree_by_levels(Node(Node(None, Node(None, None, 4), 2), Node(Node(None, None, 5), Node(None, None, 6), 3), 1)), [1, 2, 3, 4, 5, 6])

