import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework')
import codewars_test as test
from icecream import ic

# def cakes(recipe, available):
#     possible_cakes = []
#     for k in recipe:
#         if k in available:
#             possible_cakes.append(available[k]//recipe[k])
#         else:
#             return 0
#     return min(possible_cakes)

def cakes(recipe, available):
    return min([available[k]//recipe[k] if k in available else 0 for k in recipe])
 



@test.it('Testing Pete, the Baker')
def _():
    recipe = {"flour": 500, "sugar": 200, "eggs": 1}
    available = {"flour": 1200, "sugar": 1200, "eggs": 5, "milk": 200}
    test.assert_equals(cakes(recipe, available), 2, 'example #1')

    recipe = {"apples": 3, "flour": 300, "sugar": 150, "milk": 100, "oil": 100}
    available = {"sugar": 500, "flour": 2000, "milk": 2000}
    test.assert_equals(cakes(recipe, available), 0, 'example #2')
