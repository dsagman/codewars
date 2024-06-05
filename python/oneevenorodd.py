import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework')
import codewars_test as test
from icecream import ic
# from more_itertools import partition
# from itertools import tee
# https://www.codewars.com/kata/5526fc09a1bbd946250002dc

# def find_outlier(integers):
#         odd_list=[]
#         even_list=[]
#         for i in integers: 
#             if (i % 2 == 0): 
#                 even_list.append(i) 
#             else: 
#                 odd_list.append(i)
#         if (len(odd_list)==1):
#             strings = [str(integer) for integer in odd_list]
#             a_string = "".join(strings)
#             an_integer = int(a_string)
#             return(an_integer)
#         else:
#             strings = [str(integer) for integer in even_list]
#             a_string = "".join(strings)
#             an_integer = int(a_string)
#             return(an_integer)
def tee(__iterable, __n): ...


def partition(pred, iterable):
    """
    Returns a 2-tuple of iterables derived from the input iterable.
    The first yields the items that have ``pred(item) == False``.
    The second yields the items that have ``pred(item) == True``.

        >>> is_odd = lambda x: x % 2 != 0
        >>> iterable = range(10)
        >>> even_items, odd_items = partition(is_odd, iterable)
        >>> list(even_items), list(odd_items)
        ([0, 2, 4, 6, 8], [1, 3, 5, 7, 9])

    If *pred* is None, :func:`bool` is used.

        >>> iterable = [0, 1, False, True, '', ' ']
        >>> false_items, true_items = partition(None, iterable)
        >>> list(false_items), list(true_items)
        ([0, False, ''], [1, True, ' '])

    """
    if pred is None:
        pred = bool

    evaluations = ((pred(x), x) for x in iterable)
    t1, t2 = tee(evaluations)
    return (
        (x for (cond, x) in t1 if not cond),
        (x for (cond, x) in t2 if cond),
    )

def find_outlier(integers):
        even_list, odd_list = partition(lambda x: x%2==0, integers)
        even_list=list(even_list)
        odd_list=list(odd_list)
        if (len(list(odd_list))==1):
            return odd_list[0]
        else:
            return even_list[0]


test.assert_equals(find_outlier([2, 4, 6, 8, 10, 3]), 3)
test.assert_equals(find_outlier([2, 4, 0, 100, 4, 11, 2602, 36]), 11)
test.assert_equals(find_outlier([160, 3, 1719, 19, 11, 13, -21]), 160)