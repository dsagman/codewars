
import sys
# sys.path.append('/Users/dsagman/Code/github/python-test-framework')
sys.path.append('/home/agman/git/python-test-framework')
import codewars_test as test
from icecream import ic


class Bearing:
    def __init__(self, id): self.id = id
    def __repr__(self): return f"Bearing from Box {self.id}"

a, b, c, d, e = (Bearing(n) for n in range(5))

def build_weigh(deluxe):
    used = False
    def weigh(*bearings):
        nonlocal used
        if used:
            raise Exception("weigh has already been used!")
        if any(not isinstance(v, Bearing) for v in bearings):
            raise Exception("All arguments should be Bearings!")
        used = True
        return sum(10 + (bearing is deluxe) for bearing in bearings)
    return weigh

from itertools import chain, repeat

def identify_bb(bearings, weigh):
    bbs = list(chain.from_iterable(map(repeat, bearings, list(range(1,len(bearings)+1)))))
    minwt = 10*(len(bbs))
    bbswt = weigh(*bbs)
    # ic(numbbs,bbswt,minwt)
    return bearings[bbswt-minwt-1]


def run_test(vals, deluxe):
    test.expect(identify_bb(vals, build_weigh(deluxe)) is deluxe, "You did not return the deluxe bearing")

@test.describe("Sample tests")
def sample_tests():
    @test.it("Should find the deluxe bearing")
    def _():
        run_test([a, b], a)
        run_test([a, b, c], c)
        run_test([a, b, c], b)
        run_test([a, b, c, d, e], d)
        # run_test([a, b, c, d, e, f, g, h, i, j], d)
