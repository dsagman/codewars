import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
# sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic

def calculate_cart_total(contents):
    results = {'F': 0, 'R': 0, 'T': 0, 'H': 0}
    price = 0
    for item in contents:
        results[item] += 1  
    num_books = [v for k, v in results.items() if v > 0]

    while True:
        if len(num_books) == 4:
            price += 32          
        if len(num_books) == 3:
            price += 27
        if len(num_books) == 2:
            price += 19
        if len(num_books) == 1:
            price += 10 
        for k, v in results.items():
            results[k] -= 1
        num_books = [v for k, v in results.items() if v > 0]
        if len(num_books) == 0:
            break

    ic(results)
    ic(num_books)
    return price


sample_test_cases = [
    ('Should return 42 when 1 book of each ordered', [
        (['F', 'R', 'T', 'H'], 32),
    ]),
    ('Should return 10 when 1 book is ordered', [
        (['F'], 10),
        (['R'], 10),
        (['T'], 10),
        (['H'], 10),
    ]),
    ('Should return 20 when 2 copies of the same book are ordered', [
        (['F', 'F'], 20),
    ]),
]

@test.describe('Sample tests')
def sample_tests():
    for name, test_cases in sample_test_cases:
        @test.it(name)
        def _():
            for cart_contents, expected in test_cases:
                msg = f'calculate_cart_total({cart_contents})'
                test.assert_equals(calculate_cart_total(cart_contents), expected, msg)