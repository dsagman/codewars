import timeit
# get list of english words
with open('words_alpha.txt') as f:
    words = f.read().splitlines()

words = [w for w in words if len(w) >= 3]

puzzle = ['omt',
          'uri',
          'haf',
          'lpg']

alphabet = 'abcdefghijklmnopqrstuvwxyz'

not_puzzle = set(alphabet) -  set(''.join(puzzle))
print(not_puzzle)

print(timeit.timeit("[w for w in words if not set(w) & not_puzzle]", number=10, globals=globals()))
print(timeit.timeit("[w for w in words if all([letter in ''.join(puzzle) for letter in w])]", number=10, globals=globals()))

# words_possible1 = [word for word in words if not set(word) & not_puzzle]
# words_possible2 = [word for word in words if all([letter in ''.join(puzzle) for letter in word])]

# print(len(words_possible1))
# print(len(words_possible2))