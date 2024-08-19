# Solve the Letter Boxed puzzle from the New York Times
# The puzzle consists of 9 letters arranged in a 3x3 grid
# The goal is to find words that can be made by connecting adjacent letters
# Words must be at least 3 letters long and use each letter at most once

# from multiprocessing import Pool
# from multiprocessing.pool import ThreadPool
import time

# get list of english words
# words must be 3 to 9 characters long and have a vowel
# dictionary = 'words_alpha.txt'
dictionary ='/usr/share/dict/words'
with open(dictionary) as f:
    words = f.read().splitlines()

words = [w for w in words if len(w) >= 3 and len(w) <= 9 and any(v in w for v in 'aeiouy') and w[0].islower()]
print(f'Number of words imported: {len(words):,}')
puzzle = 'xnimalpjyegf'

# let's do it functionally!
words_valid = [w for w in words 
               if not any(map(lambda x,y: x == y or x == -1 or y == -1, 
               (idx := [(puzzle.find(c) // 3) for c in w]), idx[1:]))]
print(f'Number of possible words: {len(words_valid):,}')

start_time = time.perf_counter()
# Function to find valid pairs
# def find_valid_pairs(w1):
#     return [(w1, w2) for w2 in words_valid if w2[0] == w1[-1]]

# Find valid pairs in parallel
# with Pool() as p:
#     valid_pairs = p.map(find_valid_pairs, words_valid)
# valid_pairs = [pair for pairs in valid_pairs for pair in pairs]
# end_time = time.perf_counter()
# print(f'Elapsed time (parallel): {end_time - start_time:.2f} seconds')
# print(f'Number of valid two word combos: {len(valid_pairs):,}')

start_time = time.perf_counter()   
valid_pairs = [(w1,w2) for w2 in words_valid for w1 in words_valid if w2[0] == w1[-1]] 
end_time = time.perf_counter()
print(f'Number of valid two word combos: {len(valid_pairs):,}')
print(f'2 words elapsed time (sequential): {end_time - start_time:.2f} seconds')

two_word_solutions = [(w1,w2) for w1, w2 in valid_pairs if len(set(w1+w2)) == len(puzzle)]
print(f'Number of two word full solutions: {len(two_word_solutions):,}')
print(two_word_solutions)

# start_time = time.perf_counter()
# def find_valid_triples(w3):
#     return [(w1,w2,w3) for w1,w2 in valid_pairs if w3[0] == w2[-1]]

# with Pool() as p:
#     valid_triples = p.map(find_valid_triples, words_valid)
# valid_triples = [triple for triples in valid_triples for triple in triples]
# end_time = time.perf_counter()
# print(f'Elapsed time (parallel): {end_time - start_time:.2f} seconds')
# print(f'Number of valid three word combos: {len(valid_triples):,}')

start_time = time.perf_counter()
valid_triples = [(w1,w2,w3) for w3 in words_valid for w1,w2 in valid_pairs if w3[0] == w2[-1]]
three_word_solutions = [(w1,w2,w3) for w1,w2,w3 in valid_triples if len(set(w1+w2+w3)) == len(puzzle)]
end_time = time.perf_counter()
print(f'Number of valid three word combos: {len(valid_triples):,}')

print(f'3 words elapsed time (sequential): {end_time - start_time:.2f} seconds')
print(f'Number of three word full solutions: {len(three_word_solutions):,}')
# print(three_word_solutions)

# start_time = time.perf_counter()
# def find_valid_quads(w4):
#     return [(w1,w2,w3,w4) for w1,w2,w3 in valid_triples if w4[0] == w3[-1]]

# with ThreadPool() as p:
#     valid_quads = p.map(find_valid_quads, words_valid)
# valid_quads = [quad for quads in valid_quads for quad in quads]
# end_time = time.perf_counter()  
# print(f'Elapsed time (parallel): {end_time - start_time:.2f} seconds')
# print(f'Number of valid four word combos: {len(valid_quads):,}')

start_time = time.perf_counter()
valid_quads = [(w1,w2,w3,w4) for w4 in words_valid for w1,w2,w3 in valid_triples if w4[0] == w3[-1]]
four_word_solutions = [(w1,w2,w3,w4) for w1,w2,w3,w4 in valid_quads if len(set(w1+w2+w3+w4)) == len(puzzle)]
end_time = time.perf_counter()
print(f'Number of valid four word combos: {len(valid_quads):,}')
print(f'Number of four word full solutions: {len(four_word_solutions):,}')
print(f'4 words Elapsed time (sequential): {end_time - start_time:.2f} seconds')
# print(four_word_solutions)

# iterative solution for possible words

# find words that are possible to make in the puzzle
# only allow words where all the letters are different
# words_valid = []
# for w in words:
#     valid = True
#     idx_last = (puzzle.find(w[0]) // 3) + 1
#     if not idx_last:
#         continue
#     for c in w[1:]:
#         idx = (puzzle.find(c) // 3) + 1
#         if idx == idx_last or not idx:
#             valid = False
#             break
#         idx_last = idx
#     if valid and len(set(w)) == len(w):
#         words_valid.append(w)
# print(f'Number of possible words: {len(words_valid):,}')