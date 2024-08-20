# Solve the Letter Boxed puzzle from the New York Times
# The puzzle consists of 9 letters arranged in a 3x3 grid
# The goal is to find words that can be made by connecting adjacent letters
# Words must be at least 3 letters long and use each letter at most once

import time
from colorama import Fore 

def puz(num, exist_combos, words_valid):
    start_time = time.perf_counter()   
    combos = [e+[w] for w in words_valid for e in exist_combos if w[0] == e[-1][-1] and w not in e]
    sols = [c for c in combos if len(set(''.join(c))) == len(puzzle)]
    end_time = time.perf_counter()   
    print(f'{num} word valid combos: {Fore.BLUE}{len(combos):,}{Fore.WHITE}')
    print(f'{num} word full solutions: {Fore.BLUE}{len(sols):,}{Fore.WHITE}')
    print(f'{num} word elapsed time: {Fore.BLUE}{end_time - start_time:.2f} seconds{Fore.WHITE}')
    for s in sols[::max(1, round(len(sols) / 10))]:
        print(f'\t{Fore.GREEN}{s}{Fore.WHITE}')
    return combos

def get_words(dict_file):
    with open(dict_file) as f:
        words = f.read().splitlines()
    return [w for w in words if len(w) >= 3 and len(w) <= 9 and w[0].islower()]
        
if __name__ == '__main__':
    # puzzle = 'xnimalpjyegf'
    puzzle = 'nosumailtcvr'
    
    words = get_words('/usr/share/dict/words')
    print(f'Number of words imported: {Fore.BLUE}{len(words):,}{Fore.WHITE}')

    words_valid = [w for w in words 
                if not any(map(lambda x,y: x == y or x == -1 or y == -1, 
                (idx := [(puzzle.find(c) // 3) for c in w]), idx[1:]))]
    print(f'Number of possible words: {Fore.BLUE}{len(words_valid):,}{Fore.WHITE}')

    valid_2 = puz(2, [[w] for w in words_valid], words_valid)
    valid_3 = puz(3, valid_2, words_valid)
    valid_4 = puz(4, valid_3, words_valid)
    valid_5 = puz(5, valid_4, words_valid)
    
    
    
# ----------------- OLD CODE -----------------

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

# gave up on multiprocessing as too inefficient

# from multiprocessing import Pool
# from multiprocessing.pool import ThreadPool

# start_time = time.perf_counter()
# def find_valid_quads(w4):
#     return [(w1,w2,w3,w4) for w1,w2,w3 in valid_triples if w4[0] == w3[-1]]

# with ThreadPool() as p:
#     valid_quads = p.map(find_valid_quads, words_valid)
# valid_quads = [quad for quads in valid_quads for quad in quads]
# end_time = time.perf_counter()  
# print(f'Elapsed time (parallel): {end_time - start_time:.2f} seconds')
# print(f'Number of valid four word combos: {len(valid_quads):,}')

# start_time = time.perf_counter()
# def find_valid_triples(w3):
#     return [(w1,w2,w3) for w1,w2 in valid_pairs if w3[0] == w2[-1]]

# with Pool() as p:
#     valid_triples = p.map(find_valid_triples, words_valid)
# valid_triples = [triple for triples in valid_triples for triple in triples]
# end_time = time.perf_counter()
# print(f'Elapsed time (parallel): {end_time - start_time:.2f} seconds')
# print(f'Number of valid three word combos: {len(valid_triples):,}')

# start_time = time.perf_counter()
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