# Solve the Letter Boxed puzzle from the New York Times
# The puzzle consists of 9 letters arranged in a 3x3 grid
# The goal is to find words that can be made by connecting adjacent letters
# Words must be at least 3 letters long and use each letter at most once

import time
from colorama import Fore 
import numpy as np
from icecream import ic
from functools import reduce
from collections import defaultdict

class Combo:
    def __init__(self, word_idx, bitmask):
        self.word_idx = word_idx # list
        self.bitmask = bitmask # np.array
        
def multi_or(arr):
    return np.any(arr, axis=0)

# data structure:
#  [exist_combos idxes, exist_combos bitmask, words_valid idx, words_valid bitmask]
def puz_idx(num, exist_combos, words_valid, puzzle, bitmask):
    print(f'Finding {num} word combos with complicated index data structure')
    start_time = time.perf_counter()  
    combos = [Combo(e.word_idx + [words_dict[w]], e.bitmask | bitmask[w]) 
        for e in exist_combos for w in first_dict[words_valid[e.word_idx[-1]][-1]] 
        if words_dict[w] not in e.word_idx]
    sols = [c for c in combos if np.all(c.bitmask)]
    print(f'{num} word valid combos: {Fore.BLUE}{len(combos):,}{Fore.WHITE}')
    end_time = time.perf_counter()   
    print(f'{num} word full solutions: {Fore.BLUE}{len(sols):,}{Fore.WHITE}')
    print(f'{num} word elapsed time: {Fore.BLUE}{end_time - start_time:.2f} seconds{Fore.WHITE}')
    for s in sols[::max(1, round(len(sols) / 10))]:
        s_words = [words_valid[w] for w in s.word_idx]
        print(f'\t{Fore.GREEN}{s_words}{Fore.WHITE}')
    return combos
    
def puz(num, exist_combos, words_valid, puzzle):
    print(f'Finding {num} word combos with simple method')
    start_time = time.perf_counter()   
    combos = [' '.join([e,w])  for e in exist_combos for w in first_dict[e[-1]] if w not in e]
    print(f'{num} word valid combos: {Fore.BLUE}{len(combos):,}{Fore.WHITE}')
    sols = [c for c in combos if len(set(c)) == len(puzzle)+1]
    end_time = time.perf_counter()   
    print(f'{num} word full solutions: {Fore.BLUE}{len(sols):,}{Fore.WHITE}')
    print(f'{num} word elapsed time: {Fore.BLUE}{end_time - start_time:.2f} seconds{Fore.WHITE}')
    for s in sols[::max(1, round(len(sols) / 10))]:
        print(f'\t{Fore.GREEN}{s}{Fore.WHITE}')
    return combos

def get_words(dict_file):
    with open(dict_file) as f:
        words = f.read().splitlines()
    return [w for w in words if len(w) >= 3 and len(w) <= 9 and w[0].islower() and w.isalpha()]
        
if __name__ == '__main__':
    # puzzle = 'xnimalpjyegf'
    puzzle = 'nosumailtcvr'
    # puzzle = 'vkspyielurao'
    # puzzle = 'htubroqdeisw'
    
    dict_file = '/usr/share/dict/words'
    # dict_file = 'nytimes_games/words_alpha.txt'
    words = get_words(dict_file)
    print(f'Number of words imported: {Fore.BLUE}{len(words):,}{Fore.WHITE}')
    
    words_valid = [w for w in words 
                if not any(map(lambda x,y: x == y or x == -1 or y == -1, 
                (idx := [(puzzle.find(c) // 3) for c in w]), idx[1:]))]
    
    print(f'Number of possible words: {Fore.BLUE}{len(words_valid):,}{Fore.WHITE}')

       
    first_dict = {k: [w for w in words_valid if w[0] == k] for k in puzzle}
    words_dict = {w: i for i,w in enumerate(words_valid)}
    words_bitmask = {w: np.array([(puzzle[i] in w) for i in range(len(puzzle))]) for w in words_valid}
    
    words_combo = [Combo([words_dict[w]], words_bitmask[w]) for w in words_valid]
    valid_2_idx = puz_idx(2, words_combo, words_valid, puzzle, words_bitmask)
    valid_3_idx = puz_idx(3, valid_2_idx, words_valid, puzzle, words_bitmask)
    valid_4_idx = puz_idx(4, valid_3_idx, words_valid, puzzle, words_bitmask)
    
    valid_2 = puz(2, words_valid, first_dict, puzzle)
    valid_3 = puz(3, valid_2, first_dict, puzzle)
    # valid_4 = puz(4, valid_3, first_dict, puzzle)
    # valid_5 = puz(5, valid_4, first_dict, puzzle)
    
    
    
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

# another idea that wasn't worth it. checking for partial palindromes
# to prevent backtracking

    # in puz function
    # do not allow partial palindromes
    # combos = [' '.join([e,w])  for e in exist_combos for w in first_dict[e[-1]] if w not in e and w not in p_pdrom[e.split()[-1]]]

    # in main
    # possible full/partial palindrome words where we would be "backtracking"
    # p_pdrom = defaultdict(list)
    # for w in words_valid:
    #     poss_p = [w[:i-1:-1] for i in range(1,len(w)-2)]
    #     p = [p for p in poss_p if p in words_valid]
    #     if p:
    #         p_pdrom[w] = p
    #     # for p in p:
    #     #     p_pdrom[p].append(w)

    # print(f'Number of partial palindromes": {Fore.BLUE}{len(p_pdrom):,}{Fore.WHITE}')
    # # ic(p_pdrom)
