# Solve the Letter Boxed puzzle from the New York Times
# The puzzle consists of 9 letters arranged in a 3x3 grid
# The goal is to find words that can be made by connecting adjacent letters
# Words must be at least 3 letters long and use each letter at most once

import time
from colorama import Fore 
import numpy as np
import pandas as pd
from icecream import ic
# from functools import reduce
from itertools import chain
# from collections import defaultdict
        
def multi_or(arr):
    return np.any(arr, axis=0)

def puz_pd(num):
    ''' 
    num: number of words in the solution
    dataframe structure: 
    idx: list of indexes of word(s)
    first: first letter of words
    last: last letter of words
    bitmask: bitmask of letters in words vs puzzle
    '''
    global words1df
    global words2df
    global words3df
    global words4df
    global all_bits
    print(f'Finding {num} word combos with pandas')
    start_time = time.perf_counter()  
    if num == 2:
        words2df = pd.merge(words1df, words1df, left_on='last', right_on='first', suffixes=('_a','_b'))
        words2df['bitmask'] = words2df['bitmask_a'] | words2df['bitmask_b']
        words2df.rename(columns={'id1_a':'id1', 'id1_b':'id2', 'first_a':'first', 'last_b':'last'}, inplace=True)
        words2df.drop(columns=['first_b', 'last_a', 'bitmask_a', 'bitmask_b'], inplace=True)
        soldf = words2df[words2df['bitmask'] == all_bits]
        combos = words2df
    if num == 3:
        words3df = pd.merge(words2df, words1df, left_on='last', right_on='first', suffixes=('_a','_b'))
        words3df['bitmask'] = words3df['bitmask_a'] | words3df['bitmask_b']
        words3df.rename(columns={'id1_a':'id1', 'id2_a':'id2', 'id1_b':'id3', 'first_a':'first', 'last_b':'last'}, inplace=True)
        words3df.rename(columns={'first_a':'first', 'last_b':'last'}, inplace=True)
        words3df.drop(columns=['first_b', 'last_a', 'bitmask_a', 'bitmask_b'], inplace=True)
        soldf = words3df[words3df['bitmask'] == all_bits]
        combos = words3df
    if num == 4:
        words4df = pd.merge(words3df, words1df, left_on='last', right_on='first', suffixes=('_a','_b'))
        words4df['bitmask'] = words4df['bitmask_a'] | words4df['bitmask_b']
        words4df.rename(columns={'id1_a':'id1', 'id2_a':'id2', 'id3_a':'id3', 'id1_b':'id4', 'first_a':'first', 'last_b':'last'}, inplace=True)
        words4df.drop(columns=['first_b', 'last_a', 'bitmask_a', 'bitmask_b'], inplace=True)
        soldf = words4df[words4df['bitmask'] == all_bits]
        combos = words4df
    print(f'{num} word valid combos: {Fore.BLUE}{len(words2df):,}{Fore.WHITE}')
    print(f'{num} word solutions: {Fore.BLUE}{len(soldf):,}{Fore.WHITE}')
    end_time = time.perf_counter()   
    print(f'{num} word elapsed time: {Fore.BLUE}{end_time - start_time:.2f} seconds{Fore.WHITE}')
    print(f'Sample of {num} word solutions: ', Fore.GREEN) 
    samples = soldf[soldf.columns[soldf.columns.str.startswith('id')]].sample(n=10).values
    for s in samples:
        s_words = ' '.join([words_valid[i] for i in s])
        print(f'\t{Fore.GREEN}{s_words}{Fore.WHITE}')
    return

def get_bitmask(puzzle, w):
    bitmask = np.uint16(0)
    for i,c in enumerate(puzzle):
        if c in list(set(w)):
            bitmask |= (1 << i)
    return bitmask

def get_words(dict_file):
    with open(dict_file) as f:
        words = f.read().splitlines()
    return [w for w in words if len(w) >= 3 and len(w) <= 12 and w[0].islower() and w.isalpha()]
        
if __name__ == '__main__':
    # puzzle = 'xnimalpjyegf'
    # puzzle = 'nosumailtcvr'
    # puzzle = 'vkspyielurao'
    # puzzle = 'htubroqdeisw'
    puzzle = 'uxofatnhecdr'
    all_bits = 2**len(puzzle) - 1
    
    # dict_file = '/usr/share/dict/words'
    dict_file = 'words_alpha.txt'
    words = get_words(dict_file)
    print(f'Number of words imported: {Fore.BLUE}{len(words):,}{Fore.WHITE}')
    
    words_valid = [w for w in words 
                if not any(map(lambda x,y: x == y or x == -1 or y == -1, 
                (idx := [(puzzle.find(c) // 3) for c in w]), idx[1:]))]
    
    print(f'Number of possible words: {Fore.BLUE}{len(words_valid):,}{Fore.WHITE}')

    words1 = [[np.uint32(i), np.uint8(ord(w[0])), np.uint8(ord(w[-1])), get_bitmask(puzzle,w)] for i,w in enumerate(words_valid)]
    words1df = pd.DataFrame(words1, columns=['id1', 'first', 'last', 'bitmask'])
    print(words1df.info())
    
    puz_pd(2)
    puz_pd(3)
    puz_pd(4)
    
    
