# Solve the Letter Boxed puzzle from the New York Times
# The puzzle consists of 9 letters arranged in a 3x3 grid
# The goal is to find words that can be made by connecting adjacent letters
# Words must be at least 3 letters long and use each letter at most once
# Dictionary is much faster data structure

import time
from colorama import Fore 

def puz(num, exist_combos, first_dict):
    start_time = time.perf_counter()   
    # combos = [e+[w] for w in words_valid for e in exist_combos if w[0] == e[-1][-1] and w not in e]
    combos = [e + [w]  for e in exist_combos for w in first_dict[e[-1][-1]] if w not in e]
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

    first_dict = {k: [w for w in words_valid if w[0] == k] for k in puzzle}
    valid_2 = puz(2, [[w] for w in words_valid], first_dict)
    valid_3 = puz(3, valid_2, first_dict)
    valid_4 = puz(4, valid_3, first_dict)
    exit()
    # valid_5 = puz(5, valid_4, first_dict)
    
    
 