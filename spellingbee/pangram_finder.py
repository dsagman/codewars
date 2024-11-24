'''
Find all possible pangrams from a list of words
'''
from collections import defaultdict

dictionary = 'words_alpha.txt'
# dictionary ='/usr/share/dict/words'
pangrams_file = 'nyt_pangrams.txt'
with open(dictionary) as f:
    words = f.read().splitlines()
    
# Rules of NY Times Spelling Bee
# No words shorter than 4 letters
# Must include the center letter
# Pangram has all 7 letters
# Unwritten rules:
#     1<= vowels <= 3
#     No letter s
#     If q then u
#     x is not the center letter
pangrams_dict = defaultdict(list)
for w in words:
    k = ''.join(sorted(set(w)))
    if len(k) == 7:
        pangrams_dict[k].append(w)
    
nyt_pangrams = defaultdict(list)
for k, v in pangrams_dict.items():
    if 's' in k:
        continue
    if 'q' in k and 'u' not in k:
        continue
    vowels = sum([1 for l in set(k) if l in 'aeiou'])
    # all words have at least one vowel so no check for 0 vowels
    if vowels > 3:
        continue
    nyt_pangrams[k].append(v)

num_nyt_pangrams_no_x = len([k for k in nyt_pangrams.keys() if 'x' not in k])
num_nyt_pangrams_x = len(nyt_pangrams) - num_nyt_pangrams_no_x
num_nyt_puzzles = 6*num_nyt_pangrams_no_x + 7*num_nyt_pangrams_x

with open(pangrams_file, 'w') as f:
    for k, v in nyt_pangrams.items():
        f.write(f"{k} {v}\n")

print(f"Total number of words in the dictionary: {len(words):,}")
print(f"Total number of pangrams in the dictionary: {len(pangrams_dict):,}")
print(f"Total number of NYT pangrams in the dictionary: {len(nyt_pangrams):,}")
print(f"Total possible NYT Spelling Bee puzzles: {num_nyt_puzzles:,}")