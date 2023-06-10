# https://www.codewars.com/kata/579f54c672292dc1a20001bd

# Input arguments:
#     root: the root note of the chord; concatenation of the degree (C, D, E, F, G, A, B) 
#       and optionally the accidental (b, #)
#     color: the type of chord; (m, M, dim, aug); 
#       m -> minor chord, M -> major chord, dim -> diminished chord, aug -> augmented chord
# Result value:
#     chord: a list of 3 notes that make up a tertian triad (optional accidentals; 
#   bb, b, # or x); a tertian triad is a chord consisting of 3 notes that 
#   tacks intervals of 3ths.
# Implementation notes
#     Rather than using the accidentals ♯ and ♭, we'll use the 
#       following ASCII-friendly notation: bb (instead of ♭♭), 
#       b (instead of ♭), # (instead of ♯) and x (instead of ♯♯). 
#     You will not be asked to deal with notes with more than 2 flats or sharps. 
#     For this reason, B# aug will be omitted from test cases, as its 5th is triply augmented (F#x or F###). 
#     You will need to use the correct degrees; 
#     enharmonic equivalence is not allowed in this kata. For instance: C m -> (C, Eb, G) OK, but (C, D#, G) not OK. The interval from C to D# is not a 3th, it's an augmented 2nd. 



import codewars_test as test 

def chord_triad(root: str, color: str) -> tuple:
    notes = ['A','A#','B','C','C#','D','D#','E','F','F#','G','G#']*2
    root_note = root[0]
    root_idx = notes.index(root_note)
    root_acc = ''
    if len(root) == 2:
        root_acc = root[1]
    third_acc = ''
    fifth_acc = ''
    if color in ['m','dim']:
        third_acc = 'b'
    if color == 'dim':
        fifth_acc = 'b'
    if color == 'aug':
        fifth_acc = '#'
    chord = [notes[root_idx]+root_acc, 
             notes[root_idx+4]+root_acc+third_acc, 
             notes[root_idx+7]+root_acc+fifth_acc]
    for i, _ in enumerate(chord):
        chord[i] = chord[i].replace('#b','').replace('b#','').replace('##','x')
    return tuple(chord)


print(chord_triad('C', 'M'))
print(chord_triad('C#', 'M'))
print(chord_triad('Cb', 'M'))
print(chord_triad('E', 'M'))
print(chord_triad('D', 'M'))
print(chord_triad('D', 'm'))
print(chord_triad('F', 'M'))
print(chord_triad('G', 'M'))
print(chord_triad('A', 'M'))
print(chord_triad('B', 'M'))


# def act(root, color, expected):
#     @test.it(f"chord_triad({root}, {color})")
#     def run():
#         actual = chord_triad(root, color)
#         test.assert_equals(actual, expected)

# @test.describe("Sample tests")
# def test_cases():
#     act("C", "M", ("C","E","G"))
#     act("C#", "M", ("C#","E#","G#"))
#     act("C", "m", ("C","Eb","G"))
#     act("C", "dim", ("C","Eb","Gb"))
#     act("C", "aug", ("C","E","G#"))
#     act("A", "dim", ("A","C","Eb"))
#     act("D", "aug", ("D","F#","A#"))
#     act("F", "m", ("F","Ab","C"))
