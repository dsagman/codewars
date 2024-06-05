import platform
import sys
if platform.system() == 'Darwin':
    sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
else:
    sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic

from itertools import cycle

class VigenereCipher(object):
    def __init__(self, key, alphabet):
        self.key = [alphabet.find(k) for k in key]
        self.az = alphabet
        self.mod = len(alphabet)

    def encode(self, text):
        return ''.join(self.az[(self.az.find(t)+k)%self.mod] if t in self.az else t for t, k in zip(text,cycle(self.key)))


    def decode(self, text):
        return ''.join(self.az[(self.az.find(t)-k)%self.mod] if t in self.az else t for t, k in zip(text,cycle(self.key)))


abc = "abcdefghijklmnopqrstuvwxyz"
key = "password"
c = VigenereCipher(key, abc)

test.assert_equals(c.encode('codewars'), 'rovwsoiv')
test.assert_equals(c.decode('rovwsoiv'), 'codewars')

test.assert_equals(c.encode('waffles'), 'laxxhsj')
test.assert_equals(c.decode('laxxhsj'), 'waffles')

test.assert_equals(c.encode('CODEWARS'), 'CODEWARS')
test.assert_equals(c.decode('CODEWARS'), 'CODEWARS')

katakana= "アイウエオァィゥェォカキクケコサシスセソタチツッテトナニヌネノハヒフヘホマミムメモヤャユュヨョラリルレロワヲンー "
key= "カタカナ"
c2 = VigenereCipher(key, katakana)
test.assert_equals(c2.encode('カタカナ'), 'タモタワ')
test.assert_equals(c2.decode('タモタワ'), 'カタカナ')

test.assert_equals(c2.encode('ドモアリガトゴザイマス'), 'ドオカセガヨゴザキアニ')
test.assert_equals(c2.decode('ドオカセガヨゴザキアニ' ), 'ドモアリガトゴザイマス')

        # coded = ""
        # for t, k in zip(text,cycle(self.key)):
        #     if t in self.alphabet:
        #         coded += self.alphabet[(self.alphabet.find(t)+k)%self.mod]
        #     else:
        #         coded += t