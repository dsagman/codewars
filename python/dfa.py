import platform
import sys
if platform.system() == 'Darwin':
    sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
else:
    sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic

class Automaton(object):
    state_dict = {'q1￩0' : 'q1', 
                  'q1￩1' : 'q2',
                  'q2￩0' : 'q3',
                  'q2￩1' : 'q2',
                  'q3￩0' : 'q2',
                  'q3￩1' : 'q2'}
    accept_state = 'q2'
    start_state = 'q1'

    def __init__(self):
        self.state = self.start_state
    
    def read_commands(self, commands):
        for command in commands:
            self.state = self.state_dict[f'{self.state}￩{command}']
        return self.state == self.accept_state


    # def read_commands(self, commands):
    #     # Return True if we end in our accept state, False otherwise
    #     for command in commands:
    #         if command == '1':
    #             self.state = 'q2'
    #             continue     
    #         if self.state == 'q2':
    #             self.state = 'q3'
    #             continue
    #         if self.state == 'q3':
    #             self.state = 'q2'
    #             continue
    #     if self.state=='q2':
    #         return True
    #     return False
        
my_automaton = Automaton()


test.assert_equals(my_automaton.read_commands(["1", "0", "0", "1", "0"]), False)
test.assert_equals(my_automaton.read_commands(["1", "0", "0", "1", "0", "0"]), True)
test.assert_equals(my_automaton.read_commands(["1"]), True)
test.assert_equals(my_automaton.read_commands(["1", "0", "0", "1"]), True)
test.assert_equals(my_automaton.read_commands(['1', '0']), False)


# Do anything necessary to set up your automaton's states, q1, q2, and q3.