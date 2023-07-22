import codewars_test as test

class Dinglemouse(object):

    def __init__(self, queues, capacity):
        self.queue = queues
        self.capacity = capacity

    def get_calls(self, queue, direction):
        calls = []
        for i, q in enumerate(queue[::direction]):
            if (len(q) > 0) and (direction == 1) and (max(q) > i):
                    calls.append(i)
            elif (len(q) > 0) and (direction == -1) and (min(q) < len(queue) - i - 1):
                    calls.append(len(queue) - i - 1)
        return calls
      
    def theLift(self):
        queue = [list(q) for q in self.queue]
        direction, floors, in_lift = 1, [], []
        calls = self.get_calls(queue, direction)
        while True:
            if not calls:
                direction = -direction
                calls = self.get_calls(queue, direction)
                continue
                if not any(queue):
                    break
                else:
                    continue          
            if not floors or floors[-1] != calls[0]:
                floors.append(calls[0])
            if not any(queue):
                break
            in_lift = [p for p in in_lift if p != calls[0]]
            for p in queue[calls[0]][:]:
                if ((direction and p > calls[0]) or (direction == -1 and p < calls[0])) \
                        and (len(in_lift) < self.capacity):
                    in_lift.append(p)
                    calls.append(p)
                    queue[calls[0]].remove(p)
            calls = sorted(set(calls), reverse=(direction == -1))
            calls.pop(0)
        if floors and floors[-1] != 0:
            floors.append(0)
        if  not floors or floors[0] != 0:
            floors.insert(0, 0)        
        return floors
    

# Floors:    G     1      2        3     4      5      6         Answers:
cap = 5
tests = [[ ( (),   (),    (5,5,5), (),   (),    (),    () ),     [0, 2, 5, 0]          ],
         [ ( (),   (),    (1,1),   (),   (),    (),    () ),     [0, 2, 1, 0]          ],
         [ ( (),   (3,),  (4,),    (),   (5,),  (),    () ),     [0, 1, 2, 3, 4, 5, 0] ],
         [ ( (),   (0,),  (),      (),   (2,),  (3,),  () ),     [0, 5, 4, 3, 2, 1, 0] ]]
tests = [[[[3, 3, 3, 3, 3, 3], [], [], [], [], [], []], [0, 3, 0, 3, 0]],
         [[[], [0, 0, 0, 6], [], [], [], [6, 6, 0, 0, 0, 6], []], [0, 1, 5, 6, 5, 1, 0, 1, 0] ]]

# [[], [], [4, 4, 4, 4], [], [2, 2, 2, 2], [], []], capacity 2 should equal [0, 2, 4, 2, 4, 2, 0]

# cap = 1
# tests = [ [ [[], [2], [3, 3, 3], [1], [], [], []] , [0, 1, 2, 3, 1, 2, 3, 2, 3, 0] ]]
  
for queues, answer in tests:
    lift = Dinglemouse(queues, cap)
    test.assert_equals(lift.theLift(), answer)

