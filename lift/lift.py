import codewars_test as test
from icecream import ic
import builtins
import functools
import copy

debug = True

def conditional_print(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        if debug:
            return func(*args, **kwargs)
        else:
            original_print = builtins.print
            builtins.print = lambda *args, **kwargs: None
            try:
                result = func(*args, **kwargs)
            finally:
                builtins.print = original_print
            return result
    return wrapper


class Dinglemouse(object):

    def __init__(self, queues, capacity):
        self.queue = queues
        self.capacity = capacity

    def get_calls(self, queue, direction):
        calls = []
        for i, q in enumerate(queue[::direction]):
            if direction == 1:
                j = i
            else:
                j = len(queue) - i - 1
            if len(q) > 0:
                # need to check that at least one request is in the direction of travel
                if direction == 1:
                    if max(q) > j:
                        calls.append(j)
                else:
                    if min(q) < j:
                        calls.append(j)
        return calls
    
    def sort_calls(self, calls, direction):
        if direction == 1:
            return sorted(set(calls))
        return sorted(set(calls), reverse=True)
  
    @conditional_print  
    def theLift(self):
        queue = [list(q) for q in self.queue]
        if sum([len(q) for q in queue]) == 0:
            return [0]
        capacity = self.capacity
        print("Let the elevator ride begin!")
        direction = 1
        floors = []
        in_lift = []
        num_in_lift = 0
        calls = self.get_calls(queue, direction)
        while True:
            print("Queue is: ", queue, "Capacity is: ", capacity)
            if len(calls) == 0:
                direction = -direction
                calls = self.get_calls(queue, direction)
                if sum([len(q) for q in queue]) == 0:
                    break
                else:
                    continue          
            if direction == 1:
                print("Going up!")
            else:
                print("Going down!")
            print("calls: ", calls)
            cur_floor = calls[0]
            if len(floors) == 0 or floors[-1] != cur_floor:
                floors.append(cur_floor)
            print("current floor: ", cur_floor)
            in_lift = [p for p in in_lift if p != cur_floor]
            num_in_lift = len(in_lift)
            print("People in lift: ", in_lift)

            # add people into lift if waiting
            queue_cur_floor = copy.copy(queue[cur_floor])
            if len(queue_cur_floor) > 0:
                for p in queue_cur_floor:
                    if ((direction == 1 and p > cur_floor) or (direction == -1 and p < cur_floor)) \
                            and (num_in_lift < capacity):
                        in_lift.append(p)
                        calls.append(p)
                        num_in_lift += 1
                        queue[cur_floor].remove(p)
                        print(f"Person requesting {p} got on the lift, {num_in_lift} people in lift")
            calls = self.sort_calls(calls, direction)

            # queue[cur_floor] = [p for p in queue[cur_floor] if p not in in_lift]
            # print("People in lift: ", in_lift)
            print("People waiting: ", queue[cur_floor])
            print("People getting off: ", [p for p in in_lift if p == cur_floor])
            # remove people from lift if at their floor
            in_lift = [p for p in in_lift if p != cur_floor]
            num_in_lift = len(in_lift)
            print("People in lift: ", in_lift)
            # remove most recent call
            calls.pop(0)
        if floors[-1] != 0:
            floors.append(0)
        if floors[0] != 0:
            floors.insert(0, 0)
        print("Floors visited", floors)
        return floors
    
# start at ground going up
# which floor has called the elevator and direction (x)
# go to next floor in direction x allowing people to get on until capacity
# stop at floors even if at capacity
# allow people off if at requested floor
# if no more people or calls change direction or return to ground


# Floors:    G     1      2        3     4      5      6         Answers:
cap = 5
# tests = [[ ( (),   (),    (5,5,5), (),   (),    (),    () ),     [0, 2, 5, 0]          ],
#          [ ( (),   (),    (1,1),   (),   (),    (),    () ),     [0, 2, 1, 0]          ],
#          [ ( (),   (3,),  (4,),    (),   (5,),  (),    () ),     [0, 1, 2, 3, 4, 5, 0] ],
#          [ ( (),   (0,),  (),      (),   (2,),  (3,),  () ),     [0, 5, 4, 3, 2, 1, 0] ]]
# tests = [[[[3, 3, 3, 3, 3, 3], [], [], [], [], [], []], [0, 3, 0, 3, 0]],
#          [[[], [0, 0, 0, 6], [], [], [], [6, 6, 0, 0, 0, 6], []], [0, 1, 5, 6, 5, 1, 0, 1, 0] ]]

# [[], [], [4, 4, 4, 4], [], [2, 2, 2, 2], [], []], capacity 2 should equal [0, 2, 4, 2, 4, 2, 0]

cap = 1
tests = [ [ [[], [2], [3, 3, 3], [1], [], [], []] , [0, 1, 2, 3, 1, 2, 3, 2, 3, 0] ]]
  
for queues, answer in tests:
    lift = Dinglemouse(queues, cap)
    test.assert_equals(lift.theLift(), answer)

