import codewars_test as test
from icecream import ic

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
            ic(i, j, q, direction, calls)
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
  

    def theLift(self):
        queue = [list(q) for q in self.queue]
        capacity = self.capacity
        print("Let the elevator ride begin!")
        ic(queue)
        ic(capacity)
        direction = 1
        floors = [0]
        in_lift = []
        calls = sorted(set(self.get_calls(queue, direction)))
        while True:
            if len(calls) == 0:
                # reverse direction
                direction = -direction
                calls = self.get_calls(queue, direction)
                if len(calls) == 0:
                    break          
            if direction == 1:
                print("Going up!")
            else:
                print("Going down!")
            print("calls: ", calls)
            cur_floor = calls[0]
            floors.append(cur_floor)
            print("current floor: ", cur_floor)
            # add people into lift if waiting
            if len(queue[cur_floor]) > 0:
                for p in queue[cur_floor]:
                    if (direction == 1 and p > cur_floor) or (direction == -1 and p < cur_floor):
                        in_lift.append(p)
                        calls.append(p)
                        print("Person requesting {} got on the lift".format(p))
            calls = self.sort_calls(calls, direction)
            queue[cur_floor] = [p for p in queue[cur_floor] if p not in in_lift]
            # print("People in lift: ", in_lift)
            print("People waiting: ", queue[cur_floor])
            print("People getting off: ", [p for p in in_lift if p == cur_floor])
            # remove people from lift if at their floor
            in_lift = [p for p in in_lift if p != cur_floor]
            print("People in lift: ", in_lift)
            # remove most recent call
            calls.pop(0)
        if floors[-1] != 0:
            floors.append(0)
        print("Floors visited", floors)
        return floors
    
# start at ground going up
# which floor has called the elevator and direction (x)
# go to next floor in direction x allowing people to get on until capacity
# stop at floors even if at capacity
# allow people off if at requested floor
# if no more people or calls change direction or return to ground


# Floors:    G     1      2        3     4      5      6         Answers:
tests = [[ ( (),   (),    (5,5,5), (),   (),    (),    () ),     [0, 2, 5, 0]          ],
         [ ( (),   (),    (1,1),   (),   (),    (),    () ),     [0, 2, 1, 0]          ],
         [ ( (),   (3,),  (4,),    (),   (5,),  (),    () ),     [0, 1, 2, 3, 4, 5, 0] ],
         [ ( (),   (0,),  (),      (),   (2,),  (3,),  () ),     [0, 5, 4, 3, 2, 1, 0] ]]


# tests =   [[ ( (),   (0,),  (),      (),   (2,),  (3,),  () ),     [0, 5, 4, 3, 2, 1, 0] ]]
  
  
for queues, answer in tests:
    lift = Dinglemouse(queues, 5)
    ic("end")
    test.assert_equals(lift.theLift(), answer)

# lift = Dinglemouse(tests[0][0],5)
# ic(lift.theLift())