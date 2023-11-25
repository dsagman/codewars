#https://www.codewars.com/kata/5571d9fc11526780a000011a/train/python

import codewars_test as test
from icecream import ic



class Thing:
    def __init__(self, name):
        self.name = name
        self._attributes = {}

    def __getattr__(self, name):
        if name.startswith('is_a_'):
            return self._attributes.get(name[5:], False)
        if name.startswith('is_not_a_'):
            return not self._attributes.get(name[9:], False)
        if name == 'is_a':
            return AttributeSetter(self._attributes, True)
        if name == 'is_not_a':
            return AttributeSetter(self._attributes, False)
    
        raise AttributeError(f"'{type(self).__name__}' object has no attribute '{name}'")

class AttributeSetter:
    def __init__(self, attributes, value):
        self._attributes = attributes
        self._value = value

    def __getattr__(self, name):
        self._attributes[name] = self._value
        return self._value

    


@test.describe('jane =  Thing("Jane")')
def _():

    @test.it('should be "Jane"')
    def _():
        jane = Thing('Jane')
        test.assert_equals(jane.name, 'Jane')

@test.describe('#is_a')
def _():

    @test.describe('is_a.woman (dynamic key)')
    def _():
        jane = Thing('Jane')
        jane.is_a.woman

        @test.it('jane.is_a_woman should return true')
        def _():
            test.assert_equals(jane.is_a_woman, True)

@test.describe('#is_not_a')
def _():

    @test.describe('is_a.woman (dynamic key)')
    def _():
        jane = Thing('Jane')
        jane.is_a.woman

        @test.it('jane.is_a_woman should return true')
        def _():
            test.assert_equals(jane.is_a_woman, True)

@test.describe('#is_not_a')
def _():

    @test.describe('is_not_a.man (dynamic key)')
    def _():

        @test.it('jane.is_a_man should return false')
        def _():
            jane = Thing('Jane')
            jane.is_not_a.man
            test.assert_equals(jane.is_a_man, False)

# @test.describe('#has')
# def _():

#     @test.describe('jane.has(2).arms')
#     def _():
#         jane = Thing('Jane')
#         jane.has(2).arms

#         @test.it('should define an arms method that is tuple subclass')
#         def _():
#             test.assert_equals(isinstance(jane.arms, tuple), True)

#         @test.it('should populate 2 new Thing instances within the tuple subclass')
#         def _():
#             test.assert_equals(len(jane.arms), 2)

#             test.assert_equals(all(isinstance(v, Thing) for v in jane.arms), True)

#         @test.it('should call each thing by its singular form (aka "arm")')
#         def _():
#             test.assert_equals(all(v.name=="arm" for v in jane.arms), True)

#         @test.it('should have is_arm == true for each arm instance')
#         def _():
#             test.assert_equals(all(v.is_arm for v in jane.arms), True)

#     @test.describe('jane.having(2).arms (alias)')
#     def _():

#         @test.it('should populate 2 new Thing instances within the tuple subclass')
#         def _():
#             jane = Thing('Jane')
#             jane.having(2).arms
#             test.assert_equals(len(jane.arms), 2)
#             test.assert_equals(all(isinstance(v, Thing) for v in jane.arms), True)

#     @test.describe('jane.has(1).head')
#     def _():
#         jane = Thing('Jane')
#         jane.has(1).head

#         @test.it('should define head method that is a reference to a new Thing')
#         def _():
#             test.assert_equals(isinstance(jane.head, Thing), True)

#         @test.it('should name the head thing "head"')
#         def _():
#             test.assert_equals(jane.head.name, "head")

#     @test.describe('jane.has(1).head.having(2).eyes')
#     def _():
#         jane = Thing('Jane')
#         jane.has(1).head.having(2).eyes

#         @test.it('should create 2 new things on the head')
#         def _():
#             test.assert_equals(len(jane.head.eyes), 2)
#             test.assert_equals(all(isinstance(v, Thing) for v in jane.head.eyes), True)

#         @test.it('should name the eye things "eye"')
#         def _():
#             test.assert_equals(all(v.name=='eye' for v in jane.head.eyes), True)


# @test.describe('#each')
# def _():
#     @test.describe('jane.has(2).arms.each.having(5).fingers')
#     def _():
#         jane = Thing('Jane')
#         jane.has(2).arms.each.having(5).fingers

#         @test.it('should cause 2 arms to be created each with 5 fingers')
#         def _():
#             test.assert_equals(all(len(v.fingers)==5 for v in jane.arms), True)


# @test.describe('#is_the')
# def _():

#     @test.describe('jane.is_the.parent_of.joe')
#     def _():
#         jane = Thing('Jane')
#         jane.is_the.parent_of.joe

#         @test.it('should set jane.parent_of == "joe"')
#         def _():
#             test.assert_equals(jane.parent_of, "joe")

# @test.describe('#being_the')
# def _():

#     @test.describe('jane.has(1).head.having(2).eyes.each.being_the.color.blue')
#     def _():
#         @test.it("jane's eyes should both be blue")
#         def _():
#             jane = Thing('Jane')
#             jane.has(1).head.having(2).eyes.each.being_the.color.blue
#             test.assert_equals(all(v.color=='blue' for v in jane.head.eyes), True)

#     @test.describe('jane.has(2).eyes.each.being_the.color.blue.and_the.shape.round')
#     def _():
#         @test.it('should allow chaining via the and_the method')
#         def _():
#             jane = Thing('Jane')
#             jane.has(2).eyes.each.being_the.color.blue.and_the.shape.round
#             test.assert_equals(all(v.color=='blue' for v in jane.eyes), True)
#             test.assert_equals(all(v.shape=='round' for v in jane.eyes), True)

# @test.describe('jane.has(2).eyes.each.being_the.color.green.having(2).pupils.each.being_the.color.black')
# def _():
#     @test.it('should allow nesting by using having')
#     def _():
#         jane = Thing('Jane')
#         jane.has(2).eyes.each.being_the.color.green.having(1).pupil.being_the.color.black
#         test.assert_equals(all(v.color=='green' for v in jane.eyes), True)
#         test.assert_equals(all(v.pupil.color=='black' for v in jane.eyes), True)


# @test.describe('#can')
# def _():
#     @test.describe('jane.can.speak(lambda phrase: "#%s says: #%s" % (name, phrase))')
#     def _():
#         jane = Thing('Jane')

#         def fnc(phrase): 
#             return "%s says: %s" % (name, phrase)

#         jane.can.speak(fnc)

#         @test.it('should create a speak method on the instance')
#         def _():
#             test.assert_equals(jane.speak('hi'), "Jane says: hi")

#     @test.describe('jane.can.speak(lambda phrase: "#%s says: #%s" % (name, phrase), "spoke")')
#     def _():
#         jane = Thing('Jane')
#         fnc = lambda phrase: "%s says: %s" % (name, phrase)
#         jane.can.speak(fnc, 'spoke')
#         jane.speak('hi')

#         @test.it('should add a "spoke" attribute that tracks all speak call results')
#         def _():
#             test.assert_equals(jane.spoke, ["Jane says: hi"])
#             jane.speak('goodbye')
#             test.assert_equals(jane.spoke, ["Jane says: hi", "Jane says: goodbye"])