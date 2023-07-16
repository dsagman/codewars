import codewars_test as test
from icecream import ic

class Debugger:
    method_calls = []
    attribute_accesses = []

class Meta(type):
    def __new__(mcls, name, bases, namespace):
        for attr_name, attr_value in namespace.items():
            if isinstance(attr_value, type(lambda: 0)):  # check if attribute is a function
                namespace[attr_name] = mcls.wrap_method(attr_name, attr_value)

        namespace['__getattribute__'] = mcls.getattribute
        namespace['__setattr__'] = mcls.setattr
        ic(namespace)
        return super().__new__(mcls, name, bases, namespace)

    @staticmethod
    def wrap_method(name, method):
        def wrapper(*args, **kwargs):
            Debugger.method_calls.append({
                'class': args[0].__class__,
                'method': name,
                'args': args,
                'kwargs': kwargs
            })
            return method(*args, **kwargs)
        return wrapper

    @staticmethod
    def getattribute(self, name):
        value = object.__getattribute__(self, name)
        if name != "__class__":
            Debugger.attribute_accesses.append({
                'action': 'get',
                'class': self.__class__,
                'attribute': name,
                'value': value
            })
        return value

    @staticmethod
    def setattr(self, name, value):
        Debugger.attribute_accesses.append({
            'action': 'set',
            'class': self.__class__,
            'attribute': name,
            'value': value
        })
        object.__setattr__(self, name, value)

class Foo(object, metaclass = Meta):
    def __init__(self, x):
        self.x = x

    def bar(self, v):
        return (self.x, v)


a = Foo(1)
# ic(a.x)
a.bar(2)
# ic(a.bar(2))
ic(Debugger.method_calls)
ic(Debugger.attribute_accesses)

calls = Debugger.method_calls

test.assert_equals(len(calls), 2)

test.describe("Test collected method calls")

test.it("Call to init should be collected")
test.assert_equals(calls[0]['args'], (a, 1))

test.it("Call to bar should be collected")
test.assert_equals(calls[1]['args'], (a, 2))

test.describe("Test collected attribute accesses")
accesses = Debugger.attribute_accesses

test.assert_equals(len(accesses), 3)

test.it("Attribute set in init should be collected")
test.assert_equals(accesses[0]['action'], 'set')
test.assert_equals(accesses[0]['attribute'], 'x')
test.assert_equals(accesses[0]['value'], 1)

test.it("Method get should be collected too")
test.assert_equals(accesses[1]['action'], 'get')
test.assert_equals(accesses[1]['attribute'], 'bar')

test.it("Attribute get should be collected")
test.assert_equals(accesses[2]['action'], 'get')
test.assert_equals(accesses[2]['attribute'], 'x')