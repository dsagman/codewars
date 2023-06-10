import sys
sys.path.append('/Users/dsagman/Code/github/python-test-framework') # mac
# sys.path.append('/home/agman/git/python-test-framework') # linux
import codewars_test as test
from icecream import ic
from itertools import cycle, islice

# The function must accept a non-negative integer. 
# If it is zero, it just returns "now". 
# Otherwise, the duration is expressed as a 
# combination of years, days, hours, minutes and seconds.

# Detailed rules
# The resulting expression is made of components like 4 seconds, 1 year, etc. In general, a positive integer and one of the valid units of time, separated by a space. 
# The unit of time is used in plural if the integer is greater than 1.

# The components are separated by a comma and a space (", "). 
# Except the last component, which is separated by " and ", 
# just like it would be written in English.
# A more significant units of time will occur before than a least significant one. 
# Therefore, 1 second and 1 year is not correct, but 1 year and 1 second is.
# Different components have different unit of times. 
# So there is not repeated units like in 5 seconds and 1 second.
# A component will not appear at all if its value happens to be zero. 
# Hence, 1 minute and 0 seconds is not valid, but it should be just 1 minute.
# A unit of time must be used "as much as possible". 
# It means that the function should not return 61 seconds, 
# but 1 minute and 1 second instead. 
# Formally, the duration specified by of a component 
# must not be greater than any valid more significant unit of time.

secPmin = 60
secPhour = secPmin*60
secPday = secPhour*24
secPyear = secPday*365

def time_format_helper(seconds, name, conversion):
    units = seconds//conversion
    unit_string = ""
    if units > 0:
        unit_string = str(units) + " " + name
    if units > 1:
        unit_string += "s"
    return unit_string, seconds - units*conversion

def format_duration(seconds):
    if seconds == 0: return "now"
    year_string, seconds = time_format_helper(seconds, "year" , secPyear)
    day_string, seconds = time_format_helper(seconds, "day" , secPday)
    hour_string, seconds = time_format_helper(seconds, "hour" , secPhour)
    minute_string, seconds = time_format_helper(seconds, "minute" , secPmin)
    second_string, seconds = time_format_helper(seconds, "second" , 1)
    tlist = [x for x in [year_string, day_string, hour_string, minute_string, second_string] if x != '']
    nterms = len(tlist)
    if nterms == 1:
        return tlist[0]
    if nterms == 2:
        return tlist[0]+" and "+tlist[1]
    else:
        return "".join([x+", " for x in tlist[0:nterms-2]])+tlist[-2]+" and "+tlist[-1]

test.assert_equals(format_duration(1), "1 second")
test.assert_equals(format_duration(62), "1 minute and 2 seconds")
test.assert_equals(format_duration(120), "2 minutes")
test.assert_equals(format_duration(3600), "1 hour")
test.assert_equals(format_duration(3662), "1 hour, 1 minute and 2 seconds")