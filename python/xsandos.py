def xo(s):
    # xs = len([c for c in s if c in ('Xx')])
    # xo = len([c for c in s if c in ('Oo')])
    # return xs == xo
    count = 0
    for c in s:
        if c in ('Xx'): count += 1
        if c in ('Oo'): count -= 1
    return count==0


    # XO_list = list(s)
    # X_list = ((XO_list.count("x")) + (XO_list.count("X")))
    # O_list = ((XO_list.count("o")) + (XO_list.count("O")))
    # if int(X_list) == int(O_list):
    #     return(True)
    # if int(X_list) != int(O_list):
    #     return(False)
    
teststring = 'xoxoxo'
output = xo(teststring)
print(output)
