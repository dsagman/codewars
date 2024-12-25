10 for i = 0 to 300
20 for j = 0 to 300
30 x=i: y=j
50 if x-3*int(x/3) and y-3*int(y/3) then 90
60 x=int(x/3):y=int(y/3)
70 if x>0 and y>0 then 50
80 pset(i,j)
90 next j
100 next i
