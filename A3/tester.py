outputList = []
"""
n = 1024.0
b = 2.0
m = 1.0
s = 0.0
p = 1.0
list =[]
while n > 0.0:
    q = n/b
    r = n - q*b
    list.append(r)
    s = p*r+s                    
    p = p*10.0
    n = q
list.append(s)
print(list)
"""

a = 4
b = 4
#a comment
s = 3
while a > 0 :
    c = a**s
    d = 2**a
    outputList.append(c) 
    outputList.append(d)
    a = a-1
outputList.append(a)

print(outputList)