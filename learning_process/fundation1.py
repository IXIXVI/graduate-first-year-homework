def print_max(a,b):
    if a > b:
        print(a," is maximum")
    elif a==b :
        print(a,'equal to',b)
    else:
        print(b,'is maximum')

x=50

def fanc1(x):
    print("x is ",x)
    x=2
    print('change x to',x)

def fanc2():
    global x
    print(' x is'x)
    x=2
    print("change x to ",x)