#the for statement of looping which mainly to lterate all the objects
running=True

while running:
    uid=input("please input your ID(input 'quit' to break)")
     
    if uid == 'quit':
        break
    if len(uid) < 3:
        print('to small')
        continue#跳过该while block的下面部分，重新迭代一次
    else:
        print("input is of sufficient length")
        running=False  #循环语句一定要有break机制，这里可以写break也可以赋值running=False
