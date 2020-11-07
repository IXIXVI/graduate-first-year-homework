#the while statement of looping 
number = 23
running = True

while running : #== True(while running 默认running为true时执行循环)
    guess =input("please input a intger (input 'quit' to break): ")

    if guess == 'quit':
        break
    else:
        guess=int(guess)
        if guess == number:
            print("Congratulations,it is",guess,"\nyou guess it!")
            running = False
        elif  guess < number:
            print('No',guess,' is a little lower than that')
        else:
            print("sorry",guess," is to high!")

else:
    print("the while loop is over.")

print('done')