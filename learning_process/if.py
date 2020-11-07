#if block
number=12
guess=int((input("please input an integer :")))

if guess == number:
    print("congratulations you are right.")
elif guess < number:
    print("I'm sorry, your num is little lower than that.")
else:
    print("No,your num is higher than that." )

'''当 Python 完整执行了 if 语句及与其相关的 elif 和 else 子句后，它将会移动至包含
if 语句的代码块的下一句语句中。在本例中，也就是主代码块（程序开始执行的地方），
其下一句语句就是 print('It's finish,thx.') 语句。在完成这些工作后，Python 会发现已行至程序末尾
并宣告工作的完成。
'''

print("It's finish,thx.")