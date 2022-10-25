younger = c(29, 34, 33, 27, 28, 32, 31, 34, 32, 27)
older = c(18, 15, 23, 13 ,12)

m = 10
n = 5

x = mean(younger)
s1 = sd(younger)
y = mean(older)
s2 = sd(older)

print(x)
print(s1)
print(y)
print(s2)

t.test(younger, older)

