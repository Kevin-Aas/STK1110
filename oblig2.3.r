p1 = 0.162
p2 = 0.147
m = 3000
n = 3000

p = m/(m+n) * p1 + n/(m+n) * p2
q = 1 - p

z = (p1 - p2) / sqrt(p*q*(1/m + 1/n))
print(z)

p = 2*(1-pnorm(1.607))
print(p)

prop.test(x = c(486, 441), n = c(3000, 3000))
