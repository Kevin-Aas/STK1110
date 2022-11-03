p = 2*(1-pt(2.06, 30))
print(p)

n = 31
d.bar = -3.26
s.D = 8.81
t.crit = 2.042
lower = d.bar - t.crit * s.D/sqrt(n)
upper = d.bar + t.crit * s.D/sqrt(n)
print(c(lower, upper))
