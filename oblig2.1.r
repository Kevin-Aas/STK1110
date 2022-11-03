temp_menn = c(36.1, 36.3, 36.4, 36.6, 36.6, 36.7, 36.7, 37.0, 36.5, 37.1)
temp_kvin = c(36.6, 36.7, 36.8, 36.8, 36.7, 37.0, 37.1, 37.3, 36.9, 37.4)

m = length(temp_menn)
n = length(temp_kvin)

boxplot(temp_menn, temp_kvin, 
        names= c("Menn", "Kvinner"),
        ylab = "Temperatur [°C]",
        main = "Boksplott av temperatur målingene for menn og kvinner")

qqnorm(temp_menn, main="Normalplott av menn temperatur", col="#000000", pch=20)
qqline(temp_menn, col="#314bbe", lwd=2)

qqnorm(temp_kvin, main="Normalplott av kvinne temperatur", col="#000000", pch=20)
qqline(temp_kvin, col="#314bbe", lwd=2)

x.bar = mean(temp_menn)
y.bar = mean(temp_kvin)

s1 = sd(temp_menn)
s2 = sd(temp_kvin)
#s_p = sqrt((s1^2 + s2^2)/2)
s_p = s1

t_p = (x.bar - y.bar) / (s_p * sqrt(1/m + 1/n))
print(t_p)

t = (x.bar - y.bar) / sqrt(s1^2/m + s2^2/n)
print(t)

t.test(temp_menn, temp_kvin)

print(s1^2/s2^2)
var.test(temp_menn, temp_kvin)

welch = (s1^2/m + s2^2/n)^2 / ((s1^2/m)^2 / (m - 1) + (s2^2/n)^2 / (n - 1))
print(welch)

mean(temp_menn)
sd(temp_menn)
mean(temp_kvin)
sd(temp_kvin)

#f)
x = temp_kvin
y = temp_menn
n = length(x)
s = sd(x)
t.crit = 2.262
x.bar = mean(x)
y.bar = mean(y)
lower = (x.bar - y.bar) - t.crit * s * sqrt(1 + 1/n)
upper = (x.bar - y.bar) + t.crit * s * sqrt(1 + 1/n)
print(c(lower, upper))
