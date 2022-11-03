url = "https://www.uio.no/studier/emner/matnat/math/STK1110/data/snoe_vann.txt"
data = read.table(url, header=FALSE)

snø = data$V1
vann = data$V2

lin.fit = lm(vann ~ snø)
print(lin.fit)

plot(snø, vann, 
    xlab="Snømengde [vannekvivalens]",
    ylab="Vannstand  [tommer]",
    main="Plott av observasjonene og regresjonslinjen",
    pch=20, col="#0084ff")
abline(lin.fit, col="#ff5100", lwd=2)

#b)
# Finner standarissert residualene og plotter disse
stan.res = rstandard(lin.fit)
plot(snø, stan.res,
    xlab="Snømengde [vannekvivalens]",
    ylab="Standardissert residualer",
    pch=20, col="#0084ff")

qqnorm(stan.res, col="#000000", pch=20)
qqline(stan.res, col="#314bbe", lwd=2)

#c)
n = length(snø)
y = vann
y.hat = lin.fit$fitted.values
SSE = sum((y - y.hat)^2)
s.e2 = SSE/(n-2)
print(s.e2)

x = snø
beta = 0.5056
t.crit = qt(0.025, n-2, lower.tail = FALSE)
Sxx = sum((x - mean(x))^2)
lower = beta - t.crit * sqrt(s.e2)/sqrt(Sxx)
upper = beta + t.crit * sqrt(s.e2)/sqrt(Sxx)
print(c(lower, upper))

#d)
beta0 = 0.28
s_b0 = sqrt(s.e2)*sqrt(1/n + mean(x)^2/Sxx)
t = beta0 / s_b0
print(t)

p = 2*(1-pt(0.164, 16))
print(p)
lm(y~x, data)
