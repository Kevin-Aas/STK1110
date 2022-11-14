TOST = c(4200, 3600, 3750, 3675, 4050, 2770, 4870, 4500, 3450, 2700, 3750, 3300)
RBOT = c(370, 340, 375, 310, 350, 200, 400, 375, 285, 225, 345, 285)/60

n = length(TOST)

plot(TOST, RBOT)
mean(TOST)
mean(RBOT)
sd(TOST)
sd(RBOT)

S_xy = sum((TOST-mean(TOST)) * (RBOT-mean(RBOT)))

r = 1/(n-1) * S_xy / (sd(TOST) * sd(RBOT))
print(r)

qqnorm(TOST)
qqline(TOST)

qqnorm(RBOT)
qqline(RBOT)
