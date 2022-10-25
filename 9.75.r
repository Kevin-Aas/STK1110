#a)
z_alpha = qnorm(0.99, 1, 0)
p_0 = 0.2
p_m = 0.21

n = c(100, 2500, 10000, 40000, 90000)

for (n_i in n){
    teller = p_0 - p_m + z_alpha * sqrt(p_0 * (1 - p_0) / n_i)
    nevner = sqrt(p_m * (1 - p_m) / n_i)
    print(pnorm(teller / nevner))
}
print("--------")
#b)
p_0 = 0.2
p_h = 0.21

n = c(100, 2500, 10000, 40000)

for (n_i in n){
    z = (p_h - p_0) / sqrt(p_0 * (1 - p_0) / n_i)
    P_verdi = 1 - pnorm(z)
    print(P_verdi)
}