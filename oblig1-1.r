url = "https://www.uio.no/studier/emner/matnat/math/STK1110/data/forsikringskrav.txt"
data_table = read.table(url, header = FALSE)
data = data_table$V1
n = length(data)

#qqplot av data
qqnorm(data)
qqline(data)
#Histrogram av data
hist(data)
#Mean og varians
print(mean(data))
print(sum((data - mean(data))^2) / length(data))

print(sd(data))
print(length(data))
print(1.960*sd(data)/sqrt(n))
print((n-1)/6600 * sd(data)^2)
print((n-1)/24.443 * sd(data)^2)

#qqplot av logaritmen til data
qqnorm(log(data))
qqline(log(data))
#Historgram av logaritmen til data
hist(log(data)) 
#Mean og varians av logaritmen
print(mean(log(data)))
print(log(sum((data - mean(data))^2) / length(data)))

#Bootstrap mean
B = 1000
data_mean_boot = c()
for (i in 1:B){
    data_boot = c()
    for (i in 1:length(data)){ # nolint
        krav = sample(data, 1)
        data_boot = append(data_boot, krav)
    }
    data_mean_boot = append(data_mean_boot, mean(data_boot))
}
data_mean_boot = log(data_mean_boot)
hist(data_mean_boot)
print(sqrt(1/(B-1) * sum((data_mean_boot - mean(data_mean_boot))^2)))
print(sd(data)/length(data))

#Bootstrap mean
B = 1000
data_sd_boot = c()
for (i in 1:B){
    data_boot = c()
    for (i in 1:length(data)){ # nolint
        krav = sample(data, 1)
        data_boot = append(data_boot, krav)
    }
    data_sd_boot = append(data_sd_boot, mean(data_boot))
}
hist(data_sd_boot)
print(sd(data_sd_boot))
print(sqrt(2/length(data)) * sd(data)^2)

# Persentil konfidensintervall for varians:
data_sd_boot_sort = sort(data_sd_boot)
print(data_sd_boot_sort[25]^2)
print(data_sd_boot_sort[975]^2)