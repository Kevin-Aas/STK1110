#a)
url = "https://www.uio.no/studier/emner/matnat/math/STK1110/data/exe8-65.txt"
weight_table = read.table(url, header = TRUE)
weight_data = weight_table$wtgain
print(weight_data)

print(mean(weight_data))
print(sd(weight_data))

#b)
#plot(density(weight_data))

#c)
B = 1000
weight_mean_boot = c()
for (i in 1:B){
    weight_boot = c()
    for (i in 1:length(weight_data)){ # nolint
        weigth = sample(weight_data, 1)
        weight_boot = append(weight_boot, weigth)
    }
    weight_mean_boot = append(weight_mean_boot, mean(weight_boot))
}
hist(weight_mean_boot)

#d)
print(sd(weight_mean_boot))

#e)
weight_mean_boot_sort = sort(weight_mean_boot)
print(weight_mean_boot_sort[25])
print(weight_mean_boot_sort[975])
