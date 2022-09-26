url = "https://www.uio.no/studier/emner/matnat/math/STK1110/data/exe8-28.txt"
cadence_data = read.table(url, header = TRUE)
cadence_maal = cadence_data$cadence

plot(density(cadence_maal))

print("mean:")
print(mean(cadence_maal))
print("sd:")
print(sd(cadence_maal))