url = "https://www.uio.no/studier/emner/matnat/math/STK1110/data/exe8-61.txt"
time_table = read.table(url, header = TRUE)
#print(time_table)
time_data = time_table$time
#print(time_data)
qqline(time_data)

s = sd(time_data)
print(s)