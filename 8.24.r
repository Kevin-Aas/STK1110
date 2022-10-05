# a)
url <- "https://www.uio.no/studier/emner/matnat/math/STK1110/data/exe8-24.txt"
ACT_data <- read.table(url, header = TRUE)

ACT_scores <- ACT_data$ACT
#print(ACT_scores)
plot(density(ACT_scores))

#b)
mean = mean(ACT_scores)
std = sd(ACT_scores)
print("mean:", mean)
print("std:", std)
