# Leser data fra web:
url <- "https://www.uio.no/studier/emner/matnat/math/STK1110/data/exe7_04.txt"
IQ.data <- read.table(url, header = TRUE)
print(IQ.data)

IQ.menn <- IQ.data$VERBIQ[IQ.data$mf == "male"]
IQ.kvinner <- IQ.data$VERBIQ[IQ.data$mf == "female"]

IQ.diff <- mean(IQ.menn) - mean(IQ.kvinner)
print(IQ.diff)

var.diff = var(IQ.menn) / length(IQ.menn) + var(IQ.kvinner) / length(IQ.kvinner)
se.diff = sqrt(var.diff)
print(se.diff)

se.ratio <- sqrt(var(IQ.menn) / var(IQ.kvinner))
print(se.ratio)

print(IQ.menn - IQ.kvinner)
diff.se <- sqrt(var(IQ.menn - IQ.kvinner))
print(diff.se)