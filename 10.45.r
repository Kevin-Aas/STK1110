conventional = c(11, 14, 18, 22, 10, 16, 28, 20, 15, 14, 23, 17, 20) * 0.0001
perforated   = c(11, 10, 19, 13, 11, 17, 24, 20, 13, 13, 17, 15, 13) * 0.0001

difference = conventional - perforated

d_mean = mean(difference)
d_sd = sd(difference)
