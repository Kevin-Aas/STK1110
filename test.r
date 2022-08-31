x <- NULL
for (i in 1:10000) {
    u <- runif(1)
    if (u < 0.05) {
        x[i] <- 16
    }else if (u < 0.15) {
        x[i] <- 32
    }else if (u < 0.50) {
        x[i] <- 64
    }else if (u < 0.90) {
        x[i] <- 128
    }else {
        x[i] <- 256
    }
}
#hist(x)
values_hist <- hist(x, plot = FALSE)    # Histogram with percent
values_hist$density <- 100 *
  (values_hist$counts / sum(values_hist$counts))
plot(values_hist, freq = FALSE,
     ylab = "Probability",
     xlab = "GB")