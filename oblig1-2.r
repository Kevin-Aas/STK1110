data = c(525,587,547,558,591,531,571,551,566,622,561,502,556,565,562)

n = length(data)

# a)
print(n)
print(mean(data))
print(sd(data))
print(2.145*sd(data)/sqrt(n))
print(mean(data) - 2.145*sd(data)/sqrt(n))
print(mean(data) + 2.145*sd(data)/sqrt(n))

# b)
set.seed(1)
m <- 10000
conf_list <- c()
count <- 0
for (i in 1:m) {
    sim_data <- rnorm(n, 558, 30)
    sim_mean <- mean(sim_data)
    sim_sd   <- sd(sim_data)
    t_0.025  <- 2.145
    conf_low <- sim_mean - t_0.025 * sim_sd / sqrt(n)
    conf_hig <- sim_mean + t_0.025 * sim_sd / sqrt(n)
    if (conf_low < 558 && conf_hig > 558) {
        count <- count + 1
    }
    conf     <- c(conf_low, conf_hig)
    conf_list <- c(conf_list, conf, "|")
}
print(count)

# c)
m <- 10000
conf_list <- c()
count <- 0
for (i in 1:m) {
    sim_data <- rnorm(n, 558, 30)
    sim_mean <- mean(sim_data)
    sim_sd   <- sd(sim_data)
    conf_low <- sim_mean - 1.96 * sim_sd / sqrt(n)
    conf_hig <- sim_mean + 1.96 * sim_sd / sqrt(n)
    if (conf_low < 558 && conf_hig > 558) {
        count <- count + 1
    }
    conf     <- c(conf_low, conf_hig)
    conf_list <- c(conf_list, conf, "|")
}
print(count)

# d)
m <- 10000
conf_list <- c()
count <- 0
for (i in 1:m) {
    sim_data <- rnorm(n, 558, 30)
    sim_sd   <- sd(sim_data)
    chi_low  <- qchisq(p=0.05/2, df=n-1, lower.tail=FALSE)
    chi_high <- qchisq(p=1-(0.05/2), df=n-1, lower.tail=FALSE)
    conf_low <- sqrt((n-1) / chi_low * sim_sd^2)
    conf_hig <- sqrt((n-1) / chi_high * sim_sd^2)
    if (conf_low < 30 && conf_hig > 30) {
        count <- count + 1
    }
    conf     <- c(conf_low, conf_hig)
    conf_list <- c(conf_list, conf, "|")
}
print(count)

# e)
m <- 10000
conf_list <- c()
count <- 0
for (i in 1:m){
    n = 15
    tdist  <- rt(n, df=7)
    xdist  <- 558 + 30 * tdist
    x_mean <- mean(xdist)
    x_sd   <- sd(xdist)
    t_0.025  <- 2.145
    conf_low <- x_mean - t_0.025 * x_sd / sqrt(n)
    conf_hig <- x_mean + t_0.025 * x_sd / sqrt(n)
    if (conf_low < 558 && conf_hig > 558) {
        count <- count + 1
    }
    conf     <- c(conf_low, conf_hig)
    conf_list <- c(conf_list, conf, "|")
}
print(count)

# f)
m <- 10000
conf_list <- c()
count <- 0
for (i in 1:m) {
    n = 15
    tdist  <- rt(n, df=7)
    xdist  <- 558 + 30 * tdist
    x_sd   <- sd(xdist)
    chi_low  <- qchisq(p=0.05/2, df=n-1, lower.tail=FALSE)
    chi_high <- qchisq(p=1-(0.05/2), df=n-1, lower.tail=FALSE)
    conf_low <- sqrt((n-1) / chi_low * x_sd^2)
    conf_hig <- sqrt((n-1) / chi_high * x_sd^2)
    if (conf_low < sqrt(1.4)*30 && conf_hig > sqrt(1.4)*30) {
        count <- count + 1
    }
    conf     <- c(conf_low, conf_hig)
    conf_list <- c(conf_list, conf, "|")
}
print(count)
