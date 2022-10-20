soil_data <-  c(1.10, 5.09, 0.97, 1.59, 4.60, 0.32, 0.55, 1.45,
                0.14, 4.47, 1.20, 3.50, 5.02, 4.67, 5.22, 2.69,
                3.98, 3.17, 3.03, 2.21, 0.69, 4.47, 3.31, 1.17,
                0.76, 1.17, 1.57, 2.62, 1.66, 2.05)

mean_soil <- mean(soil_data)
print(mean_soil)

sd_soil <- sd(soil_data)
print(sd_soil)

t_value <- (mean_soil - 3) / (sd_soil / sqrt(length(soil_data)))
print(T_value)

p_value <- 2 * (1 - pt(abs(t_value), df = length(soil_data) - 1))
print(p_value)
