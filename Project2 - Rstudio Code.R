# Step 1: Enter home sale prices into a vector
prices <- c(525000, 474900, 399000, 374999, 699000, 889900, 459905, 895000,
            435045, 499000, 418605, 385000, 456045, 415305, 433305, 859000,
            472930, 472780, 493730, 470580, 761000, 491530, 473530, 471630,
            809000)
# Step 2: Calculate mean and standard deviation
mean_price <- mean(prices)
std_dev <- sd(prices)

# Print the results
mean_price
std_dev

# Enter the home sale prices
prices <- c(525000, 474900, 399000, 374999, 699000, 889900, 459905, 895000,
            435045, 499000, 418605, 385000, 456045, 415305, 433305, 859000,
            472930, 472780, 493730, 470580, 761000, 491530, 473530, 471630,
            809000)

# Calculate sample mean
mean_price <- mean(prices)

# Calculate sample standard deviation (uses n - 1)
std_dev <- sd(prices)

# Print results
mean_price
std_dev


prices <- c(525000, 474900, 399000, 374999, 699000, 889900, 459905, 895000,
            435045, 499000, 418605, 385000, 456045, 415305, 433305, 859000,
            472930, 472780, 493730, 470580, 761000, 491530, 473530, 471630,
            809000)

mean_price <- mean(prices)
std_dev <- sd(prices)
n <- length(prices)
z_star <- qnorm(0.95)

ebm <- z_star * std_dev / sqrt(n)
lower_bound <- mean_price - ebm
upper_bound <- mean_price + ebm

c(lower_bound, upper_bound)


x_vals <- seq(mean_price - 3*std_dev, mean_price + 3*std_dev, length=1000)
y_vals <- dnorm(x_vals, mean=mean_price, sd=std_dev/sqrt(n))

x_vals <- seq(mean_price - 3*std_dev, mean_price + 3*std_dev, length=1000)
y_vals <- dnorm(x_vals, mean=mean_price, sd=std_dev/sqrt(n))



library(ggplot2)

x_vals <- seq(mean_price - 3*std_dev, mean_price + 3*std_dev, length=1000)
y_vals <- dnorm(x_vals, mean=mean_price, sd=std_dev/sqrt(n))

plot(x_vals, y_vals, type="l", lwd=2, main="90% Confidence Interval", xlab="Price", ylab="Density")
abline(v=c(lower_bound, upper_bound), col="blue", lty=2)
polygon(c(lower_bound, seq(lower_bound, upper_bound, length=100), upper_bound),
        c(0, dnorm(seq(lower_bound, upper_bound, length=100), mean_price, std_dev/sqrt(n)), 0),
        col=rgb(0,0,1,0.2), border=NA)

# Sample statistics
mean_price <- mean(prices)
std_error <- sd(prices) / sqrt(length(prices))

# Confidence interval (90%)
z_star <- qnorm(0.95)
lower_bound <- mean_price - z_star * std_error
upper_bound <- mean_price + z_star * std_error

# Create x-values for normal curve
x_vals <- seq(mean_price - 4*std_error, mean_price + 4*std_error, length.out = 1000)
y_vals <- dnorm(x_vals, mean = mean_price, sd = std_error)

# Plot the normal curve
plot(x_vals, y_vals, type = "l", lwd = 2, col = "black",
     main = "90% Confidence Interval for Mean Home Price",
     xlab = "Home Sale Price", ylab = "Density")

# Add shaded area for 90% CI
x_shade <- seq(lower_bound, upper_bound, length.out = 100)
y_shade <- dnorm(x_shade, mean = mean_price, sd = std_error)
polygon(c(lower_bound, x_shade, upper_bound),
        c(0, y_shade, 0),
        col = rgb(0, 0, 1, 0.2), border = NA)

# Add vertical lines for the bounds
abline(v = c(lower_bound, upper_bound), col = "blue", lty = 2)


