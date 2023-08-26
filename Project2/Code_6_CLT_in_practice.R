# CTL in practice
# ChatGPT example, roll dices

# Number of times to roll the dice
num_rolls <- 1000

# Function to simulate rolling two dice and calculating the sum
simulate_rolls <- function(n) {
  dice1 <- sample(1:6, n, replace = TRUE)
  dice2 <- sample(1:6, n, replace = TRUE)
  return(dice1 + dice2)
}

# Simulate rolling two dice and calculating the sum
sums <- simulate_rolls(num_rolls)

# Calculate the mean of the sums
mean_of_sums <- mean(sums)

# Plot a histogram of the calculated means
hist(sums, breaks = 20, main = "Histogram of Sum of Two Dice Rolls", 
     xlab = "Sum", ylab = "Frequency", col = "lightblue", border = "black")
abline(v = mean_of_sums, col = "red", lwd = 2)
