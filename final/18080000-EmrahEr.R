# -------------------------------------------------------------------------- ###
# Question 1a ---- https://github.com/Gandil312740/21080647.git
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 2a ----
library(dplyr)

grouped_data <- titanic %>% group_by(gender)

average_fare <- grouped_data %>% summarize(avg_fare = mean(fare))

comparison <- average_fare %>% mutate(women_or_men = ifelse(avg_fare[women] > avg_fare[men], "women", "men"))

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 2b ----

library(tidyverse)

filtered_data <- titanic %>% filter(!is.na(age))

age_plot <- ggplot(data = filtered_data)

age_plot <- age_plot + geom_boxplot(aes(x = sex, y = age))

age_plot <- age_plot + labs(x = "Sex", y = "Age", title = "Boxplot of Ages by Sex")

age_plot

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 2c ----
hist(age, breaks = 30, col = "lightblue", xlab = "Density", ylab = "Age", main = "Histogram of Age with Density")

abline(v = density, col = "lightpink", lwd = 2)

legend("topright", legend = "Density", col = "lightpink", lwd = 2)

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 3a ----


x <- 10:20
x

subset <- x[seq(1, 5, by = 3)]
subset

Output: [1] 10 13

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 3b ----

library(dplyr)

dt3 <- inner_join(dt1, dt2)

dt3
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 3c ----

library(ggplot2)

ggplot(dt, aes(x = b, y = a)) +
  geom_point()

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 3d ----

library(purrr)

mylist <- list(1:3, c(3:5, NA))
myresult <- map(mylist, ~ mean(.x, na.rm = TRUE)) %>% unlist()

myresult

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 3e ----

mu <- 10
sigma <- 2
sample_data <- rnorm(25, mean = mu, sd = sigma)

Z <- 5 * (sample_data - mu) / sd(sample_data)

prob_Z_leq_1 <- pnorm(1, mean = 0, sd = 1)

prob_Z_leq_1

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 3f ----

roll_dice <- function() {
  dice1 <- sample(1:6, 1, replace = TRUE)
  dice2 <- sample(1:6, 1, replace = TRUE)

  cat("Dice 1:", dice1, "\n")
  cat("Dice 2:", dice2, "\n")
  cat("Total:", dice1 + dice2, "\n")
}

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 3g ----

survivors <- titanic$titanic_survived == 1
non_survivors <- titanic$titanic_survived == 0

result <- t.test(titanic$titanic_age[survivors], titanic$titanic_age[non_survivors], var.equal = TRUE)

print(result)

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 4a ----

library(dplyr)

dat <- tibble(
  country = c("England", "Germany", "England", "Germany", "England", "Germany"),
  year = c(2018, 2018, 2019, 2019, 2020, 2020),
  gdp = c(8000, 10000, 8100, 11000, 8500, 10200),
  population = c(80, 100, 90, 110, 100, 120)
)

dat2 <- dat %>%
  filter(country == "Germany") %>%
  select(country, year, gdp, population)

dat2

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Question 5a ----

library(tidyr)

dat <- tibble(
  country = c("Turkey", "Greece"),
  `<2018` = c(8000, 10000),
  `2019` = c(8100, 11000),
  `2020` = c(8500, 10200)
)

dat2 <- dat %>%
  pivot_longer(cols = starts_with("`"), names_to = "year", values_to = "gdp")

dat2

# -------------------------------------------------------------------------- ###
