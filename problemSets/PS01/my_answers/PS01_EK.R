#####################
# load libraries
# set wd
setwd("~/Desktop/TCD/Quantitative Methods I/Repo/StatsI_Fall2024/problemSets/PS01/my_answers")
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stringr", "dplyr", "ggplot2", "tidyr"), pkgTest)

#####################
# Problem 1
#####################

# Random sample of IQ score of 25 students
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Find a 90% confidence interval for the average student IQ in the school.
# First calculate the mean of IQ scores and the std. deviation 
mean_y <- mean(y)
sd_y <- sd(y)

# And now the 90% CI (formula: mean(y) +/- z*(sd/sqrt(n)))
upper_90_y <- mean(y) + 1.645 * (sd_y/sqrt(length(y)))
lower_90_y <- mean(y) - 1.645 * (sd_y/sqrt(length(y))) 

# Print the output
cat("The 90% CI for the average student IQ score: (", lower_90_y, ", ", upper_90_y, ")\n")

# Next, the school counselor was curious whether the average student IQ in her school
# is higher than the average IQ score (100) among all the schools in the country.
# Using the same sample, conduct the appropriate hypothesis test with α = 0.05.

# Doing an one-sided t.test because we care about the IQ being higher than the average! 
result <- t.test(y, mu = 100, alternative = "greater")

# Print the output
result

# Check the p-value and compare with alpha
if (result$p.value < 0.05) {
  cat("We can reject the null hypothesis: The average IQ is significantly greater than 100.\n")
} else {
  cat("We cannot reject the null hypothesis: There is not enough evidence that the average IQ is greater than 100.\n")
}

#####################
# Problem 2
#####################

# Load the data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

# Variables
# Y = per capita expenditure on shelters/housing assistance in state
# X1 = per capita personal income in state
# X2 = Number of residents per 100,000 that are ”financially insecure” in state
# X3 = Number of people per thousand residing in urban areas in state
# Region: 1=Northeast, 2=North Central, 3=South, 4=West

# Explore the expenditure data set and import data into R.
head(expenditure)
summary(expenditure)

# Please plot the relationships among Y, X1, X2, and X3? What are the correlations
# among them (you just need to describe the graph and the relationships among them)?

# X1 
p1 <- ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "blue") +
  labs(x = "Per capita personal income", y = "Per capita expenditure on shelters/housing assistance") +
  theme_minimal()
ggsave("p1.png", p1)


# X2
p2 <- ggplot(expenditure, aes(x = X2, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red") +
  labs(x = "N. of financially insecure residents (per 100,000)",
       y = "Per capita expenditure on shelters/housing assistance") +
  theme_minimal()
ggsave("p2.png", p2)

# X3
p3 <- ggplot(expenditure, aes(x = X3, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "darkgreen") +
  labs(x = "Number of people per thousand residing in urban areas",
       y = "Per capita expenditure on shelters/housing assistance") +
  theme_minimal()
ggsave("p3.png", p3)

# Please plot the relationship between Y and Region? On average, which region has the
# highest per capita expenditure on housing assistance?

p4 <- ggplot(expenditure, aes(x = factor(Region), y = Y)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(x = "Region", y = "Per capita expenditure on shelters/housing assistance",
       title = "Expenditure by region") +
  scale_x_discrete(labels = c("1" = "Northeast", "2" = "North Central", "3" = "South", "4" = "West")) + 
  theme_minimal()
ggsave("p4.png", p4)

# Please plot the relationship between Y and X1? Describe this graph and the relationship.
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "blue") +
  labs(x = "Per capita personal income", y = "Per capita expenditure on shelters/housing assistance") +
  theme_minimal()

# Reproduce the above graph including one more variable Region and display
# different regions with different types of symbols and colors.

# Plot 
p5 <- ggplot(expenditure, 
             aes(x = X1, 
                 y = Y, 
                 color = factor(Region), 
                 shape = factor(Region))) +
  geom_point(size = 2.5, alpha = 0.75) +
  labs(title = "Relationship between Y and X1 by Region",
       x = "Per capita personal income", 
       y = "Per capita expenditure on shelters/housing assistance", 
       color = "Region", 
       shape = "Region") + 
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple"), 
                     labels = c("1" = "Northeast", "2" = "North Central", "3" = "South", "4" = "West")) +
  scale_shape_manual(values = c(16, 17, 18, 19), 
                     labels = c("1" = "Northeast", "2" = "North Central", "3" = "South", "4" = "West")) +
  theme_minimal()

ggsave("p5.png", p5)

