####################
# Problem Set 02
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

lapply(c("stringr", "stargazer"),  pkgTest)

#####################
# Question 1
#####################

# (a) Calculate the x^2 test statistic by hand/manually

# First, I create the table in R.
table <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)

# Formula: (Row total/Grand total) X Column total
# I calculate the different components of this formula as follows"
row_total_upper <- sum(table[1, ])  # 14 + 6 + 7 = 27
row_total_lower <- sum(table[2, ])  # 7 + 7 + 1 = 15

column_total_notstopped <- sum(table[, 1])  # 14 + 7 = 21
column_total_bribe <- sum(table[, 2])  # 6 + 7 = 13
column_total_warning <- sum(table[, 3])  # 7 + 1 = 8

# Grand total (sum of all frequencies)
grand_total <- sum(table)  # 14 + 6 + 7 + 7 + 7 + 1 = 42

# And now I calculate the expected frequencies ($f_e$)
fe_notstopped_upper <- (row_total_upper / grand_total) * column_total_notstopped
fe_notstopped_lower <- (row_total_lower / grand_total) * column_total_notstopped

fe_bribe_upper <- (row_total_upper / grand_total) * column_total_bribe
fe_bribe_lower <- (row_total_lower / grand_total) * column_total_bribe

fe_warning_upper <- (row_total_upper / grand_total) * column_total_warning
fe_warning_lower <- (row_total_lower / grand_total) * column_total_warning

# And the chi-square 'manually'
chi2 <- ((((14 - fe_notstopped_upper) ^ 2) / fe_notstopped_upper) + (((7 - fe_notstopped_lower) ^ 2) / fe_notstopped_lower) 
+ (((6 - fe_bribe_upper) ^ 2) / fe_bribe_upper) + (((7 - fe_bribe_lower) ^ 2) / fe_bribe_lower) + 
  (((7 - fe_warning_upper) ^ 2) / fe_warning_upper) + (((1 - fe_warning_lower) ^ 2) / fe_warning_lower))

# ...and alternatively:
chi2_alt <- sum(((table - matrix(c(fe_notstopped_upper, fe_notstopped_lower, 
                                  fe_bribe_upper, fe_bribe_lower, 
                                  fe_warning_upper, fe_warning_lower), 
                                 nrow = 2, byrow = TRUE))^2) / 
              matrix(c(fe_notstopped_upper, fe_notstopped_lower, 
                       fe_bribe_upper, fe_bribe_lower, 
                       fe_warning_upper, fe_warning_lower), nrow = 2, byrow = TRUE))


chi2 # 3.791168
chi2_alt # same

# And the p-value
# All frequencies should be >5 but we still calculate it now
# Degrees of freedom = (number of rows - 1) * (number of columns - 1)
df <- (2 - 1) * (3 - 1)
p_value <- pchisq(chi2, df = df, lower.tail = F)
p_value

# Check with chisq.test
chi2_test <- chisq.test(table)

chi2_test # both x^2 and p-value correspond to the 'manual' calculation 

## 0.15 > 0.10 so we cannot reject the null hypothesis

# (c) Calculate the standardized residuals for each cell and put them in the table below
chi2_test <- chisq.test(table)
chi2_test$residuals

# How might the standardized residuals help you interpret the results? 

##### Question 2: Economics #####
# Getting the data
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
head(data)

# Authors hypothesize that 'female politicians are more likely to support policies female voters want'
# They found that more women complain about the quality of drinking water than men. 
# I need to estimate the effect of the reservation policy on the number of new or repaired drinking water facilities in the villages

# (a) State a null and alternative (two-tailed) hypothesis. 
# H0 = The reservation policy had not effect on the number of new or repaired drinking water facilities
# Ha = The researvation policy had an effect on the number of new or repaired drinking water facilities

# (b) Run a bivariate regression to test this hypothesis in R
biv_reg <- lm(water ~ reserved, data = data)
summary(biv_reg)

# (c) Interpret the coefficient estimate for reservation policy
stargazer(biv_reg, type = "latex", 
          title = "Coefficient Table", 
          dep.var.labels = "New/Repaired Working facilities", 
          covariate.labels = "Reserved Status", 
          out = "regression_output.tex")
# The reservation policy lead to approximately 9 more new or repaired drinking water facilities in the two villages,
# on average. This result is significant on the 0.01 alpha level of confidence. 