#####################
# load libraries
# set wd
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
lapply(c("stringr", "ggplot2", "viridis", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

# Question 1
# We are interested in knowing how the difference in campaign spending 
# between incumbent and challenger affects the incumbent's vote share.

# 1. Run a regression where the outcome variable is 'voteshare' and the explanatory 
# variable is 'difflog'.
reg1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(reg1)

# 2. Make a scatterplot of the two variables and add a regression line. 
ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point(alpha = 0.5, size = 0.8, color = viridis(1)) + 
  geom_smooth(method='lm', color = viridis(2)[2]) +
  labs(title = "Relationship between Vote Share and Difference in \n Campaign Spending (logged)",
       x = "Difference in Campaign Spending (logged)",
       y = "Vote Share of Incumbent") +
  theme_minimal()
ggsave("reg1_plot.png", plot = last_plot())

# 3. Save the residuals of the model in a separate object. 
reg1_resid <- reg1$residuals

# Question 2
# We are interested in knowing how the difference between incumbent's and challenger's
# spending and the vote share of the presidential candidate of the incumbent's party
# are related. 

# 1. Run a regression where the outcome variable is 'presvote' and the explanatory
# variable is 'difflog'. 
reg2 <- lm(presvote ~ difflog, inc.sub)
summary(reg2)

# 2. Make a scatterplot of the two variables and add a regression line. 
ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(alpha = 0.5, size = 0.8, color = viridis(1)) + 
  geom_smooth(method='lm', color = viridis(2)[2]) +
  labs(title = "Relationship between Vote Share and Difference in \n Campaign Spending (logged)",
       x = "Difference in Campaign Spending (logged)",
       y = "Vote Share of Presidential Candidate") +
  theme_minimal()
ggsave("reg2_plot.png", plot = last_plot())

# 3. Save the residuals of the model in a separate object.
reg2_resid <- reg2$residuals

# Question 3
# We are interested in knowing how the vote share of the presidential candidate
# of the incumbent's party is associated with the incumbent's electoral success.

# 1. Run a regression where the outcome variable is 'voteshare' and the
# explanatory variable is 'presvote'.
reg3 <- lm(voteshare ~ presvote, inc.sub)
summary(reg3)

# 2. Make a scatterplot of the two variables and add the regression line.
ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(alpha = 0.5, size = 0.8, color = viridis(1)) + 
  geom_smooth(method='lm', color = viridis(2)[2]) +
  labs(title = "Relationship between the Incumbent's and the Presidential \n Candidate's Vote Shares",
       x = "Vote Share of Presidential Candidate",
       y = "Vote Share of Incumbent") +
  theme_minimal()
ggsave("reg3_plot.png", plot = last_plot())

# Question 4
# The residuals from part (a) tell us how much of the variation in voteshare is 
# not explained by the difference in spending between incumbent and challenger. 
# The residuals in part (b) tell us how much of the variation in presvote is not 
# explained by the difference in spending between incumbent and challenger in 
# the district.

# 1. Run a regression where the outcome variable is the residuals from Question 1 
# and the explanatory variable is the residuals from Question 2.

# I put the numbers in a data frame to also use in the scatterplot later. 
resid_df <- data.frame(reg1_resid, reg2_resid)

# Run the regression
resid_reg <- lm(reg1_resid ~ reg2_resid, data = resid_df)
summary(resid_reg)


# 2. Make a scatterplot of the two residuals and add the regression line.
ggplot(resid_df, aes(x = reg2_resid, y = reg1_resid)) +
  geom_point(alpha = 0.5, size = 0.8, color = viridis(1)) + 
  geom_smooth(method='lm', color = viridis(2)[2]) +
  labs(title = "Residual Association between Incumbent's and Presidential Vote Shares",
       x = "Residuals of Presidential Vote Share (from Model 1)",
       y = "Residuals of Incumbent Vote Share (from Model 2)") + 
  theme_minimal()
ggsave("resid_reg_plot.png", plot = last_plot())

# "Unexplained Variation in Incumbent Vote Share and Presidential Vote Share",
# x = "Residuals of Presidential Vote Share (variation not explained by spending difference)",
# y = "Residuals of Incumbent Vote Share (variation not explained by spending difference)"

# Question 5
# What if the incumbent's vote share is affected by both the president's popularity 
# and the difference in spending between incumbent and challenger?

# 1. Run a regression where the outcome variable is the incumbent's voteshare and the
# explanatory variables are difflog and presvote.
reg4 <- lm(voteshare ~ difflog + presvote, inc.sub)
summary(reg4)

# LaTeX tables
stargazer(reg1, type = "latex", 
          title = "Coefficient Table of Model 1", 
          dep.var.labels = "Incumbent VS", 
          covariate.labels = "$\\Delta$ in Campaign Spending", 
          out = "reg1_output.tex")

stargazer(reg2, type = "latex", 
          title = "Coefficient Table of Model 2", 
          dep.var.labels = "Presidential Cand. VS", 
          covariate.labels = "$\\Delta$ in Campaign Spending", 
          out = "reg2_output.tex")

stargazer(reg3, type = "latex", 
          title = "Coefficient Table of Model 2", 
          dep.var.labels = "Incumbent VS", 
          covariate.labels = "Presidential Cand. VS", 
          out = "reg3_output.tex")

stargazer(reg4, type = "latex",
          title = "Coefficient Table of Model 3",
          dep.var.labels = "Incumbent VS",
          covariate.labels = c("$\\Delta$ in Campaign Spending", 
                               " Presidential Cand. VS"),
          out = "reg4_output.tex")

stargazer(resid_reg, type = "latex",
          title = "Coefficient Table of Residuals Regression",
          dep.var.labels = "Residuals from Reg1",
          covariate.labels = "Residuals from Reg2",
          out = "resid_reg_output.tex")
