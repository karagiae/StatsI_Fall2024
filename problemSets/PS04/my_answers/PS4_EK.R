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
lapply(c("car", "dplyr", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the data
data("Prestige")
help("Prestige")

# The Prestige df has 102 rows and 6 columns.
# Observations = Occupations
# Variables: education, income, women, prestige, census, type.

head(Prestige)

# We would like to study whether individuals with higher levels of income have
# more prestigious jobs. Moreover, we would like to study whether professionals
# have more prestigious jobs than blue and white collar workers. 

# (a) Create a new variable 'professional' by recoding the variable 'type' so
# that professionals are coded as 1, and blue and white collar workers as 0 
# Hint: ifelse()

# Creating 'df' for simplicity
df <- Prestige

df <- df %>%
  mutate(professional = ifelse(type == "prof", 1, 0))

# (b) Run a linear model with 'prestige' as an outcome and 'income', 'professional',
# and the interaction of the two as predictors.
# Note: continuous X dummy interaction

model1 <- lm(prestige ~ income + professional + income:professional, data = df)
summary(model1)

# Stargazer table for LaTeX
stargazer(model1, type = "latex",
          title = "Coefficient Table of Model 1",
          dep.var.labels = "Prestige",
          covariate.labels = c("Income", "Professional", "Income X Professional"),
          out = "model1_output.tex")
# (c) Write the prediction equation based on the result

# (f) 1000 income increase
marginal_effect_income <- (coef["income"] + coef["income:professional"]) * 1000
print(marginal_effect_income)

# (g) 6000 income - change from professional to non professional
# Set the income value to 6000
income_value <- 6000

# Predicted values for professional = 1 and 0
y_prof <- coef["(Intercept)"] + coef["income"] * income_value + coef["professional"] + coef["income:professional"] * income_value
y_non_prof <- coef["(Intercept)"] + coef["income"] * income_value

# Effect of changing to professional 
effect_g <- y_prof - y_non_prof
effect_g
