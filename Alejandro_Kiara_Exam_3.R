################################################
# ECON 418-518 Exam 3
# Kiara Alejandro
# The University of Arizona
# kiaraalajendro@arizona.edu 
# 15 December 2024
###################################################

# Load necessary libraries
library(data.table)

# Set seed for reproducibility
set.seed(418518)

# Load the data
data <- fread("ECON_418-518_Exam_3_Data.csv")

# Inspect the structure of the dataset
str(data)

# Summary statistics for initial exploration
summary(data)

# View the first few rows of the data
head(data)

# Check for missing values
colSums(is.na(data))

# Add a time indicator for easier use later (e.g., pre- and post-treatment)
data[, time_period_binary := ifelse(time.period == "February", 0, 1)]

# Create a treatment indicator for New Jersey (1) vs Pennsylvania (0)
data[, treatment := ifelse(state == 1, 1, 0)]

# Quick summary: Total employment by state and time
summary_table <- data[, .(
  avg_emp = mean(total.emp, na.rm = TRUE)
), by = .(state, time_period_binary)]

print(summary_table)

#####################
# Question 2
#####################

##############
# Part (ii)
##############

# Create indicators for time period (Post) and state (New Jersey)
data[, post := ifelse(time.period == "November", 1, 0)]  # Indicator for Post-treatment
data[, nj := ifelse(state == 1, 1, 0)]                  # Indicator for New Jersey

# Calculate mean total employment by state and time period
mean_emp <- data[, .(mean_emp = mean(total.emp, na.rm = TRUE)), by = .(state, time.period)]

# Print results
print(mean_emp)

##############
# Part (iii)
##############

# Compute means
mean_employment <- data[, .(mean_emp = mean(total_emp)), by = .(state, time_period)]
nj_diff <- diff(mean_employment[state == 1]$mean_emp)
pa_diff <- diff(mean_employment[state == 0]$mean_emp)
DiD <- nj_diff - pa_diff
DiD

##############
# Part (iv)
##############

# lm() estimation
model <- lm(total_emp ~ state * time_period, data = data)
summary(model)

# Confidence interval calculation
coeff <- coef(summary(model))["state:time_period", ]
estimate <- coeff["Estimate"]
se <- coeff["Std. Error"]
ci_lower <- estimate - 1.96 * se
ci_upper <- estimate + 1.96 * se
ci <- c(ci_lower, ci_upper)
ci

##############
# Part (v)
##############

# Pre-policy interaction test
pre_policy_model <- lm(total_emp ~ state * time_period_before, data = pre_data)
summary(pre_policy_model)

##############
# Part (vii)
##############

did_fixed <- lm(total_emp ~ state * time_period + factor(restaurant_id), data = data)
summary(did_fixed)



