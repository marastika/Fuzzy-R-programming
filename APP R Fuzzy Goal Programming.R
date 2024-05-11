#####--------------SP HJ 05---------------------####
#Fuzzy Goal Programming Inlet Disolved Oksigen 
values.inlet.DO <- c(1.17, 0.49, 1.15, 0.37, 1.05, 0.43, 1.09, 1.17, 0.91, 1.39, 1.19, 1.35)

# Control limits
LCL.inlet.DO <- -0.09
UCL.inlet.DO <- 2.05
CL.inlet.DO <- 0.98

# Define fuzzy goal variables
fuzzy_goal_low. <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.inlet.DO <- pmax(0, (UCL.inlet.DO - values.inlet.DO) / (UCL.inlet.DO - LCL.inlet.DO))
membership_medium.inlet.DO <- pmax(0, 1 - abs((values.inlet.DO - CL.inlet.DO) / (UCL.inlet.DO - CL.inlet.DO)))
membership_high.inlet.DO <- pmax(0, (values.inlet.DO - LCL.inlet.DO) / (UCL.inlet.DO - LCL.inlet.DO))
membership.inlet.DO<-data.frame(membership_low.inlet.DO,membership_medium.inlet.DO,membership_high.inlet.DO)

# Set fuzzy goal constraints
fuzzy_goal_A.inlet.DO <- membership_low.inlet.DO  # (AX)_i ≤̃ b_i
fuzzy_goal_B.inlet.DO <- membership_high.inlet.DO  # (AX)_i ≥̃ b_i
fuzzy_goal_C.inlet.DO <- membership_medium.inlet.DO  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.inlet.DO <- function(x) {
  sum((fuzzy_goal_A.inlet.DO - x)^2 + (fuzzy_goal_B.inlet.DO - x)^2 + (fuzzy_goal_C.inlet.DO - x)^2)
}

# Initial guess for the optimizer
initial_guess.inlet.DO <- rep(0.5, length(values.inlet.DO))

# Optimize the objective function
result.inlet.DO <- optim(par = initial_guess.inlet.DO, fn = objective_function.inlet.DO, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.inlet.DO$par)

# Buat dataframe dari hasil optimal
optimal_result.inlet.DO <- data.frame(result.inlet.DO$par)

#Fuzzy Goal Programming inlet Biological Oxygen Demand (BOD)
values.inlet.BOD <- c(6.69,6.05,6.36,6.15,6.15,6.35,6.05,6.61,6.21,6.59,6.93,6.36)

# Control limits
LCL.inlet.BOD <- 5.54
UCL.inlet.BOD <- 7.21
CL.inlet.BOD <- 6.38

# Define fuzzy goal variables
fuzzy_goal_low. <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.inlet.BOD <- pmax(0, (UCL.inlet.BOD - values.inlet.BOD) / (UCL.inlet.BOD - LCL.inlet.BOD))
membership_medium.inlet.BOD <- pmax(0, 1 - abs((values.inlet.BOD - CL.inlet.BOD) / (UCL.inlet.BOD - CL.inlet.BOD)))
membership_high.inlet.BOD <- pmax(0, (values.inlet.BOD - LCL.inlet.BOD) / (UCL.inlet.BOD - LCL.inlet.BOD))
membership.inlet.BOD<-data.frame(membership_low.inlet.BOD,membership_medium.inlet.BOD,membership_high.inlet.BOD)

# Set fuzzy goal constraints
fuzzy_goal_A.inlet.BOD <- membership_low.inlet.BOD  # (AX)_i ≤̃ b_i
fuzzy_goal_B.inlet.BOD <- membership_high.inlet.BOD  # (AX)_i ≥̃ b_i
fuzzy_goal_C.inlet.BOD <- membership_medium.inlet.BOD  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.inlet.BOD <- function(x) {
  sum((fuzzy_goal_A.inlet.BOD - x)^2 + (fuzzy_goal_B.inlet.BOD - x)^2 + (fuzzy_goal_C.inlet.BOD - x)^2)
}

# Initial guess for the optimizer
initial_guess.inlet.BOD <- rep(0.5, length(values.inlet.BOD))

# Optimize the objective function
result.inlet.BOD <- optim(par = initial_guess.inlet.BOD, fn = objective_function.inlet.BOD, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.inlet.BOD$par)

# Buat dataframe dari hasil optimal
optimal_result.inlet.BOD <- data.frame(result.inlet.BOD$par)

#Fuzzy Goal Programming outlet Dissolved Oxygen (DO)
values.outlet.DO <- c(4.33,4.21,4.69,4.93,5.36,4.86,4.55,4.67,5.07,5.15,4.75,4.85)

# Control limits
LCL.outlet.DO <- 3.80
UCL.outlet.DO <- 5.77
CL.outlet.DO <- 4.79

# Define fuzzy goal variables
fuzzy_goal_low <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.outlet.DO <- pmax(0, (UCL.outlet.DO - values.outlet.DO) / (UCL.outlet.DO - LCL.outlet.DO))
membership_medium.outlet.DO <- pmax(0, 1 - abs((values.outlet.DO - CL.outlet.DO) / (UCL.outlet.DO - CL.outlet.DO)))
membership_high.outlet.DO <- pmax(0, (values.outlet.DO - LCL.outlet.DO) / (UCL.outlet.DO - LCL.outlet.DO))
membership.outlet.DO<-data.frame(membership_low.outlet.DO,membership_medium.outlet.DO,membership_high.outlet.DO)

# Set fuzzy goal constraints
fuzzy_goal_A.outlet.DO <- membership_low.outlet.DO  # (AX)_i ≤̃ b_i
fuzzy_goal_B.outlet.DO <- membership_high.outlet.DO  # (AX)_i ≥̃ b_i
fuzzy_goal_C.outlet.DO <- membership_medium.outlet.DO  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.outlet.DO <- function(x) {
  sum((fuzzy_goal_A.outlet.DO - x)^2 + (fuzzy_goal_B.outlet.DO - x)^2 + (fuzzy_goal_C.outlet.DO - x)^2)
}

# Initial guess for the optimizer
initial_guess.outlet.DO <- rep(0.5, length(values.outlet.DO))

# Optimize the objective function
result.outlet.DO <- optim(par = initial_guess.outlet.DO, fn = objective_function.outlet.DO, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.outlet.DO$par)

# Buat dataframe dari hasil optimal
optimal_result.outlet.DO <- data.frame(result.outlet.DO$par)

#Fuzzy Goal Programming outlet Biological Oxygen Demand (BOD)
values.outlet.BOD <- c(2.03,2.19,1.09,2.33,2.17,2.49,2.15,2.35,2.05,2.41,2.13,2.49)

# Control limits
LCL.outlet.BOD <- 1.04
UCL.outlet.BOD <- 3.27
CL.outlet.BOD <- 2.16

# Define fuzzy goal variables
fuzzy_goal_low <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.outlet.BOD <- pmax(0, (UCL.outlet.BOD - values.outlet.BOD) / (UCL.outlet.BOD - LCL.outlet.BOD))
membership_medium.outlet.BOD <- pmax(0, 1 - abs((values.outlet.BOD - CL.outlet.BOD) / (UCL.outlet.BOD - CL.outlet.BOD)))
membership_high.outlet.BOD <- pmax(0, (values.outlet.BOD - LCL.outlet.BOD) / (UCL.outlet.BOD - LCL.outlet.BOD))
membership.outlet.BOD<-data.frame(membership_low.outlet.BOD,membership_medium.outlet.BOD,membership_high.outlet.BOD)

# Set fuzzy goal constraints
fuzzy_goal_A.outlet.BOD <- membership_low.outlet.BOD  # (AX)_i ≤̃ b_i
fuzzy_goal_B.outlet.BOD <- membership_high.outlet.BOD  # (AX)_i ≥̃ b_i
fuzzy_goal_C.outlet.BOD <- membership_medium.outlet.BOD  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.outlet.BOD <- function(x) {
  sum((fuzzy_goal_A.outlet.BOD - x)^2 + (fuzzy_goal_B.outlet.BOD - x)^2 + (fuzzy_goal_C.outlet.BOD - x)^2)
}

# Initial guess for the optimizer
initial_guess.outlet.BOD <- rep(0.5, length(values.outlet.BOD))

# Optimize the objective function
result.outlet.BOD <- optim(par = initial_guess.outlet.BOD, fn = objective_function.outlet.BOD, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.outlet.BOD$par)

# Buat dataframe dari hasil optimal
optimal_result.outlet.BOD <- data.frame(result.outlet.BOD$par)

#####--------------SP HJ 05---------------------####
#Fuzzy Goal Programming Inlet Disolved Oksigen 
values.inlet.DO <- c(1.17, 0.49, 1.15, 0.37, 1.05, 0.43, 1.09, 1.17, 0.91, 1.39, 1.19, 1.35)

# Control limits
LCL.inlet.DO <- -0.09
UCL.inlet.DO <- 2.05
CL.inlet.DO <- 0.98

# Define fuzzy goal variables
fuzzy_goal_low. <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.inlet.DO <- pmax(0, (UCL.inlet.DO - values.inlet.DO) / (UCL.inlet.DO - LCL.inlet.DO))
membership_medium.inlet.DO <- pmax(0, 1 - abs((values.inlet.DO - CL.inlet.DO) / (UCL.inlet.DO - CL.inlet.DO)))
membership_high.inlet.DO <- pmax(0, (values.inlet.DO - LCL.inlet.DO) / (UCL.inlet.DO - LCL.inlet.DO))
membership.inlet.DO<-data.frame(membership_low.inlet.DO,membership_medium.inlet.DO,membership_high.inlet.DO)

# Set fuzzy goal constraints
fuzzy_goal_A.inlet.DO <- membership_low.inlet.DO  # (AX)_i ≤̃ b_i
fuzzy_goal_B.inlet.DO <- membership_high.inlet.DO  # (AX)_i ≥̃ b_i
fuzzy_goal_C.inlet.DO <- membership_medium.inlet.DO  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.inlet.DO <- function(x) {
  sum((fuzzy_goal_A.inlet.DO - x)^2 + (fuzzy_goal_B.inlet.DO - x)^2 + (fuzzy_goal_C.inlet.DO - x)^2)
}

# Initial guess for the optimizer
initial_guess.inlet.DO <- rep(0.5, length(values.inlet.DO))

# Optimize the objective function
result.inlet.DO <- optim(par = initial_guess.inlet.DO, fn = objective_function.inlet.DO, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.inlet.DO$par)

# Buat dataframe dari hasil optimal
optimal_result.inlet.DO <- data.frame(result.inlet.DO$par)

#Fuzzy Goal Programming inlet Biological Oxygen Demand (BOD)
values.inlet.BOD <- c(6.69,6.05,6.36,6.15,6.15,6.35,6.05,6.61,6.21,6.59,6.93,6.36)

# Control limits
LCL.inlet.BOD <- 5.54
UCL.inlet.BOD <- 7.21
CL.inlet.BOD <- 6.38

# Define fuzzy goal variables
fuzzy_goal_low. <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.inlet.BOD <- pmax(0, (UCL.inlet.BOD - values.inlet.BOD) / (UCL.inlet.BOD - LCL.inlet.BOD))
membership_medium.inlet.BOD <- pmax(0, 1 - abs((values.inlet.BOD - CL.inlet.BOD) / (UCL.inlet.BOD - CL.inlet.BOD)))
membership_high.inlet.BOD <- pmax(0, (values.inlet.BOD - LCL.inlet.BOD) / (UCL.inlet.BOD - LCL.inlet.BOD))
membership.inlet.BOD<-data.frame(membership_low.inlet.BOD,membership_medium.inlet.BOD,membership_high.inlet.BOD)

# Set fuzzy goal constraints
fuzzy_goal_A.inlet.BOD <- membership_low.inlet.BOD  # (AX)_i ≤̃ b_i
fuzzy_goal_B.inlet.BOD <- membership_high.inlet.BOD  # (AX)_i ≥̃ b_i
fuzzy_goal_C.inlet.BOD <- membership_medium.inlet.BOD  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.inlet.BOD <- function(x) {
  sum((fuzzy_goal_A.inlet.BOD - x)^2 + (fuzzy_goal_B.inlet.BOD - x)^2 + (fuzzy_goal_C.inlet.BOD - x)^2)
}

# Initial guess for the optimizer
initial_guess.inlet.BOD <- rep(0.5, length(values.inlet.BOD))

# Optimize the objective function
result.inlet.BOD <- optim(par = initial_guess.inlet.BOD, fn = objective_function.inlet.BOD, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.inlet.BOD$par)

# Buat dataframe dari hasil optimal
optimal_result.inlet.BOD <- data.frame(result.inlet.BOD$par)

#Fuzzy Goal Programming outlet Dissolved Oxygen (DO)
values.outlet.DO <- c(4.33,4.21,4.69,4.93,5.36,4.86,4.55,4.67,5.07,5.15,4.75,4.85)

# Control limits
LCL.outlet.DO <- 3.80
UCL.outlet.DO <- 5.77
CL.outlet.DO <- 4.79

# Define fuzzy goal variables
fuzzy_goal_low <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.outlet.DO <- pmax(0, (UCL.outlet.DO - values.outlet.DO) / (UCL.outlet.DO - LCL.outlet.DO))
membership_medium.outlet.DO <- pmax(0, 1 - abs((values.outlet.DO - CL.outlet.DO) / (UCL.outlet.DO - CL.outlet.DO)))
membership_high.outlet.DO <- pmax(0, (values.outlet.DO - LCL.outlet.DO) / (UCL.outlet.DO - LCL.outlet.DO))
membership.outlet.DO<-data.frame(membership_low.outlet.DO,membership_medium.outlet.DO,membership_high.outlet.DO)

# Set fuzzy goal constraints
fuzzy_goal_A.outlet.DO <- membership_low.outlet.DO  # (AX)_i ≤̃ b_i
fuzzy_goal_B.outlet.DO <- membership_high.outlet.DO  # (AX)_i ≥̃ b_i
fuzzy_goal_C.outlet.DO <- membership_medium.outlet.DO  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.outlet.DO <- function(x) {
  sum((fuzzy_goal_A.outlet.DO - x)^2 + (fuzzy_goal_B.outlet.DO - x)^2 + (fuzzy_goal_C.outlet.DO - x)^2)
}

# Initial guess for the optimizer
initial_guess.outlet.DO <- rep(0.5, length(values.outlet.DO))

# Optimize the objective function
result.outlet.DO <- optim(par = initial_guess.outlet.DO, fn = objective_function.outlet.DO, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.outlet.DO$par)

# Buat dataframe dari hasil optimal
optimal_result.outlet.DO <- data.frame(result.outlet.DO$par)

#Fuzzy Goal Programming outlet Biological Oxygen Demand (BOD)
values.outlet.BOD <- c(2.03,2.19,1.09,2.33,2.17,2.49,2.15,2.35,2.05,2.41,2.13,2.49)

# Control limits
LCL.outlet.BOD <- 1.04
UCL.outlet.BOD <- 3.27
CL.outlet.BOD <- 2.16

# Define fuzzy goal variables
fuzzy_goal_low <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.outlet.BOD <- pmax(0, (UCL.outlet.BOD - values.outlet.BOD) / (UCL.outlet.BOD - LCL.outlet.BOD))
membership_medium.outlet.BOD <- pmax(0, 1 - abs((values.outlet.BOD - CL.outlet.BOD) / (UCL.outlet.BOD - CL.outlet.BOD)))
membership_high.outlet.BOD <- pmax(0, (values.outlet.BOD - LCL.outlet.BOD) / (UCL.outlet.BOD - LCL.outlet.BOD))
membership.outlet.BOD<-data.frame(membership_low.outlet.BOD,membership_medium.outlet.BOD,membership_high.outlet.BOD)

# Set fuzzy goal constraints
fuzzy_goal_A.outlet.BOD <- membership_low.outlet.BOD  # (AX)_i ≤̃ b_i
fuzzy_goal_B.outlet.BOD <- membership_high.outlet.BOD  # (AX)_i ≥̃ b_i
fuzzy_goal_C.outlet.BOD <- membership_medium.outlet.BOD  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.outlet.BOD <- function(x) {
  sum((fuzzy_goal_A.outlet.BOD - x)^2 + (fuzzy_goal_B.outlet.BOD - x)^2 + (fuzzy_goal_C.outlet.BOD - x)^2)
}

# Initial guess for the optimizer
initial_guess.outlet.BOD <- rep(0.5, length(values.outlet.BOD))

# Optimize the objective function
result.outlet.BOD <- optim(par = initial_guess.outlet.BOD, fn = objective_function.outlet.BOD, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.outlet.BOD$par)

# Buat dataframe dari hasil optimal
optimal_result.outlet.BOD <- data.frame(result.outlet.BOD$par)

#####--------------SP HJ 05---------------------####
#Fuzzy Goal Programming Inlet Disolved Oksigen 
values.inlet.DO <- c(1.17, 0.49, 1.15, 0.37, 1.05, 0.43, 1.09, 1.17, 0.91, 1.39, 1.19, 1.35)

# Control limits
LCL.inlet.DO <- -0.09
UCL.inlet.DO <- 2.05
CL.inlet.DO <- 0.98

# Define fuzzy goal variables
fuzzy_goal_low. <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.inlet.DO <- pmax(0, (UCL.inlet.DO - values.inlet.DO) / (UCL.inlet.DO - LCL.inlet.DO))
membership_medium.inlet.DO <- pmax(0, 1 - abs((values.inlet.DO - CL.inlet.DO) / (UCL.inlet.DO - CL.inlet.DO)))
membership_high.inlet.DO <- pmax(0, (values.inlet.DO - LCL.inlet.DO) / (UCL.inlet.DO - LCL.inlet.DO))
membership.inlet.DO<-data.frame(membership_low.inlet.DO,membership_medium.inlet.DO,membership_high.inlet.DO)

# Set fuzzy goal constraints
fuzzy_goal_A.inlet.DO <- membership_low.inlet.DO  # (AX)_i ≤̃ b_i
fuzzy_goal_B.inlet.DO <- membership_high.inlet.DO  # (AX)_i ≥̃ b_i
fuzzy_goal_C.inlet.DO <- membership_medium.inlet.DO  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.inlet.DO <- function(x) {
  sum((fuzzy_goal_A.inlet.DO - x)^2 + (fuzzy_goal_B.inlet.DO - x)^2 + (fuzzy_goal_C.inlet.DO - x)^2)
}

# Initial guess for the optimizer
initial_guess.inlet.DO <- rep(0.5, length(values.inlet.DO))

# Optimize the objective function
result.inlet.DO <- optim(par = initial_guess.inlet.DO, fn = objective_function.inlet.DO, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.inlet.DO$par)

# Buat dataframe dari hasil optimal
optimal_result.inlet.DO <- data.frame(result.inlet.DO$par)

#Fuzzy Goal Programming inlet Biological Oxygen Demand (BOD)
values.inlet.BOD <- c(6.69,6.05,6.36,6.15,6.15,6.35,6.05,6.61,6.21,6.59,6.93,6.36)

# Control limits
LCL.inlet.BOD <- 5.54
UCL.inlet.BOD <- 7.21
CL.inlet.BOD <- 6.38

# Define fuzzy goal variables
fuzzy_goal_low. <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.inlet.BOD <- pmax(0, (UCL.inlet.BOD - values.inlet.BOD) / (UCL.inlet.BOD - LCL.inlet.BOD))
membership_medium.inlet.BOD <- pmax(0, 1 - abs((values.inlet.BOD - CL.inlet.BOD) / (UCL.inlet.BOD - CL.inlet.BOD)))
membership_high.inlet.BOD <- pmax(0, (values.inlet.BOD - LCL.inlet.BOD) / (UCL.inlet.BOD - LCL.inlet.BOD))
membership.inlet.BOD<-data.frame(membership_low.inlet.BOD,membership_medium.inlet.BOD,membership_high.inlet.BOD)

# Set fuzzy goal constraints
fuzzy_goal_A.inlet.BOD <- membership_low.inlet.BOD  # (AX)_i ≤̃ b_i
fuzzy_goal_B.inlet.BOD <- membership_high.inlet.BOD  # (AX)_i ≥̃ b_i
fuzzy_goal_C.inlet.BOD <- membership_medium.inlet.BOD  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.inlet.BOD <- function(x) {
  sum((fuzzy_goal_A.inlet.BOD - x)^2 + (fuzzy_goal_B.inlet.BOD - x)^2 + (fuzzy_goal_C.inlet.BOD - x)^2)
}

# Initial guess for the optimizer
initial_guess.inlet.BOD <- rep(0.5, length(values.inlet.BOD))

# Optimize the objective function
result.inlet.BOD <- optim(par = initial_guess.inlet.BOD, fn = objective_function.inlet.BOD, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.inlet.BOD$par)

# Buat dataframe dari hasil optimal
optimal_result.inlet.BOD <- data.frame(result.inlet.BOD$par)

#Fuzzy Goal Programming outlet Dissolved Oxygen (DO)
values.outlet.DO <- c(4.33,4.21,4.69,4.93,5.36,4.86,4.55,4.67,5.07,5.15,4.75,4.85)

# Control limits
LCL.outlet.DO <- 3.80
UCL.outlet.DO <- 5.77
CL.outlet.DO <- 4.79

# Define fuzzy goal variables
fuzzy_goal_low <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.outlet.DO <- pmax(0, (UCL.outlet.DO - values.outlet.DO) / (UCL.outlet.DO - LCL.outlet.DO))
membership_medium.outlet.DO <- pmax(0, 1 - abs((values.outlet.DO - CL.outlet.DO) / (UCL.outlet.DO - CL.outlet.DO)))
membership_high.outlet.DO <- pmax(0, (values.outlet.DO - LCL.outlet.DO) / (UCL.outlet.DO - LCL.outlet.DO))
membership.outlet.DO<-data.frame(membership_low.outlet.DO,membership_medium.outlet.DO,membership_high.outlet.DO)

# Set fuzzy goal constraints
fuzzy_goal_A.outlet.DO <- membership_low.outlet.DO  # (AX)_i ≤̃ b_i
fuzzy_goal_B.outlet.DO <- membership_high.outlet.DO  # (AX)_i ≥̃ b_i
fuzzy_goal_C.outlet.DO <- membership_medium.outlet.DO  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.outlet.DO <- function(x) {
  sum((fuzzy_goal_A.outlet.DO - x)^2 + (fuzzy_goal_B.outlet.DO - x)^2 + (fuzzy_goal_C.outlet.DO - x)^2)
}

# Initial guess for the optimizer
initial_guess.outlet.DO <- rep(0.5, length(values.outlet.DO))

# Optimize the objective function
result.outlet.DO <- optim(par = initial_guess.outlet.DO, fn = objective_function.outlet.DO, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.outlet.DO$par)

# Buat dataframe dari hasil optimal
optimal_result.outlet.DO <- data.frame(result.outlet.DO$par)

#Fuzzy Goal Programming outlet Biological Oxygen Demand (BOD)
values.outlet.BOD <- c(2.03,2.19,1.09,2.33,2.17,2.49,2.15,2.35,2.05,2.41,2.13,2.49)

# Control limits
LCL.outlet.BOD <- 1.04
UCL.outlet.BOD <- 3.27
CL.outlet.BOD <- 2.16

# Define fuzzy goal variables
fuzzy_goal_low <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.outlet.BOD <- pmax(0, (UCL.outlet.BOD - values.outlet.BOD) / (UCL.outlet.BOD - LCL.outlet.BOD))
membership_medium.outlet.BOD <- pmax(0, 1 - abs((values.outlet.BOD - CL.outlet.BOD) / (UCL.outlet.BOD - CL.outlet.BOD)))
membership_high.outlet.BOD <- pmax(0, (values.outlet.BOD - LCL.outlet.BOD) / (UCL.outlet.BOD - LCL.outlet.BOD))
membership.outlet.BOD<-data.frame(membership_low.outlet.BOD,membership_medium.outlet.BOD,membership_high.outlet.BOD)

# Set fuzzy goal constraints
fuzzy_goal_A.outlet.BOD <- membership_low.outlet.BOD  # (AX)_i ≤̃ b_i
fuzzy_goal_B.outlet.BOD <- membership_high.outlet.BOD  # (AX)_i ≥̃ b_i
fuzzy_goal_C.outlet.BOD <- membership_medium.outlet.BOD  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.outlet.BOD <- function(x) {
  sum((fuzzy_goal_A.outlet.BOD - x)^2 + (fuzzy_goal_B.outlet.BOD - x)^2 + (fuzzy_goal_C.outlet.BOD - x)^2)
}

# Initial guess for the optimizer
initial_guess.outlet.BOD <- rep(0.5, length(values.outlet.BOD))

# Optimize the objective function
result.outlet.BOD <- optim(par = initial_guess.outlet.BOD, fn = objective_function.outlet.BOD, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.outlet.BOD$par)

# Buat dataframe dari hasil optimal
optimal_result.outlet.BOD <- data.frame(result.outlet.BOD$par)

#####--------------SP HJ 09---------------------####
#Fuzzy Goal Programming Inlet Disolved Oksigen 
values.inlet.DO <- c(0.9,0.09,0.35,0.3,0.15,0.21,0.69,0.33,0.21,0.69,0.93,0.36)

# Control limits
LCL.inlet.DO <- -0.44
UCL.inlet.DO <- 1.31
CL.inlet.DO <- 0.43

# Define fuzzy goal variables
fuzzy_goal_low. <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.inlet.DO <- pmax(0, (UCL.inlet.DO - values.inlet.DO) / (UCL.inlet.DO - LCL.inlet.DO))
membership_medium.inlet.DO <- pmax(0, 1 - abs((values.inlet.DO - CL.inlet.DO) / (UCL.inlet.DO - CL.inlet.DO)))
membership_high.inlet.DO <- pmax(0, (values.inlet.DO - LCL.inlet.DO) / (UCL.inlet.DO - LCL.inlet.DO))
membership.inlet.DO<-data.frame(membership_low.inlet.DO,membership_medium.inlet.DO,membership_high.inlet.DO)

# Set fuzzy goal constraints
fuzzy_goal_A.inlet.DO <- membership_low.inlet.DO  # (AX)_i ≤̃ b_i
fuzzy_goal_B.inlet.DO <- membership_high.inlet.DO  # (AX)_i ≥̃ b_i
fuzzy_goal_C.inlet.DO <- membership_medium.inlet.DO  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.inlet.DO <- function(x) {
  sum((fuzzy_goal_A.inlet.DO - x)^2 + (fuzzy_goal_B.inlet.DO - x)^2 + (fuzzy_goal_C.inlet.DO - x)^2)
}

# Initial guess for the optimizer
initial_guess.inlet.DO <- rep(0.5, length(values.inlet.DO))

# Optimize the objective function
result.inlet.DO <- optim(par = initial_guess.inlet.DO, fn = objective_function.inlet.DO, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.inlet.DO$par)

# Buat dataframe dari hasil optimal
optimal_result.inlet.DO <- data.frame(result.inlet.DO$par)

#Fuzzy Goal Programming inlet Biological Oxygen Demand (BOD)
values.inlet.BOD <- c(6.65,6.66,6.63,4.63,6.69,6.93,5.36,4.86,6.67,6.69,6.65,4.37)

# Control limits
LCL.inlet.BOD <-3.19 
UCL.inlet.BOD <- 8.95
CL.inlet.BOD <- 6.07

# Define fuzzy goal variables
fuzzy_goal_low. <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.inlet.BOD <- pmax(0, (UCL.inlet.BOD - values.inlet.BOD) / (UCL.inlet.BOD - LCL.inlet.BOD))
membership_medium.inlet.BOD <- pmax(0, 1 - abs((values.inlet.BOD - CL.inlet.BOD) / (UCL.inlet.BOD - CL.inlet.BOD)))
membership_high.inlet.BOD <- pmax(0, (values.inlet.BOD - LCL.inlet.BOD) / (UCL.inlet.BOD - LCL.inlet.BOD))
membership.inlet.BOD<-data.frame(membership_low.inlet.BOD,membership_medium.inlet.BOD,membership_high.inlet.BOD)

# Set fuzzy goal constraints
fuzzy_goal_A.inlet.BOD <- membership_low.inlet.BOD  # (AX)_i ≤̃ b_i
fuzzy_goal_B.inlet.BOD <- membership_high.inlet.BOD  # (AX)_i ≥̃ b_i
fuzzy_goal_C.inlet.BOD <- membership_medium.inlet.BOD  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.inlet.BOD <- function(x) {
  sum((fuzzy_goal_A.inlet.BOD - x)^2 + (fuzzy_goal_B.inlet.BOD - x)^2 + (fuzzy_goal_C.inlet.BOD - x)^2)
}

# Initial guess for the optimizer
initial_guess.inlet.BOD <- rep(0.5, length(values.inlet.BOD))

# Optimize the objective function
result.inlet.BOD <- optim(par = initial_guess.inlet.BOD, fn = objective_function.inlet.BOD, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.inlet.BOD$par)

# Buat dataframe dari hasil optimal
optimal_result.inlet.BOD <- data.frame(result.inlet.BOD$par)

#Fuzzy Goal Programming outlet Dissolved Oxygen (DO)
values.outlet.DO <- c(4.15,4.35,4.05,4.41,4.13,4.49,4.17,4.94,4.49,4.35,4.34,4.15)

# Control limits
LCL.outlet.DO <- 3.61
UCL.outlet.DO <- 5.06
CL.outlet.DO <- 4.33

# Define fuzzy goal variables
fuzzy_goal_low <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.outlet.DO <- pmax(0, (UCL.outlet.DO - values.outlet.DO) / (UCL.outlet.DO - LCL.outlet.DO))
membership_medium.outlet.DO <- pmax(0, 1 - abs((values.outlet.DO - CL.outlet.DO) / (UCL.outlet.DO - CL.outlet.DO)))
membership_high.outlet.DO <- pmax(0, (values.outlet.DO - LCL.outlet.DO) / (UCL.outlet.DO - LCL.outlet.DO))
membership.outlet.DO<-data.frame(membership_low.outlet.DO,membership_medium.outlet.DO,membership_high.outlet.DO)

# Set fuzzy goal constraints
fuzzy_goal_A.outlet.DO <- membership_low.outlet.DO  # (AX)_i ≤̃ b_i
fuzzy_goal_B.outlet.DO <- membership_high.outlet.DO  # (AX)_i ≥̃ b_i
fuzzy_goal_C.outlet.DO <- membership_medium.outlet.DO  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.outlet.DO <- function(x) {
  sum((fuzzy_goal_A.outlet.DO - x)^2 + (fuzzy_goal_B.outlet.DO - x)^2 + (fuzzy_goal_C.outlet.DO - x)^2)
}

# Initial guess for the optimizer
initial_guess.outlet.DO <- rep(0.5, length(values.outlet.DO))

# Optimize the objective function
result.outlet.DO <- optim(par = initial_guess.outlet.DO, fn = objective_function.outlet.DO, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.outlet.DO$par)

# Buat dataframe dari hasil optimal
optimal_result.outlet.DO <- data.frame(result.outlet.DO$par)

#Fuzzy Goal Programming outlet Biological Oxygen Demand (BOD)
values.outlet.BOD <- c(2.17,1.92,2.29,1.35,2.34,1.55,2.33,1.21,2.69,1.93,2.36,1.06)

# Control limits
LCL.outlet.BOD <- 0.36
UCL.outlet.BOD <- 3.51
CL.outlet.BOD <- 1.93

# Define fuzzy goal variables
fuzzy_goal_low <- "Low"
fuzzy_goal_medium <- "Medium"
fuzzy_goal_high <- "High"

# Calculate membership values for each linguistic term
membership_low.outlet.BOD <- pmax(0, (UCL.outlet.BOD - values.outlet.BOD) / (UCL.outlet.BOD - LCL.outlet.BOD))
membership_medium.outlet.BOD <- pmax(0, 1 - abs((values.outlet.BOD - CL.outlet.BOD) / (UCL.outlet.BOD - CL.outlet.BOD)))
membership_high.outlet.BOD <- pmax(0, (values.outlet.BOD - LCL.outlet.BOD) / (UCL.outlet.BOD - LCL.outlet.BOD))
membership.outlet.BOD<-data.frame(membership_low.outlet.BOD,membership_medium.outlet.BOD,membership_high.outlet.BOD)

# Set fuzzy goal constraints
fuzzy_goal_A.outlet.BOD <- membership_low.outlet.BOD  # (AX)_i ≤̃ b_i
fuzzy_goal_B.outlet.BOD <- membership_high.outlet.BOD  # (AX)_i ≥̃ b_i
fuzzy_goal_C.outlet.BOD <- membership_medium.outlet.BOD  # (AX)_i =̃ b_i

# Objective function (minimize the sum of squared deviations from the fuzzy goals)
objective_function.outlet.BOD <- function(x) {
  sum((fuzzy_goal_A.outlet.BOD - x)^2 + (fuzzy_goal_B.outlet.BOD - x)^2 + (fuzzy_goal_C.outlet.BOD - x)^2)
}

# Initial guess for the optimizer
initial_guess.outlet.BOD <- rep(0.5, length(values.outlet.BOD))

# Optimize the objective function
result.outlet.BOD <- optim(par = initial_guess.outlet.BOD, fn = objective_function.outlet.BOD, method = "L-BFGS-B")

# Display the result
cat("Optimal Adjustments:\n")
print(result.outlet.BOD$par)

# Buat dataframe dari hasil optimal
optimal_result.outlet.BOD <- data.frame(result.outlet.BOD$par)