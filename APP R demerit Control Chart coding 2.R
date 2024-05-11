# Install and load required packages
install.packages(c("igraph", "qcc"))
library(igraph)
library(qcc)

#####--------------SP HJ 05---------------------####
# Data Inlet DO
data.inlet.DO <- data.frame(
  Date = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Value = c(1.17, 0.49, 1.15, 0.37, 1.05, 0.43, 1.09, 1.17, 0.91, 1.39, 1.19, 1.35)
)

# Calculate demerit points
mean_value.inlet.DO <- mean(data.inlet.DO$Value)
sd_value.inlet.DO <- sd(data.inlet.DO$Value)

# Control limits
UCL.inlet.DO <- mean_value.inlet.DO +  3 * sd_value.inlet.DO
LCL.inlet.DO <- mean_value.inlet.DO -  3 * sd_value.inlet.DO

# Function to calculate membership value based on control limits
membership_value.inlet.DO <- function(x, LCL.inlet.DO, UCL.inlet.DO) {
  if (x <= LCL.inlet.DO) {
    return(1)
  } else if (x >= UCL.inlet.DO) {
    return(0)
  } else {
    return((UCL.inlet.DO - x) / (UCL.inlet.DO - LCL.inlet.DO))
  }
}

# Calculate membership values for each data point
data.inlet.DO$Membership <- sapply(data.inlet.DO$Value, membership_value.inlet.DO, LCL = LCL.inlet.DO, UCL = UCL.inlet.DO)

# Create a control chart
qcc_chart.inlet.DO <- qcc(data.inlet.DO$Value, type = "xbar.one", title = "Control Chart Inlet Dissolved Oxygen (DO)", ylab = "Values", xlab = "Date")

# Data Inlet bod
data.inlet.BOD <- data.frame(
  Date = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Value = c(6.69,6.05,6.36,6.15,6.15,6.35,6.05,6.61,6.21,6.59,6.93,6.36)
)

# Calculate demerit points
mean_value.inlet.BOD <- mean(data.inlet.BOD$Value)
sd_value.inlet.BOD <- sd(data.inlet.BOD$Value)

# Control limits
UCL.inlet.BOD <- mean_value.inlet.BOD +  3 * sd_value.inlet.BOD
LCL.inlet.BOD <- mean_value.inlet.BOD -  3 * sd_value.inlet.BOD

# Function to calculate membership value based on control limits
membership_value.inlet.BOD <- function(x, LCL.inlet.BOD, UCL.inlet.BOD) {
  if (x <= LCL.inlet.BOD) {
    return(1)
  } else if (x >= UCL.inlet.BOD) {
    return(0)
  } else {
    return((UCL.inlet.BOD - x) / (UCL.inlet.BOD - LCL.inlet.BOD))
  }
}

# Calculate membership values for each data point
data.inlet.BOD$Membership <- sapply(data.inlet.BOD$Value, membership_value.inlet.BOD, LCL = LCL.inlet.BOD, UCL = UCL.inlet.BOD)

# Create a control chart
qcc_chart.inlet.BOD <- qcc(data.inlet.BOD$Value, type = "xbar.one", title = "Control Chart Inlet Biological Oxygen Demand (BOD)", ylab = "Values", xlab = "Date")

# Data Outlet DO
data.outlet.DO <- data.frame(
  Date = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Value = c(4.33,4.21,4.69,4.93,5.36,4.86,4.55,4.67,5.07,5.15,4.75,4.85)
)

# Calculate demerit points
mean_value.outlet.DO <- mean(data.outlet.DO$Value)
sd_value.outlet.DO <- sd(data.outlet.DO$Value)

# Control limits
UCL.outlet.DO <- mean_value.outlet.DO +  3 * sd_value.outlet.DO
LCL.outlet.DO <- mean_value.outlet.DO -  3 * sd_value.outlet.DO

# Function to calculate membership value based on control limits
membership_value.outlet.DO <- function(x, LCL.outlet.DO, UCL.outlet.DO) {
  if (x <= LCL.outlet.DO) {
    return(1)
  } else if (x >= UCL.outlet.DO) {
    return(0)
  } else {
    return((UCL.outlet.DO - x) / (UCL.outlet.DO - LCL.outlet.DO))
  }
}

# Calculate membership values for each data point
data.outlet.DO$Membership <- sapply(data.outlet.DO$Value, membership_value.outlet.DO, LCL = LCL.outlet.DO, UCL = UCL.outlet.DO)

# Create a control chart
qcc_chart.outlet.DO <- qcc(data.outlet.DO$Value, type = "xbar.one", title = "Control Chart Outlet Dissolved Oxygen (DO)", ylab = "Values", xlab = "Date")

# Data Outlet BOD
data.outlet.BOD <- data.frame(
  Date = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Value = c(2.03,2.19,1.09,2.33,2.17,2.49,2.15,2.35,2.05,2.41,2.13,2.49)
)

# Calculate demerit points
mean_value.outlet.BOD <- mean(data.outlet.BOD$Value)
sd_value.outlet.BOD <- sd(data.outlet.BOD$Value)

# Control limits
UCL.outlet.BOD <- mean_value.outlet.BOD +  3 * sd_value.outlet.BOD
LCL.outlet.BOD <- mean_value.outlet.BOD -  3 * sd_value.outlet.BOD

# Function to calculate membership value based on control limits
membership_value.outlet.BOD <- function(x, LCL.outlet.BOD, UCL.outlet.BOD) {
  if (x <= LCL.outlet.BOD) {
    return(1)
  } else if (x >= UCL.outlet.BOD) {
    return(0)
  } else {
    return((UCL.outlet.BOD - x) / (UCL.outlet.BOD - LCL.outlet.BOD))
  }
}

# Calculate membership values for each data point
data.outlet.BOD$Membership <- sapply(data.outlet.BOD$Value, membership_value.outlet.BOD, LCL = LCL.outlet.BOD, UCL = UCL.outlet.BOD)

# Create a control chart
qcc_chart.outlet.BOD <- qcc(data.outlet.BOD$Value, type = "xbar.one", title = "Control Chart Outlet Biological Oxygen Demand (BOD)", ylab = "Values", xlab = "Date")

#####--------------SP HJ 09---------------------####
# Data Inlet DO
data.inlet.DO <- data.frame(
  Date = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Value = c(0.9,0.09,0.35,0.3,0.15,0.21,0.69,0.33,0.21,0.69,0.93,0.36)
)

# Calculate demerit points
mean_value.inlet.DO <- mean(data.inlet.DO$Value)
sd_value.inlet.DO <- sd(data.inlet.DO$Value)

# Control limits
UCL.inlet.DO <- mean_value.inlet.DO +  3 * sd_value.inlet.DO
LCL.inlet.DO <- mean_value.inlet.DO -  3 * sd_value.inlet.DO

# Function to calculate membership value based on control limits
membership_value.inlet.DO <- function(x, LCL.inlet.DO, UCL.inlet.DO) {
  if (x <= LCL.inlet.DO) {
    return(1)
  } else if (x >= UCL.inlet.DO) {
    return(0)
  } else {
    return((UCL.inlet.DO - x) / (UCL.inlet.DO - LCL.inlet.DO))
  }
}

# Calculate membership values for each data point
data.inlet.DO$Membership <- sapply(data.inlet.DO$Value, membership_value.inlet.DO, LCL = LCL.inlet.DO, UCL = UCL.inlet.DO)

# Create a control chart
qcc_chart.inlet.DO <- qcc(data.inlet.DO$Value, type = "xbar.one", title = "Control Chart Inlet Dissolved Oxygen (DO)", ylab = "Values", xlab = "Date")

# Data Inlet bod
data.inlet.BOD <- data.frame(
  Date = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Value = c(6.65,6.66,6.63,4.63,6.69,6.93,5.36,4.86,6.67,6.69,6.65,4.37)
)

# Calculate demerit points
mean_value.inlet.BOD <- mean(data.inlet.BOD$Value)
sd_value.inlet.BOD <- sd(data.inlet.BOD$Value)

# Control limits
UCL.inlet.BOD <- mean_value.inlet.BOD +  3 * sd_value.inlet.BOD
LCL.inlet.BOD <- mean_value.inlet.BOD -  3 * sd_value.inlet.BOD

# Function to calculate membership value based on control limits
membership_value.inlet.BOD <- function(x, LCL.inlet.BOD, UCL.inlet.BOD) {
  if (x <= LCL.inlet.BOD) {
    return(1)
  } else if (x >= UCL.inlet.BOD) {
    return(0)
  } else {
    return((UCL.inlet.BOD - x) / (UCL.inlet.BOD - LCL.inlet.BOD))
  }
}

# Calculate membership values for each data point
data.inlet.BOD$Membership <- sapply(data.inlet.BOD$Value, membership_value.inlet.BOD, LCL = LCL.inlet.BOD, UCL = UCL.inlet.BOD)

# Create a control chart
qcc_chart.inlet.BOD <- qcc(data.inlet.BOD$Value, type = "xbar.one", title = "Control Chart Inlet Biological Oxygen Demand (BOD)", ylab = "Values", xlab = "Date")

# Data Outlet DO
data.outlet.DO <- data.frame(
  Date = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Value = c(4.15,4.35,4.05,4.41,4.13,4.49,4.17,4.94,4.49,4.35,4.34,4.15)
)

# Calculate demerit points
mean_value.outlet.DO <- mean(data.outlet.DO$Value)
sd_value.outlet.DO <- sd(data.outlet.DO$Value)

# Control limits
UCL.outlet.DO <- mean_value.outlet.DO +  3 * sd_value.outlet.DO
LCL.outlet.DO <- mean_value.outlet.DO -  3 * sd_value.outlet.DO

# Function to calculate membership value based on control limits
membership_value.outlet.DO <- function(x, LCL.outlet.DO, UCL.outlet.DO) {
  if (x <= LCL.outlet.DO) {
    return(1)
  } else if (x >= UCL.outlet.DO) {
    return(0)
  } else {
    return((UCL.outlet.DO - x) / (UCL.outlet.DO - LCL.outlet.DO))
  }
}

# Calculate membership values for each data point
data.outlet.DO$Membership <- sapply(data.outlet.DO$Value, membership_value.outlet.DO, LCL = LCL.outlet.DO, UCL = UCL.outlet.DO)

# Create a control chart
qcc_chart.outlet.DO <- qcc(data.outlet.DO$Value, type = "xbar.one", title = "Control Chart Outlet Dissolved Oxygen (DO)", ylab = "Values", xlab = "Date")

# Data Outlet BOD
data.outlet.BOD <- data.frame(
  Date = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Value = c(2.17,1.92,2.29,1.35,2.34,1.55,2.33,1.21,2.69,1.93,2.36,1.06)
)

# Calculate demerit points
mean_value.outlet.BOD <- mean(data.outlet.BOD$Value)
sd_value.outlet.BOD <- sd(data.outlet.BOD$Value)

# Control limits
UCL.outlet.BOD <- mean_value.outlet.BOD +  3 * sd_value.outlet.BOD
LCL.outlet.BOD <- mean_value.outlet.BOD -  3 * sd_value.outlet.BOD

# Function to calculate membership value based on control limits
membership_value.outlet.BOD <- function(x, LCL.outlet.BOD, UCL.outlet.BOD) {
  if (x <= LCL.outlet.BOD) {
    return(1)
  } else if (x >= UCL.outlet.BOD) {
    return(0)
  } else {
    return((UCL.outlet.BOD - x) / (UCL.outlet.BOD - LCL.outlet.BOD))
  }
}

# Calculate membership values for each data point
data.outlet.BOD$Membership <- sapply(data.outlet.BOD$Value, membership_value.outlet.BOD, LCL = LCL.outlet.BOD, UCL = UCL.outlet.BOD)

# Create a control chart
qcc_chart.outlet.BOD <- qcc(data.outlet.BOD$Value, type = "xbar.one", title = "Control Chart Outlet Biological Oxygen Demand (BOD)", ylab = "Values", xlab = "Date")

