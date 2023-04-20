# ****************************************
# Variable Selection Simulation Study
# 
# Perform Variable Selection and Build Models
# 
# Emma Tarmey
#
# Started:          11/04/2023
# Most Recent Edit: 17/04/2023
# ****************************************


# ----- Preamble -----

rm(list = ls())

suppressPackageStartupMessages({
  library(broom)
  library(data.table)
  library(dplyr)
  library(ggdag)
  library(ggplot2)
  library(glmnet)
  library(gridExtra)
  library(knitr)
  library(modelr)
  library(ncvreg)
  library(OpenMx)
  library(scales)
  library(sgee)
  library(stringr)
  library(tidyverse)
  library(VARSELECTEXPOSURE)
})

source("generate_data.R")


# ----- Simulation Helper Functions -----

model.bias <- function(pred, real) {
  return (mean(pred) - mean(real))
}

all.biases <- function(current.data = NULL) {
  # Partition test and training sets with index vector
  sample      <- sample(c(TRUE, FALSE), nrow(current.data), replace = TRUE, prob = c(0.7, 0.3))
  train.data  <- current.data[sample, ]
  test.data   <- current.data[!sample, ]
  
  # Separate input matrix (X) from output (y) with column selection
  X.train <- train.data %>% subset( select = -c(y) )
  y.train <- train.data$y
  X.test  <- test.data %>% subset( select = -c(y) )
  y.test  <- test.data$y
  
  # Model fitting
  linear.model <- lm(y.train ~ as.matrix(X.train)) # TODO: fix this !
  lasso.model  <- glmnet::glmnet(X.train, y.train, alpha = 1, family.train = "gaussian", intercept = F)
  ridge.model  <- glmnet::glmnet(X.train, y.train, alpha = 0, family.train = "gaussian", intercept = F)
  scad.model   <- ncvreg::ncvreg(X.train, y.train, family.train = c("gaussian"), penalty.train = c("SCAD"))
  mcp.model    <- ncvreg::ncvreg(X.train, y.train, family.train = c("gaussian"), penalty.train = c("MCP"))
  
  # Predictions
  linear.pred.y <- predict(linear.model, newdata = data.frame(X.test))
  lasso.pred.y  <- predict(lasso.model,  newx = as.matrix(X.test))
  ridge.pred.y  <- predict(ridge.model,  newx = as.matrix(X.test))
  scad.pred.y   <- predict(scad.model,   X = as.matrix(X.test))
  mcp.pred.y    <- predict(mcp.model,    X = as.matrix(X.test))
  
  # Bias measurement
  linear.bias <- model.bias(linear.pred.y, y.test)
  lasso.bias  <- model.bias(lasso.pred.y,  y.test)
  ridge.bias  <- model.bias(ridge.pred.y,  y.test)
  scad.bias   <- model.bias(scad.pred.y,   y.test)
  mcp.bias    <- model.bias(mcp.pred.y,    y.test)
  
  # Return biases
  return( c(linear.bias, lasso.bias, ridge.bias, scad.bias, mcp.bias) )
}


# ----- Simulation Scenario 1 -----

set.seed(2023)        # reproducibility
N             <- 100  # repetitions for this scenario
n             <- 1000 # synthetic data-set size
current.data  <- NULL
results       <- matrix(0, nrow = N, ncol = 5)
method.labels <- c("linear", "lasso", "ridge", "scad", "mcp")


for (i in 1:N) {
  # Generate synthetic data
  current.data <- generate.data.1(sample.size = n)
  
  # Fit models, determine biases of each model
  current.biases <- all.biases(current.data)
  
  # Record results
  results[i, ] <- current.biases
}

# check results
results %>% head %>% knitr::kable(col.names = method.labels)
results           <- as.data.frame(results)
colnames(results) <- method.labels

# save data
write_csv(results, "data/results_s1.csv")


# ----- Simulation Scenario 2 -----

set.seed(2023)        # reproducibility
N             <- 100  # repetitions for this scenario
n             <- 1000 # synthetic data-set size
current.data  <- NULL
results       <- matrix(0, nrow = N, ncol = 5)
method.labels <- c("linear", "lasso", "ridge", "scad", "mcp")


for (i in 1:N) {
  # Generate synthetic data
  current.data <- generate.data.2(sample.size = n)
  
  # Fit models, determine biases of each model
  current.biases <- all.biases(current.data)
  
  # Record results
  results[i, ] <- current.biases
}

# check results
results %>% head %>% knitr::kable(col.names = method.labels)
results           <- as.data.frame(results)
colnames(results) <- method.labels

# save data
write_csv(results, "data/results_s2.csv")


