# ****************************************
# Variable Selection Simulation Study
# 
# Perform Variable Selection and Build Models
# 
# Emma Tarmey
#
# Started:          11/04/2023
# Most Recent Edit: 26/05/2023
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

outcome.bias <- function(pred, real) {
  return (mean(pred) - mean(real))
}

param.bias <- function(param.values, true.values) {
  return (param.values - true.values)
}

all.biases <- function(current.data = NULL, param.true = NULL) {
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
  linear.model <- lm(y.train ~ as.matrix(X.train))
  lasso.model  <- glmnet::glmnet(X.train, y.train, alpha = 1, family.train = "gaussian", intercept = F)
  ridge.model  <- glmnet::glmnet(X.train, y.train, alpha = 0, family.train = "gaussian", intercept = F)
  scad.model   <- ncvreg::ncvreg(X.train, y.train, family.train = c("gaussian"), penalty.train = c("SCAD"))
  mcp.model    <- ncvreg::ncvreg(X.train, y.train, family.train = c("gaussian"), penalty.train = c("MCP"))
  
  # error checking
  #"Model!" %>% message()
  #View(lasso.model)
  #View(ridge.model)
  #View(scad.model)
  #View(mcp.model)
  
  
  # Extract fitted model parameters
  ## lm object type
  linear.param <- linear.model$coefficients[2:7]
  
  ## glmnet object type
  lasso.param  <- (lasso.model$beta) %>% rowMeans()
  ridge.param  <- (ridge.model$beta) %>% rowMeans()
  
  ## ncvreg object type
  scad.param   <- scad.model$beta[2:7]
  mcp.param    <- mcp.model$beta[2:7]
  
  
  # Parameter Bias measurement
  linear.bias <- param.bias(linear.param, param.true)
  lasso.bias  <- param.bias(lasso.param,  param.true)
  ridge.bias  <- param.bias(ridge.param,  param.true)
  scad.bias   <- param.bias(scad.param,   param.true)
  mcp.bias    <- param.bias(mcp.param,    param.true)
  
  # error checking
  #"Param!" %>% message()
  #linear.bias %>% length() %>% print()
  #lasso.bias %>% length() %>% print()
  #ridge.bias %>% length() %>% print()
  #scad.bias %>% length() %>% print()
  #mcp.bias %>% length() %>% print()
  
  
  # Return biases
  return( array( data = c(linear.bias, lasso.bias, ridge.bias, scad.bias, mcp.bias),
                 dim  = c(5, length(param.true)) ) )
}



# ----- Simulation Scenario 1 -----

set.seed(2023)        # reproducibility
N             <- 1000 # repetitions for this scenario
M             <- 5    # number of VS techniques under investigation 
p             <- 6    # number of variables in data (includes id, excludes intercept and outcome y)
n             <- 1000 # synthetic data-set size
current.data  <- NULL

method.labels <- c("linear", "lasso", "ridge", "scad", "mcp")
var.labels    <- c("id", "c.1", "c.2", "x.1", "x.2", "x.3")
iter.labels   <- paste("i = ", c(1:N))
results.s1    <- array(data     = NA,
                       dim      = c(N, M, p),
                       dimnames = list(iter.labels, method.labels, var.labels) )


for (i in 1:N) {
  # Generate synthetic data
  current.data <- generate.data.1(sample.size = n)
  param.true   <- get.true.param.1()
  
  # Fit models, determine biases of each model
  current.biases <- all.biases(current.data, param.true)
  
  # Record results
  results.s1[i, , ] <- current.biases
}


# check results
results.s1[1, , ] %>% knitr::kable()

# save data
save( results.s1, file = "data/results_s1.rda" )



# ----- Simulation Scenario 2 -----

set.seed(2023)        # reproducibility
N             <- 1000 # repetitions for this scenario
M             <- 5    # number of VS techniques under investigation 
p             <- 6    # number of variables in data (includes id, excludes intercept and outcome y)
n             <- 1000 # synthetic data-set size
current.data  <- NULL

method.labels <- c("linear", "lasso", "ridge", "scad", "mcp")
var.labels    <- c("id", "c.1", "c.2", "x.1", "x.2", "x.3")
iter.labels   <- paste("i = ", c(1:N))
results.s2    <- array(data     = NA,
                       dim      = c(N, M, p),
                       dimnames = list(iter.labels, method.labels, var.labels) )


for (i in 1:N) {
  # Generate synthetic data
  current.data <- generate.data.2(sample.size = n)
  param.true   <- get.true.param.2()
  
  # Fit models, determine biases of each model
  current.biases <- all.biases(current.data, param.true)
  
  # Record results
  results.s2[i, , ] <- current.biases
}


# check results
results.s2[1, , ] %>% knitr::kable()

# save data
save( results.s2, file = "data/results_s2.rda" )



# ----- Simulation Scenario 3 -----

set.seed(2023)        # reproducibility
N             <- 1000 # repetitions for this scenario
M             <- 5    # number of VS techniques under investigation 
p             <- 6    # number of variables in data (includes id, excludes intercept and outcome y)
n             <- 1000 # synthetic data-set size
current.data  <- NULL

method.labels <- c("linear", "lasso", "ridge", "scad", "mcp")
var.labels    <- c("id", "c.1", "c.2", "x.1", "x.2", "x.3")
iter.labels   <- paste("i = ", c(1:N))
results.s3    <- array(data     = NA,
                       dim      = c(N, M, p),
                       dimnames = list(iter.labels, method.labels, var.labels) )


for (i in 1:N) {
  # Generate synthetic data
  current.data <- generate.data.3(sample.size = n)
  param.true   <- get.true.param.3()
  
  # Fit models, determine biases of each model
  current.biases <- all.biases(current.data, param.true)
  
  # Record results
  results.s3[i, , ] <- current.biases
}


# check results
results.s3[1, , ] %>% knitr::kable()

# save data
save( results.s3, file = "data/results_s3.rda" )



# ----- Simulation Scenario 4 -----

set.seed(2023)        # reproducibility
N             <- 1000 # repetitions for this scenario
M             <- 5    # number of VS techniques under investigation 
p             <- 6    # number of variables in data (includes id, excludes intercept and outcome y)
n             <- 1000 # synthetic data-set size
current.data  <- NULL

method.labels <- c("linear", "lasso", "ridge", "scad", "mcp")
var.labels    <- c("id", "c.1", "c.2", "x.1", "x.2", "x.3")
iter.labels   <- paste("i = ", c(1:N))
results.s4    <- array(data     = NA,
                       dim      = c(N, M, p),
                       dimnames = list(iter.labels, method.labels, var.labels) )


for (i in 1:N) {
  # Generate synthetic data
  current.data <- generate.data.4(sample.size = n)
  param.true   <- get.true.param.4()
  
  # Fit models, determine biases of each model
  current.biases <- all.biases(current.data, param.true)
  
  # Record results
  results.s4[i, , ] <- current.biases
}


# check results
results.s4[1, , ] %>% knitr::kable()

# save data
save( results.s4, file = "data/results_s4.rda" )



# ----- Sanity Checks -----

results.s1 %>% dim %>% print
results.s2 %>% dim %>% print
results.s3 %>% dim %>% print
results.s4 %>% dim %>% print



