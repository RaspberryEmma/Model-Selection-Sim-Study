# ****************************************
# Variable Selection Simulation Study
# 
# Isolated Linear Regression Scenario 1 Sim
# 
# Emma Tarmey
#
# Started:          28/06/2023
# Most Recent Edit: 03/10/2023
# ****************************************



# ----- Preamble -----

rm(list = ls())

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggcorrplot)
  library(ggdag)
  library(ggplot2)
  library(glmnet)
  library(ncvreg)
  library(scales)
  library(stringr)
  library(tidyverse)
})

if ( !is.element( "RStudio", commandArgs() ) ) {
  setwd("../R") # only needed for bash version, not running in RStudio
}

source("generate_data.R")


# ----- Simulation Helper Functions -----

outcome.bias <- function(pred, real) {
  return (mean(pred) - mean(real))
}

param.bias <- function(param.values, true.values) {
  return (param.values - true.values)
}

all.biases <- function(current.params = NULL, param.true = NULL) {
  
  # Extract fitted model parameters
  linear.param <- current.params[1, ]
  
  # Parameter Bias measurement
  linear.bias <- param.bias(linear.param, param.true)
  
  
  # Return biases
  return( array( data = c(linear.bias),
                 dim  = c(1, length(param.true)) ) )
}

all.params <- function(current.data = NULL) {
  
  # Partition test and training sets with index vector
  #sample      <- sample(c(TRUE, FALSE), nrow(current.data), replace = TRUE, prob = c(0.7, 0.3))
  #train.data  <- current.data
  #test.data   <- current.data[!sample, ]
  
  
  # Separate input matrix (X) from output (y) with column selection
  X.train <- current.data %>% subset( select = -c(y) )
  y.train <- current.data$y
  
  
  # Model fitting
  linear.model <- lm(y.train ~ as.matrix(X.train))
  
  
  # Extract fitted model parameters
  linear.param <- linear.model$coefficients[2:7]
  
  # Return params
  return( array( data = c(linear.param),
                 dim  = c(1, length(linear.param)) ) )
}



# ----- Simulating Scenarios -----

# generic function to simulate a given scenario s
sim.scenario <- function(s = NULL, conf.sd = NULL) {
  N             <- 1      # repetitions for this scenario
  M             <- 1      # number of VS techniques under investigation 
  p             <- 6      # number of variables in data (includes id, excludes intercept and outcome y)
  n             <- 100000 # synthetic data-set size (1,000)
  current.data  <- NULL
  
  method.labels <- c("linear")
  coef.labels   <- c("true", "linear")
  var.labels    <- c("id", "c.1", "c.2", "x.1", "x.2", "x.3")
  iter.labels   <- paste("i = ", c(1:N))
  
  # 3 dimensional matrix of coefficients generated
  coef.results <- array(data     = NA,
                        dim      = c(N, M+1, p),
                        dimnames = list(iter.labels, coef.labels, var.labels) )
  
  # 3 dimensional matrix of error measurements
  bias.results <- array(data     = NA,
                        dim      = c(N, M, p),
                        dimnames = list(iter.labels, method.labels, var.labels) )
  
  for (i in 1:N) {
    # Generate synthetic data
    current.data <- do.call( paste("generate.data.", s, sep = ""),
                             list( sample.size = n, conf.sd = conf.sd ) )
    param.true   <- do.call( paste("get.true.param.", s, sep = ""), list() )
    
    
    # Fit models, determine parameters and biases of each model
    current.params <- all.params(current.data)
    current.biases <- all.biases(current.params, param.true)
    
    
    # Record coefficient results
    coef.results[i, 1,       ] <- param.true
    coef.results[i, 2:(M+1), ] <- current.params
    
    # Record bias results
    bias.results[i, , ] <- current.biases
  }
  
  # check results
  bias.results[1, , ] %>% knitr::kable()
  coef.results[1, , ] %>% knitr::kable()
  
  return ( list(bias.results, coef.results) )
}


dataset <- generate.data.1()


set.seed(2023) # reproducibility
sim               <- NULL
bias.results      <- NULL
coef.results      <- NULL
conf.sd.gaps      <- c(0, 1, 2)
c1.sd             <- 2.5
c2.sd             <- 2.5


s <- 1
sim <- sim.scenario(s = s, conf.sd = c(c1.sd, c2.sd))
bias.results <- sim[[1]]
coef.results <- sim[[2]]

assign(paste("bias.results.s", s, sep = ""), bias.results)
assign(paste("coef.results.s", s, sep = ""), coef.results)

message("Biases:")
bias.results.s1 %>% knitr::kable()

message("\n\n\n\nCoefficient Values:")
coef.results.s1 %>% knitr::kable()




