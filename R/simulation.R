# ****************************************
# Variable Selection Simulation Study
# 
# Perform Variable Selection and Build Models
# 
# Emma Tarmey
#
# Started:          11/04/2023
# Most Recent Edit: 02/11/2023
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
  library(MASS)
  library(ncvreg)
  library(scales)
  library(sjmisc)
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
  lasso.param  <- current.params[2, ]
  ridge.param  <- current.params[3, ]
  scad.param   <- current.params[4, ]
  mcp.param    <- current.params[5, ]
  step.param   <- current.params[6, ]
  
  # Parameter Bias measurement
  linear.bias <- param.bias(linear.param, param.true)
  lasso.bias  <- param.bias(lasso.param,  param.true)
  ridge.bias  <- param.bias(ridge.param,  param.true)
  scad.bias   <- param.bias(scad.param,   param.true)
  mcp.bias    <- param.bias(mcp.param,    param.true)
  step.bias   <- param.bias(step.param,   param.true)
  
  # Return biases
  return( matrix( data  = c(linear.bias, lasso.bias, ridge.bias, scad.bias, mcp.bias, step.bias),
                  nrow  = 6,
                  ncol  = length(linear.bias),
                  byrow = TRUE ) )
}

reorder.labels <- function(params = NULL, labels = NULL) {
  reordered.params <- rep(0, length(labels))
  match.index      <- 0
  match.value      <- 0
  
  for (i in c(1:length(labels))) {
    label <- labels[i]
    
    #print(label)
    #print(names(params))
    #print(str_detect(names(params), label))
    #writeLines("\n")
    
    # find label in params vector names
    match.index <- match(TRUE, str_detect(names(params), label))
    
    # extract corresponding value
    match.value <- params[match.index]
    
    # assign param in appropriate position
    reordered.params[i] <- match.value
  }

  return (reordered.params)
}

reorder.labels.silent <- function(params = NULL, labels = NULL) {
  reordered.params <- rep(0, length(labels))
  match.index      <- 0
  match.value      <- 0
  
  for (i in c(1:length(labels))) {
    label <- labels[i]
    
    # find label in params vector names
    match.index <- match(TRUE, str_detect(names(params), label))
    
    # extract corresponding value
    match.value <- params[match.index]
    
    # assign param in appropriate position
    reordered.params[i] <- match.value
  }
  
  return (reordered.params)
}

# model fitting occurs here
all.params <- function(current.data = NULL, var.labels = NULL) {
  # handle missingness
  # TODO: expand here to do something more interesting
  # e.g multiple imputation
  current.data <- na.omit(current.data)
  
  # Separate input matrix (X) from output (y) with column selection
  X.train <- current.data %>% subset( select = -c(y) )
  y.train <- current.data$y
  
  # generate penalty.factor sequence using var.labels
  # ensure exposures (variables marked with 'x') are always included
  penalty.factor <- rep(1, length(var.labels))
  for (i in 1:length(var.labels)) {
    if ( sjmisc::str_contains(var.labels[i], "x") ) { penalty.factor[i] <- 0 }
  }
  #print(var.labels)
  #print(penalty.factor)
  
  # Model fitting
  linear.model <- lm(y.train ~ as.matrix(X.train))
  lasso.model  <- glmnet::glmnet(X.train, y.train, alpha = 1, family.train = "gaussian", intercept = F, penalty.factor = penalty.factor)
  ridge.model  <- glmnet::glmnet(X.train, y.train, alpha = 0, family.train = "gaussian", intercept = F, penalty.factor = penalty.factor)
  scad.model   <- ncvreg::ncvreg(X.train, y.train, family.train = c("gaussian"), penalty.train = c("SCAD"), penalty.factor = penalty.factor)
  mcp.model    <- ncvreg::ncvreg(X.train, y.train, family.train = c("gaussian"), penalty.train = c("MCP"),  penalty.factor = penalty.factor)
  
  # TODO - fix here!
  step.model   <- MASS::stepAIC(linear.model, direction = "both", trace = FALSE)
  #step.model   <- MASS::stepAIC(linear.model, direction = "both", trace = FALSE,
  #                              scope = list(lower = as.formula(y ~ x.1 + x.2 + x.3), upper = linear.model))
  
  
  
  # Extract fitted model parameters
  
  ## lm object type
  linear.param <- linear.model$coefficients[2:7]
  linear.param <- reorder.labels.silent(linear.param, var.labels)
  
  ## glmnet object type
  lasso.param <- coef(lasso.model, s = 0.1)[,1]
  lasso.param <- reorder.labels.silent(lasso.param, var.labels)
  
  ## glmnet object type
  ridge.param <- coef(ridge.model, s = 0.1)[,1]
  ridge.param <- reorder.labels.silent(ridge.param, var.labels)
  
  ## ncvreg object type
  scad.param <- coef(scad.model, lambda = scad.model$lambda[50])
  scad.param <- reorder.labels.silent(scad.param, var.labels)
  
  ## ncvreg object type
  mcp.param <- coef(mcp.model, lambda = mcp.model$lambda[50])
  mcp.param <- reorder.labels.silent(mcp.param, var.labels)
  
  ## MASS object type
  step.param <- coef(step.model)
  step.param <- reorder.labels.silent(step.param, var.labels)
  
  
  # Return params
  return( matrix( data  = c(linear.param, lasso.param, ridge.param, scad.param, mcp.param, step.param),
                  nrow  = 6,
                  ncol  = length(linear.param),
                  byrow = TRUE ) )
}



# ----- Simulating Scenario Function -----

# generic function to simulate a given scenario s

sim.scenario <- function(S       = NULL,
                         s       = NULL,
                         conf.sd = NULL,
                         N       = NULL,
                         M       = NULL,
                         p       = NULL,
                         n       = NULL,
                         method.labels = NULL,
                         coef.labels   = NULL,
                         var.labels    = NULL,
                         mech.missing  = NULL,
                         prop.missing  = NULL,
                         messages      = FALSE) {
  
    current.data  <- NULL
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
        if (messages) {
          writeLines("\n")
          message(paste("Scenario ",  s, "/", S))
          message(paste("Iteration ", i, "/", N))
        }
      
        # Generate synthetic data
        current.data <- do.call( paste("generate.data.", s, sep = ""),
                                 list( sample.size = n, conf.sd = conf.sd, mech.missing = mech.missing, prop.missing = prop.missing ) )
        param.true   <- do.call( paste("get.true.param.", s, sep = ""), list() )
        
        
        # Fit models, determine parameters and biases of each model
        current.params <- all.params(current.data, var.labels)
        current.biases <- all.biases(current.params, param.true)
        
        if (messages) {
          writeLines("\nParams:")
          current.params %>% print()
          writeLines("\nBiases:")
          current.biases %>% print()
        }
        
        # Record coefficient results
        coef.results[i, 1,       ] <- param.true
        coef.results[i, 2:(M+1), ] <- current.params
        
        # Record bias results
        bias.results[i, , ] <- current.biases
    }
    
    if (messages) {
      # check results
      bias.results[1, , ] %>% knitr::kable()
      coef.results[1, , ] %>% knitr::kable()
    }
    
    return ( list(bias.results, coef.results) )
}


# ----- Entry Point -----

run.simulation <- function(S = NULL,
                           N = NULL,
                           M = NULL,
                           p = NULL,
                           n = NULL,
                           mech.missing = "none",
                           prop.missing = 0.0,
                           messages = NULL) {

  # reproducibility
  set.seed(2023)
  
  # simulation parameters
  #S <- S  # number of scenarios
  #N <- N  # repetitions for this scenario
  #M <- M  # number of VS techniques under investigation 
  #p <- p  # number of variables in data (includes id, excludes intercept and outcome y)
  #n <- n  # synthetic data-set size (10,000)
  
  # control standard deviations of confounder variables
  conf.sd.gaps  <- c(0, 1, 2)
  c1.sd         <- 2.5
  c2.sd         <- 2.5
  
  # labels for interpretability
  method.labels <- c("linear", "lasso", "ridge", "scad", "mcp", "stepwise")
  coef.labels   <- c("true", "linear", "lasso", "ridge", "scad", "mcp", "stepwise")
  var.labels    <- c("id", "c.1", "c.2", "x.1", "x.2", "x.3")
  
  # holder objects
  sim               <- NULL
  bias.results      <- NULL
  coef.results      <- NULL
  
  # fixed conf sd gap
  for (s in 1:S) {
      message(paste("Scenario ", s, "/", 4))
      
      # run simulation N times for a given scenario, fixing all parameters
      sim <- sim.scenario(S       = S,
                          s       = s,
                          conf.sd = c(c1.sd, c2.sd),
                          N       = N,
                          M       = M,
                          p       = p,
                          n       = n,
                          method.labels = method.labels,
                          coef.labels   = coef.labels,
                          var.labels    = var.labels,
                          mech.missing  = mech.missing,
                          prop.missing  = prop.missing,
                          messages      = messages)
      
      # extract estimands of interest
      bias.results <- sim[[1]]
      coef.results <- sim[[2]]
      
      assign(paste("bias.results.s", s, sep = ""), bias.results)
      assign(paste("coef.results.s", s, sep = ""), coef.results)
      
      save( list = paste("bias.results.s", s, sep = ""),
            file = paste("../data/bias_results_s", s , ".rda", sep = "") )
      
      save( list = paste("coef.results.s", s, sep = ""),
            file = paste("../data/coef_results_s", s , ".rda", sep = "") )
      
  }

}







