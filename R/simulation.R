# ****************************************
# Variable Selection Simulation Study
# 
# Perform Variable Selection and Build Models
# 
# Emma Tarmey
#
# Started:          11/04/2023
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
  lasso.param  <- current.params[2, ]
  ridge.param  <- current.params[3, ]
  scad.param   <- current.params[4, ]
  mcp.param    <- current.params[5, ]
  
  # Parameter Bias measurement
  linear.bias <- param.bias(linear.param, param.true)
  lasso.bias  <- param.bias(lasso.param,  param.true)
  ridge.bias  <- param.bias(ridge.param,  param.true)
  scad.bias   <- param.bias(scad.param,   param.true)
  mcp.bias    <- param.bias(mcp.param,    param.true)
  
  # Return biases
  return( matrix( data  = c(linear.bias, lasso.bias, ridge.bias, scad.bias, mcp.bias),
                  nrow  = 5,
                  ncol  = length(linear.bias),
                  byrow = TRUE ) )
}

reorder.labels <- function(params = NULL, labels = NULL) {
  reordered.params <- rep(0, length(labels))
  match.index      <- 0
  match.value      <- 0
  
  for (i in c(1:length(labels))) {
    label <- labels[i]
    
    print(label)
    print(names(params))
    print(str_detect(names(params), label))
    writeLines("\n")
    
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

all.params <- function(current.data = NULL, var.labels = NULL) {
    
    # Partition test and training sets with index vector
    #sample      <- sample(c(TRUE, FALSE), nrow(current.data), replace = TRUE, prob = c(0.7, 0.3))
    #train.data  <- current.data
    #test.data   <- current.data[!sample, ]
    
    
    # Separate input matrix (X) from output (y) with column selection
    X.train <- current.data %>% subset( select = -c(y) )
    y.train <- current.data$y
    
    
    # Model fitting
    linear.model <- lm(y.train ~ as.matrix(X.train))
    lasso.model  <- glmnet::glmnet(X.train, y.train, alpha = 1, family.train = "gaussian", intercept = F)
    ridge.model  <- glmnet::glmnet(X.train, y.train, alpha = 0, family.train = "gaussian", intercept = F)
    scad.model   <- ncvreg::ncvreg(X.train, y.train, family.train = c("gaussian"), penalty.train = c("SCAD"))
    mcp.model    <- ncvreg::ncvreg(X.train, y.train, family.train = c("gaussian"), penalty.train = c("MCP"))
    
    
    # Extract fitted model parameters
    
    ## print labels
    print(var.labels)
    
    ## lm object type
    writeLines("\n *** LINEAR *** \n")
    linear.param <- linear.model$coefficients[2:7]
    linear.param <- reorder.labels.silent(linear.param, var.labels)
    print(linear.param)
    
    ## glmnet object type
    writeLines("\n *** LASSO *** \n")
    lasso.param <- coef(lasso.model, s = 0.1)[,1]
    lasso.param <- reorder.labels.silent(lasso.param, var.labels)
    print(lasso.param)
    
    ## glmnet object type
    writeLines("\n *** RIDGE *** \n")
    ridge.param <- coef(ridge.model, s = 0.1)[,1]
    ridge.param <- reorder.labels.silent(ridge.param, var.labels)
    print(ridge.param)
    
    ## ncvreg object type
    writeLines("\n *** SCAD *** \n")
    scad.param <- coef(scad.model, lambda = 0.08)
    scad.param <- reorder.labels.silent(scad.param, var.labels)
    print(scad.param)
    
    ## ncvreg object type
    writeLines("\n *** MCP *** \n")
    mcp.param <- coef(scad.model, lambda = 0.08)
    mcp.param <- reorder.labels.silent(mcp.param, var.labels)
    print(mcp.param)
    
    # Return params
    return( matrix( data  = c(linear.param, lasso.param, ridge.param, scad.param, mcp.param),
                    nrow  = 5,
                    ncol  = length(linear.param),
                    byrow = TRUE ) )
}



# ----- Simulating Scenario Function -----

# generic function to simulate a given scenario s

sim.scenario <- function(s       = NULL,
                         conf.sd = NULL,
                         N       = NULL,
                         M       = NULL,
                         p       = NULL,
                         n       = NULL,
                         method.labels = NULL,
                         coef.labels   = NULL,
                         var.labels    = NULL) {
  
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
        writeLines("\n")
        message(paste("Scenario ",  s, "/", S))
        message(paste("Iteration ", i, "/", N))
      
        # Generate synthetic data
        current.data <- do.call( paste("generate.data.", s, sep = ""),
                                 list( sample.size = n, conf.sd = conf.sd ) )
        param.true   <- do.call( paste("get.true.param.", s, sep = ""), list() )
        
        
        # Fit models, determine parameters and biases of each model
        current.params <- all.params(current.data, var.labels)
        current.biases <- all.biases(current.params, param.true)
        
        writeLines("\nParams:")
        current.params %>% print()
        writeLines("\nBiases:")
        current.biases %>% print()
        
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


# ----- Entry Point -----

set.seed(2023) # reproducibility

# simulation parameters
S             <- 4      # number of scenarios
N             <- 1000   # repetitions for this scenario
M             <- 5      # number of VS techniques under investigation 
p             <- 6      # number of variables in data (includes id, excludes intercept and outcome y)
n             <- 10000  # synthetic data-set size (100,00)
conf.sd.gaps  <- c(0, 1, 2)
c1.sd         <- 2.5
c2.sd         <- 2.5

# labels for interpretability
method.labels <- c("linear", "lasso", "ridge", "scad", "mcp")
coef.labels   <- c("true", "linear", "lasso", "ridge", "scad", "mcp")
var.labels    <- c("id", "c.1", "c.2", "x.1", "x.2", "x.3")

# holder objects
sim               <- NULL
bias.results      <- NULL
coef.results      <- NULL



# fixed conf sd gap
for (s in 1:S) {
    message(paste("Scenario ", s, "/", 4))
    
    # run simulation N times for a given scenario, fixing all parameters
    sim <- sim.scenario(s       = s,
                        conf.sd = c(c1.sd, c2.sd),
                        N       = N,
                        M       = M,
                        p       = p,
                        n       = n,
                        method.labels = method.labels,
                        coef.labels   = coef.labels,
                        var.labels    = var.labels)
    
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





# conf sd gap variable
#for (gap in conf.sd.gaps) {
#  for (s in 1:S) {
#    results <- sim.scenario(s = s, conf.sd = c(c1.sd - gap, c2.sd + gap))
#    
#    assign(paste("results.s", s, "_conf_gap_", gap, sep = ""), results)
#    
#    save( list = paste("results.s", s, "_conf_gap_", gap, sep = ""),
#          file = paste("../data/results_s", s ,"_conf_gap_", gap, ".rda", sep = "") )
#  }
#}






