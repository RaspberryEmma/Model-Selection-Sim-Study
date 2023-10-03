# ****************************************
# Variable Selection Simulation Study
# 
# Synthetic Data Generation
# 
# Emma Tarmey
#
# Started:          09/04/2023
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


# ----- Synthetic Data Generation -----

draw.DAG.1 <- function() {
  # specify relationships and node positions
  causal_dag <- ggdag::dagify(y ~ x.1 + x.2 + x.3,
                              c.1 ~ c.1,
                              c.2 ~ c.2,
                              coords = list(x = c(y = 2.0, x.1 = 1.0, x.2 = 2.0, x.3 = 3.0, c.1 = 1.5, c.2 = 2.5),
                                            y = c(y = 1.0, x.1 = 2.0, x.2 = 2.0, x.3 = 2.0, c.1 = 3.0, c.2 = 3.0)))
  
  # plot DAG
  p <- ggdag(causal_dag) +
    theme_dag() +
    ggtitle("Synthetic Data Causal DAG - Scenario 1")
  
  return (p)
}

get.true.param.1 <- function() {
  # id, c1, c2, x1, x2, x3
  return ( c(0.0, 0.0, 0.0, 1.0, 1.0, 1.0) )
}

generate.data.1 <- function(sample.size = 1000, conf.sd     = c(2.5, 2.5)) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 10, sd = conf.sd[1]),
    c.2 = rnorm(n, mean = 5,  sd = conf.sd[2]),
    
    # generate independent variables
    x.1 = rnorm(n, mean = 5,  sd = 7.5),
    x.2 = rnorm(n, mean = 20, sd = 2.5),
    x.3 = rnorm(n, mean = 15, sd = 3.5)
    
  ) %>% 
    mutate(
      # response variable
      y = x.1 + x.2 + x.3 + rnorm(n, mean = 0, sd = 3)
    )
  
  # re-order columns
  synthetic.data %>% setcolorder( c("id", "c.1", "c.2", "x.1", "x.2", "x.3", "y") )
  
  # return result
  return (synthetic.data)
}



draw.DAG.2 <- function() {
  # specify relationships and node positions
  causal_dag <- ggdag::dagify(y ~ x.1 + x.2 + x.3,
                              x.2 ~ c.1,
                              c.2 ~ c.2,
                              coords = list(x = c(y = 2.0, x.1 = 1.0, x.2 = 2.0, x.3 = 3.0, c.1 = 1.5, c.2 = 2.5),
                                            y = c(y = 1.0, x.1 = 2.0, x.2 = 2.0, x.3 = 2.0, c.1 = 3.0, c.2 = 3.0)))
  
  # plot DAG
  p <- ggdag(causal_dag) +
    theme_dag() +
    ggtitle("Synthetic Data Causal DAG - Scenario 2")
  
  return (p)
}

get.true.param.2 <- function() {
  # id, c1, c2, x1, x2, x3
  # c.1 = 0 here as x.1 blocks in causal pathway
  return ( c(0.0, 0.0, 0.0, 1.0, 1.0, 1.0) )
}

generate.data.2 <- function(sample.size = 1000, conf.sd = c(2.5, 2.5)) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 10, sd = conf.sd[1]),
    c.2 = rnorm(n, mean = 5,  sd = conf.sd[2]),
    
    # generate independent variable
    x.1 = rnorm(n, mean = 5,  sd = 7.5),
    x.3 = rnorm(n, mean = 15, sd = 3.5)
    
  ) %>% 
    mutate(
      # generate dependent variables with noise
      x.2 = (c.1 * 0.5) + rnorm(n, mean = 0, sd = 1),
      
      # response variable
      y = x.1 + x.2 + x.3 + rnorm(n, mean = 0, sd = 3)
    )
  
  # re-order columns
  synthetic.data %>% setcolorder( c("id", "c.1", "c.2", "x.1", "x.2", "x.3", "y") )
  
  # return result
  return (synthetic.data)
}



draw.DAG.3 <- function() {
  # specify relationships and node positions
  causal_dag <- ggdag::dagify(y ~ x.1 + x.2 + x.3,
                              x.1 ~ c.1,
                              x.2 ~ c.1,
                              c.2 ~ c.2,
                              coords = list(x = c(y = 2.0, x.1 = 1.0, x.2 = 2.0, x.3 = 3.0, c.1 = 1.5, c.2 = 2.5),
                                            y = c(y = 1.0, x.1 = 2.0, x.2 = 2.0, x.3 = 2.0, c.1 = 3.0, c.2 = 3.0)))
  
  # plot DAG
  p <- ggdag(causal_dag) +
    theme_dag() +
    ggtitle("Synthetic Data Causal DAG - Scenario 3")
  
  return (p)
}

get.true.param.3 <- function() {
  # id, c1, c2, x1, x2, x3
  # c.1 = 0 here as x.1 blocks in causal pathway
  # c.2 = 0 here as x.2 blocks in causal pathway
  return ( c(0.0, 0.0, 0.0, 1.0, 1.0, 1.0) )
}

generate.data.3 <- function(sample.size = 1000, conf.sd = c(2.5, 2.5)) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 10, sd = conf.sd[1]),
    c.2 = rnorm(n, mean = 5,  sd = conf.sd[2]),
    
    # generate independent variable
    x.3 = rnorm(n, mean = 15, sd = 3.5)
    
  ) %>% 
    mutate(
      # generate dependent variables with noise
      x.1 = (c.1 * 0.15) + rnorm(n, mean = 0, sd = 1),
      x.2 = (c.1 * 0.05) + rnorm(n, mean = 0, sd = 1),
      
      # response variable
      y = x.1 + x.2 + x.3 + rnorm(n, mean = 0, sd = 1)
    )
  
  # re-order columns
  synthetic.data %>% setcolorder( c("id", "c.1", "c.2", "x.1", "x.2", "x.3", "y") )
  
  # return result
  return (synthetic.data)
}



draw.DAG.4 <- function() {
  # specify relationships and node positions
  causal_dag <- ggdag::dagify(y ~ x.1 + x.2 + x.3,
                              x.1 ~ c.1,
                              x.2 ~ c.1 + c.2,
                              coords = list(x = c(y = 2.0, x.1 = 1.0, x.2 = 2.0, x.3 = 3.0, c.1 = 1.5, c.2 = 2.5),
                                            y = c(y = 1.0, x.1 = 2.0, x.2 = 2.0, x.3 = 2.0, c.1 = 3.0, c.2 = 3.0)))
  
  # plot DAG
  p <- ggdag(causal_dag) +
    theme_dag() +
    ggtitle("Synthetic Data Causal DAG - Scenario 4")
  
  return (p)
}

get.true.param.4 <- function() {
  # id, c1, c2, x1, x2, x3
  # c.1 = 0 here as x.1 blocks in causal pathway
  # c.2 = 0 here as x.2 blocks in causal pathway
  return ( c(0.0, 0.0, 0.0, 1.0, 1.0, 1.0) )
}

generate.data.4 <- function(sample.size = 1000, conf.sd = c(2.5, 2.5)) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 10, sd = conf.sd[1]),
    c.2 = rnorm(n, mean = 5,  sd = conf.sd[2]),
    
    # generate independent variable
    x.3 = rnorm(n, mean = 15, sd = 3.5)
    
  ) %>% 
    mutate(
      # generate dependent variables with noise
      x.1 = (c.1 * 0.10) + runif(n, min = 0, max = 1),
      x.2 = (c.1 * 0.05) + (c.2 * 0.11) + rnorm(n, mean = 0, sd = 1),
      
      # response variable
      y = x.1 + x.2 + x.3 + rnorm(n, mean = 0, sd = 1)
    )
  
  # re-order columns
  synthetic.data %>% setcolorder( c("id", "c.1", "c.2", "x.1", "x.2", "x.3", "y") )
  
  # return result
  return (synthetic.data)
}



