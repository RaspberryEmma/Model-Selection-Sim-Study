# ****************************************
# Variable Selection Simulation Study
# 
# Explore Models under Study
# 
# Emma Tarmey
#
# Started:          11/04/2023
# Most Recent Edit: 14/04/2023
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


# ----- Perform Variable Selection -----

synth.data <- generate.data.1()
X <- synth.data %>% select(c.1, c.2, x.1, x.2, x.3)
y <- synth.data$y

# Linear Regression 
linear.model <- lm(y ~ as.matrix(X)) 

# LASSO
# least absolute shrinkage and selection operator penalty
lasso.model <- glmnet::glmnet(X, y,
                              alpha     = 1, # penalty function!
                              family    = "gaussian", 
                              intercept = F)
# Ridge
# ridge shape present in likelihood function -> multicollinearity
ridge.model <- glmnet::glmnet(X, y,
                              alpha     = 0, # penalty function!
                              family    = "gaussian", 
                              intercept = F)

# SCAD
# smoothly clipped absolute deviation penalty
scad.model <- ncvreg::ncvreg(X, y,
                             family = c("gaussian"),
                             penalty = c("SCAD"))

# MCP
# minimax concave penalty
mcp.model <- ncvreg::ncvreg(X, y,
                            family = c("gaussian"),
                            penalty = c("MCP"))

# SEE
# stagewise estimating equation
#see.model <- sgee::hisee(y, X,
#                         family    = gaussian(),
#                         corstr    = "exchangeable", 
#                         control = sgee.control(maxIt = 50, epsilon = 0.5))



# ----- Look at Models Fitted Closely by Type -----
linear.model %>% summary()
lasso.model  %>% summary()
ridge.model  %>% summary()
#scad.model   %>% summary()
#mcp.model    %>% summary()

scad.model$lambda
mcp.model$lambda

