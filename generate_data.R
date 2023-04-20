# ****************************************
# Variable Selection Simulation Study
# 
# Synthetic Data Generation
# 
# Emma Tarmey
#
# Started:          09/04/2023
# Most Recent Edit: 17/04/2023
# ****************************************


# ----- Preamble -----

rm(list = ls())

suppressPackageStartupMessages({
  library(broom)
  library(data.table)
  library(dplyr)
  library(ggcorrplot)
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


# ----- Drawing Causal DAG -----

draw.DAG.1 <- function() {
  # specify relationships and node positions
  causal_dag <- ggdag::dagify(y ~ x.1 + x.2 + x.3,
                       x.1 ~ c.1,
                       x.2 ~ c.1 + c.2,
                       coords = list(x = c(y = 2.0, x.1 = 1.0, x.2 = 2.0, x.3 = 3.0, c.1 = 1.5, c.2 = 2.5),
                                     y = c(y = 1.0, x.1 = 2.0, x.2 = 2.0, x.3 = 2.0, c.1 = 3.0, c.2 = 3.0)))
  
  # plot DAG
  p <- ggdag(causal_dag) +
       theme_dag() +
       ggtitle("Synthetic Data Causal DAG - Scenario 1")
  
  return (p)
}

draw.DAG.2 <- function() {
  # specify relationships and node positions
  causal_dag <- ggdag::dagify(y ~ x.1 + x.2 + x.3,
                              c.1 ~ c.1,
                              c.2 ~ c.2,
                              coords = list(x = c(y = 2.0, x.1 = 1.0, x.2 = 2.0, x.3 = 3.0, c.1 = 1.5, c.2 = 2.5),
                                            y = c(y = 1.0, x.1 = 2.0, x.2 = 2.0, x.3 = 2.0, c.1 = 3.0, c.2 = 3.0)))
  
  # plot DAG
  p <- ggdag(causal_dag) +
    theme_dag() +
    ggtitle("Synthetic Data Causal DAG - Scenario 2")
  
  return (p)
}


# ----- Synthetic Data Generation -----

generate.data.1 <- function(sample.size = 1000) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 100, sd = 75),
    c.2 = rnorm(n, mean = 50,  sd = 2.5),
    
    # generate independent variable
    x.3 = runif(n, min = 40, max = 90)
    
  ) %>% 
  mutate(
    # generate dependent variables with noise
    x.1 = (c.1 * 0.02) + runif(n, min = 0, max = 1),
    x.2 = (c.1 * 0.05) + runif(n, min = 0, max = 1),
    
    # response variable
    y = x.1 + x.2 + x.3 + runif(n, min = 0, max = 1),
    y = rescale(y, to = c(min(y), 100))
  )
  
  # re-order columns
  synthetic.data %>% setcolorder( c("id", "c.1", "c.2", "x.1", "x.2", "x.3", "y") )
  
  # re-scale columns
  #synthetic.data <- scale(synthetic.data)
  
  # return result
  return (synthetic.data)
}


generate.data.2 <- function(sample.size = 1000) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 100, sd = 75),
    c.2 = rnorm(n, mean = 5,  sd = 2.5),
    
    # generate independent variables
    x.1 = rweibull(n, shape = 1, scale = 1),
    x.2 = rgeom(n, prob = 0.3),
    x.3 = runif(n, min = 40, max = 90)
    
  ) %>% 
    mutate(
      # response variable
      y = x.1 + x.2 + x.3 + rnorm(n, mean = 0, sd = 3),
      y = rescale(y, to = c(min(y), 100))
    )
  
  # re-order columns
  synthetic.data %>% setcolorder( c("id", "c.1", "c.2", "x.1", "x.2", "x.3", "y") )
  
  # re-scale columns
  #synthetic.data <- scale(synthetic.data)
  
  # return result
  return (synthetic.data)
}


# ----- Test Scenario 1 -----

# open png file
png("plots/synthetic_data_s1_DAG.png")

p <- draw.DAG.1()
p

# close png file
dev.off()

# data - scenario - trial
data.1.1 <- generate.data.1()
data.1.2 <- generate.data.1()
data.1.3 <- generate.data.1()

data.1.1 %>% head() %>% knitr::kable()
data.1.2 %>% head() %>% knitr::kable()
data.1.3 %>% head() %>% knitr::kable()

data.1.1 %>% cor()
data.1.2 %>% cor()
data.1.3 %>% cor()

# Correlation plot
png("plots/synthetic_data_s1_t1_corr.png")
p <- data.1.1 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 1 - Trial 1")
p
dev.off()

png("plots/synthetic_data_s1_t2_corr.png")
p <- data.1.2 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 1 - Trial 2")
p
dev.off()

png("plots/synthetic_data_s1_t3_corr.png")
p <- data.1.3 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 1 - Trial 3")
p
dev.off()


# ----- Test Scenario 2 -----

# DAG plot
png("plots/synthetic_data_s2_DAG.png")
p <- draw.DAG.2()
p
dev.off()

# data - scenario - trial
data.2.1 <- generate.data.2()
data.2.2 <- generate.data.2()
data.2.3 <- generate.data.2()

data.2.1 %>% head() %>% knitr::kable()
data.2.2 %>% head() %>% knitr::kable()
data.2.3 %>% head() %>% knitr::kable()

# Correlation plot
png("plots/synthetic_data_s2_t1_corr.png")
p <- data.2.1 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 2 - Trial 1")
p
dev.off()

png("plots/synthetic_data_s2_t2_corr.png")
p <- data.2.2 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 2 - Trial 2")
p
dev.off()

png("plots/synthetic_data_s2_t3_corr.png")
p <- data.2.3 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 2 - Trial 3")
p
dev.off()




