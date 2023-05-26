# ****************************************
# Variable Selection Simulation Study
# 
# Synthetic Data Generation
# 
# Emma Tarmey
#
# Started:          09/04/2023
# Most Recent Edit: 26/05/2023
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

generate.data.1 <- function(sample.size = 1000) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 10, sd = 2.5),
    c.2 = rnorm(n, mean = 5,  sd = 2.5),
    
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
  return ( c(0.0, 0.5, 0.0, 1.0, 1.0, 1.0) )
}

generate.data.2 <- function(sample.size = 1000) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 10, sd = 2.5),
    c.2 = rnorm(n, mean = 5,  sd = 2.5),
    
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
  return ( c(0.0, 0.20, 0.0, 1.0, 1.0, 1.0) )
}

generate.data.3 <- function(sample.size = 1000) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 10, sd = 2.5),
    c.2 = rnorm(n, mean = 5,  sd = 2.5),
    
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
  return ( c(0.0, 0.15, 0.11, 1.0, 1.0, 1.0) )
}

generate.data.4 <- function(sample.size = 1000) {
  # sample size
  n <- sample.size
  
  # generate the data itself
  synthetic.data <- tibble(
    # unique identifier by row
    id = 1:n,
    
    # generate independent confounder variables
    c.1 = rnorm(n, mean = 10, sd = 2.5),
    c.2 = rnorm(n, mean = 5,  sd = 2.5),
    
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



# ----- Test Scenario 3 -----

# DAG plot
png("plots/synthetic_data_s3_DAG.png")
p <- draw.DAG.3()
p
dev.off()

# data - scenario - trial
data.3.1 <- generate.data.3()
data.3.2 <- generate.data.3()
data.3.3 <- generate.data.3()

data.3.1 %>% head() %>% knitr::kable()
data.3.2 %>% head() %>% knitr::kable()
data.3.3 %>% head() %>% knitr::kable()

data.3.1 %>% cor()
data.3.2 %>% cor()
data.3.3 %>% cor()

# Correlation plot
png("plots/synthetic_data_s3_t1_corr.png")
p <- data.3.1 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 3 - Trial 1")
p
dev.off()

png("plots/synthetic_data_s3_t2_corr.png")
p <- data.3.2 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 3 - Trial 2")
p
dev.off()

png("plots/synthetic_data_s3_t3_corr.png")
p <- data.3.3 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 3 - Trial 3")
p
dev.off()



# ----- Test Scenario 4 -----

# DAG plot
png("plots/synthetic_data_s4_DAG.png")
p <- draw.DAG.4()
p
dev.off()

# data - scenario - trial
data.4.1 <- generate.data.4()
data.4.2 <- generate.data.4()
data.4.3 <- generate.data.4()

data.4.1 %>% head() %>% knitr::kable()
data.4.2 %>% head() %>% knitr::kable()
data.4.3 %>% head() %>% knitr::kable()

data.4.1 %>% cor()
data.4.2 %>% cor()
data.4.3 %>% cor()

# Correlation plot
png("plots/synthetic_data_s4_t1_corr.png")
p <- data.4.1 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 4 - Trial 1")
p
dev.off()

png("plots/synthetic_data_s4_t2_corr.png")
p <- data.4.2 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 4 - Trial 2")
p
dev.off()

png("plots/synthetic_data_s4_t3_corr.png")
p <- data.4.3 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 4 - Trial 3")
p
dev.off()


