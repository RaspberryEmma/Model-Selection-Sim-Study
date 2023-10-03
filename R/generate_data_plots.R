# ****************************************
# Variable Selection Simulation Study
# 
# Explore Models under Study
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
source("plot_rescale.R")

# ----- Test Scenario 1 -----

# open png file
png("../plots/synthetic_data_s1_DAG.png")

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
png("../plots/synthetic_data_s1_t1_corr.png")
p <- data.1.1 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 1 - Trial 1")
p
dev.off()

png("../plots/synthetic_data_s1_t2_corr.png")
p <- data.1.2 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 1 - Trial 2")
p
dev.off()

png("../plots/synthetic_data_s1_t3_corr.png")
p <- data.1.3 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 1 - Trial 3")
p
dev.off()



# ----- Test Scenario 2 -----

# DAG plot
png("../plots/synthetic_data_s2_DAG.png")
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
png("../plots/synthetic_data_s2_t1_corr.png")
p <- data.2.1 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 2 - Trial 1")
p
dev.off()

png("../plots/synthetic_data_s2_t2_corr.png")
p <- data.2.2 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 2 - Trial 2")
p
dev.off()

png("../plots/synthetic_data_s2_t3_corr.png")
p <- data.2.3 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 2 - Trial 3")
p
dev.off()



# ----- Test Scenario 3 -----

# DAG plot
png("../plots/synthetic_data_s3_DAG.png")
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
png("../plots/synthetic_data_s3_t1_corr.png")
p <- data.3.1 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 3 - Trial 1")
p
dev.off()

png("../plots/synthetic_data_s3_t2_corr.png")
p <- data.3.2 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 3 - Trial 2")
p
dev.off()

png("../plots/synthetic_data_s3_t3_corr.png")
p <- data.3.3 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 3 - Trial 3")
p
dev.off()



# ----- Test Scenario 4 -----

# DAG plot
png("../plots/synthetic_data_s4_DAG.png")
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
png("../plots/synthetic_data_s4_t1_corr.png")
p <- data.4.1 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 4 - Trial 1")
p
dev.off()

png("../plots/synthetic_data_s4_t2_corr.png")
p <- data.4.2 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 4 - Trial 2")
p
dev.off()

png("../plots/synthetic_data_s4_t3_corr.png")
p <- data.4.3 %>% cor() %>% ggcorrplot::ggcorrplot() +
  ggtitle("Synthetic Data Correlation Plot - Scenario 4 - Trial 3")
p
dev.off()


