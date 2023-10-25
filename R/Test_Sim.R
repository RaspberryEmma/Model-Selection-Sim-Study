# ****************************************
# Variable Selection Simulation Study
# 
# RStudio Testing Environment!
# 
# Emma Tarmey
#
# Started:          25/10/2023
# Most Recent Edit: 25/10/2023
# ****************************************


# sanity check file location
getwd()


# pull from R file
source("../R/simulation.R")
#file.show("simulation.R")


# test missing data generation mechanism
source("../R/generate_data.R")

generate.data.1(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "none", prop.missing = 0.0) %>% head() %>% knitr::kable()
generate.data.1(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MCAR", prop.missing = 0.2) %>% head() %>% knitr::kable()
generate.data.1(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MAR",  prop.missing = 0.2) %>% head() %>% knitr::kable()
generate.data.1(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MNAR", prop.missing = 0.2) %>% head() %>% knitr::kable()

generate.data.2(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "none", prop.missing = 0.0) %>% head() %>% knitr::kable()
generate.data.2(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MCAR", prop.missing = 0.2) %>% head() %>% knitr::kable()
generate.data.2(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MAR",  prop.missing = 0.2) %>% head() %>% knitr::kable()
generate.data.2(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MNAR", prop.missing = 0.2) %>% head() %>% knitr::kable()

generate.data.3(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "none", prop.missing = 0.0) %>% head() %>% knitr::kable()
generate.data.3(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MCAR", prop.missing = 0.2) %>% head() %>% knitr::kable()
generate.data.3(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MAR",  prop.missing = 0.2) %>% head() %>% knitr::kable()
generate.data.3(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MNAR", prop.missing = 0.2) %>% head() %>% knitr::kable()

generate.data.4(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "none", prop.missing = 0.0) %>% head() %>% knitr::kable()
generate.data.4(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MCAR", prop.missing = 0.2) %>% head() %>% knitr::kable()
generate.data.4(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MAR",  prop.missing = 0.2) %>% head() %>% knitr::kable()
generate.data.4(sample.size = 1000, conf.sd = c(2.5, 2.5), mech.missing = "MNAR", prop.missing = 0.2) %>% head() %>% knitr::kable()
