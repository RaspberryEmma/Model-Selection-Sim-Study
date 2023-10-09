# ****************************************
# Variable Selection Simulation Study
# 
# RStudio Demonstration!
# 
# Emma Tarmey
#
# Started:          09/10/2023
# Most Recent Edit: 09/10/2023
# ****************************************

# sanity check file location
getwd()

# pull from R file
source("../R/simulation.R")
#file.show("simulation.R")

# run simulation
run.simulation(S = 4,
               N = 1000,
               M = 5,
               p = 6,
               n = 10000,
               messages = FALSE)

source("interpret_bias_results.R")

all.results <- get.results.data()

bias.results.s1 <- all.results[[1]]
bias.results.s2 <- all.results[[2]]
bias.results.s3 <- all.results[[3]]
bias.results.s4 <- all.results[[4]]

coef.results.s1 <- all.results[[5]]
coef.results.s2 <- all.results[[6]]
coef.results.s3 <- all.results[[7]]
coef.results.s4 <- all.results[[8]]

all.means <- bias.tables(bias.results.s1, bias.results.s2, bias.results.s3, bias.results.s4,
                         coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4)

s1.bias.means <- all.means[[1]]
s2.bias.means <- all.means[[2]]
s3.bias.means <- all.means[[3]]
s4.bias.means <- all.means[[4]]

s1.bias.means %>% knitr::kable()
s2.bias.means %>% knitr::kable()
s3.bias.means %>% knitr::kable()
s4.bias.means %>% knitr::kable()


library("png")

plot.new()
pp <- readPNG("../plots/bias_s1.png")
rasterImage(pp, 0.00, 0.00, 1.00, 1.00)

plot.new()
pp <- readPNG("../plots/bias_s2.png")
rasterImage(pp, 0.00, 0.00, 1.00, 1.00)

plot.new()
pp <- readPNG("../plots/bias_s3.png") 
rasterImage(pp, 0.00, 0.00, 1.00, 1.00)

plot.new()
pp <- readPNG("../plots/bias_s4.png")
rasterImage(pp, 0.00, 0.00, 1.00, 1.00)


source("interpret_coef_results.R")

all.results <- get.results.data()

bias.results.s1 <- all.results[[1]]
bias.results.s2 <- all.results[[2]]
bias.results.s3 <- all.results[[3]]
bias.results.s4 <- all.results[[4]]

coef.results.s1 <- all.results[[5]]
coef.results.s2 <- all.results[[6]]
coef.results.s3 <- all.results[[7]]
coef.results.s4 <- all.results[[8]]

# TODO: generalise this across all technniques!
all.coef <- coef.tables(coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4)

lr.coef.summary.s1 <- all.coef[[1]]
lr.coef.summary.s2 <- all.coef[[2]]
lr.coef.summary.s3 <- all.coef[[3]]
lr.coef.summary.s4 <- all.coef[[4]]

lr.coef.summary.s1 %>% knitr::kable()
lr.coef.summary.s2 %>% knitr::kable()
lr.coef.summary.s3 %>% knitr::kable()
lr.coef.summary.s4 %>% knitr::kable()



bias.plots(s1.bias.means, s2.bias.means, s3.bias.means, s4.bias.means)

