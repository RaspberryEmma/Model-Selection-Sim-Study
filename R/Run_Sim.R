# ****************************************
# Variable Selection Simulation Study
# 
# RStudio Demonstration!
# 
# Emma Tarmey
#
# Started:          09/10/2023
# Most Recent Edit: 25/10/2023
# ****************************************

# sanity check file location
getwd()

# pull from R file
source("../R/simulation.R")
#file.show("simulation.R")

# run simulation
# S = number of scenarios
# N = repetitions for this scenario
# M = number of VS techniques under investigation 
# p = number of variables in data (includes id, excludes intercept and outcome y)
# n = synthetic data-set size

run.simulation(S = 4,
               N = 1000,
               M = 6,
               p = 6,
               n = 10000,
               mech.missing = "MCAR",
               prop.missing = 0.2,
               messages     = FALSE)

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

bias.plots(s1.bias.means, s2.bias.means, s3.bias.means, s4.bias.means)


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

lr.coef <- coef.tables(method = "linear", coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4)

lr.coef.summary.s1 <- lr.coef[[1]]
lr.coef.summary.s2 <- lr.coef[[2]]
lr.coef.summary.s3 <- lr.coef[[3]]
lr.coef.summary.s4 <- lr.coef[[4]]

lr.coef.summary.s1 %>% knitr::kable()
lr.coef.summary.s2 %>% knitr::kable()
lr.coef.summary.s3 %>% knitr::kable()
lr.coef.summary.s4 %>% knitr::kable()


lasso.coef <- coef.tables(method = "lasso", coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4)

lasso.coef.summary.s1 <- lasso.coef[[1]]
lasso.coef.summary.s2 <- lasso.coef[[2]]
lasso.coef.summary.s3 <- lasso.coef[[3]]
lasso.coef.summary.s4 <- lasso.coef[[4]]

lasso.coef.summary.s1 %>% knitr::kable()
lasso.coef.summary.s2 %>% knitr::kable()
lasso.coef.summary.s3 %>% knitr::kable()
lasso.coef.summary.s4 %>% knitr::kable()


ridge.coef <- coef.tables(method = "ridge", coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4)

ridge.coef.summary.s1 <- ridge.coef[[1]]
ridge.coef.summary.s2 <- ridge.coef[[2]]
ridge.coef.summary.s3 <- ridge.coef[[3]]
ridge.coef.summary.s4 <- ridge.coef[[4]]

ridge.coef.summary.s1 %>% knitr::kable()
ridge.coef.summary.s2 %>% knitr::kable()
ridge.coef.summary.s3 %>% knitr::kable()
ridge.coef.summary.s4 %>% knitr::kable()


scad.coef <- coef.tables(method = "scad", coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4)

scad.coef.summary.s1 <- scad.coef[[1]]
scad.coef.summary.s2 <- scad.coef[[2]]
scad.coef.summary.s3 <- scad.coef[[3]]
scad.coef.summary.s4 <- scad.coef[[4]]

scad.coef.summary.s1 %>% knitr::kable()
scad.coef.summary.s2 %>% knitr::kable()
scad.coef.summary.s3 %>% knitr::kable()
scad.coef.summary.s4 %>% knitr::kable()


mcp.coef <- coef.tables(method = "mcp", coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4)

mcp.coef.summary.s1 <- mcp.coef[[1]]
mcp.coef.summary.s2 <- mcp.coef[[2]]
mcp.coef.summary.s3 <- mcp.coef[[3]]
mcp.coef.summary.s4 <- mcp.coef[[4]]

mcp.coef.summary.s1 %>% knitr::kable()
mcp.coef.summary.s2 %>% knitr::kable()
mcp.coef.summary.s3 %>% knitr::kable()
mcp.coef.summary.s4 %>% knitr::kable()


step.coef <- coef.tables(method = "stepwise", coef.results.s1, coef.results.s2, coef.results.s3, coef.results.s4)

step.coef.summary.s1 <- step.coef[[1]]
step.coef.summary.s2 <- step.coef[[2]]
step.coef.summary.s3 <- step.coef[[3]]
step.coef.summary.s4 <- step.coef[[4]]

step.coef.summary.s1 %>% knitr::kable()
step.coef.summary.s2 %>% knitr::kable()
step.coef.summary.s3 %>% knitr::kable()
step.coef.summary.s4 %>% knitr::kable()

