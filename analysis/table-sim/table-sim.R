# table-sim.R
# -----------

source("code/multinom.R")


set.seed(0, "Mersenne-Twister")

recipCount <- 100
coefCount <- 5

coef <- rnorm(coefCount)
varset <- matrix(rnorm(recipCount * coefCount), recipCount)
multinom <- Multinom(coef, varset)

msgCount <- recipCount^2
msgLength <- pmin(1 + rgeom(msgCount, 0.7), recipCount)

strategy <- "stepwise"
sample <- SampleMultinom(msgCount, multinom, msgLength, strategy = strategy)
msg <- MessageSet(sample, varset)
boot <- BootFitMultinom(msg, varset, strategy = strategy)
