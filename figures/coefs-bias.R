# figures/coefs-bias.R
# --------------------

source("code/boot.R")


sys.source("analysis/dynamic.R",
    dynamic <- new.env(parent = baseenv()))

boot <- GetBoot()
nrep <- nrow(boot$coefs)

intervals.send <- dynamic$intervals.send
intervals.recv <- dynamic$intervals.recv
coefs <- dynamic$coefs
coefs.se <- dynamic$coefs.se

bias <- colMeans(boot$coefs) - coefs
bias.se <- sqrt(apply(boot$coefs, 2, var) * ((nrep-1)/nrep)
                / nrep)

coefs1 <- coefs - bias
coefs1.se <- sqrt(bias.se^2 + coefs.se^2) / coefs.se