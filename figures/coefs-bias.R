# figures/coefs-bias.R
# --------------------

source("code/boot.R")


bias <- GetBias()
coefs1 <- coefs - bias$est
coefs1.se <- sqrt(bias$est.se^2 + coefs.se^2)
