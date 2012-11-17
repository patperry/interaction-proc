# tables/group.R
# --------------

source("code/utils.R")

nrecv <- 4
nsend <- 5

fit.s <- read.h5out("output/fit-static-main.h5")
fit <- read.h5out("output/fit-dynamic-main.h5")
load("output/boot-main.rda")

cov.s <- get.cov(fit.s)
cov <- get.cov(fit)

coef.s <- matrix(fit.s$coef[seq_len(nsend * nrecv)], nsend, nrecv, byrow=TRUE)
coef <- matrix(fit$coef[seq_len(nsend * nrecv)], nsend, nrecv, byrow=TRUE)

ix0 <- seq_len(nsend * nrecv)
cov.coef.s <- cov.s[ix0,ix0]
cov.coef <- cov[ix0, ix0]
se.coef.s <- matrix(sqrt(pmax(0, diag(cov.coef.s))), nsend, nrecv, byrow=TRUE)
se.coef <- matrix(sqrt(pmax(0, diag(cov.coef))), nsend, nrecv, byrow=TRUE)


recvnames <- list("L", "T", "J", "F")
sendnames <- c("1", recvnames)

print.coefs <- function(coef, se, include.se = TRUE) {
    zstat <- abs(coef / se)
    coef <- formatC(coef, format="f", digits=2)
    se <- formatC(se, format="f", digits=2)

    cat("\\begin{tabular}{l", rep("r", nrecv), "}\n", sep='')
    cat("\\toprule\n")
    cat("& \\multicolumn{", nrecv, "}{c}{Receiver} \\\\\n", sep='')
    cat("\\cmidrule(l){2-", 1 + nrecv, "} \n", sep='')
    cat("Sender", paste("& \\multicolumn{1}{c}{", recvnames, "}", sep=''), "\\\\\n")
    cat("\\midrule\n")
    for (i in 1:nsend) {
        cat("\\multirow{2}{*}{", sendnames[[i]], "}", sep='')
        for (j in 1:nrecv) {
            cat(' &')
            if (i == j + 1)
                cat("\\cellcolor{Gray}")
            if (zstat[i,j] < qnorm(1 - 0.5 * 1e-3)) {
                cat("\\textcolor{LightGray}{", coef[i,j], "}", sep='') 
            } else {
                cat(coef[i,j])
            }
        }
        cat("\\\\\n")
        if (include.se) {
            for (j in 1:nrecv) {
                cat(' &')
                if (i == j + 1)
                    cat("\\cellcolor{Gray}")
                if (zstat[i,j] < qnorm(1 - 0.5 * 1e-3)) {
                    cat("\\textcolor{LightGray}{\\tiny{(", se[i,j], ")}}", sep='') 
                } else {
                    cat("\\tiny{(", se[i,j], ")}", sep='')
                }
            }
            cat("\\\\[1ex]\n")
        }
    }
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
}

sink(file = "tables/group-static-main.tex")
print.coefs(coef.s, se.coef.s, TRUE)
sink()

sink(file = "tables/group-dynamic-main.tex")
print.coefs(coef - bias.mean[1:(nsend*nrecv)], se.coef, TRUE)
sink()



