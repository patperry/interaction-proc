# tables/group.R
# --------------

source("code/process.R")

nrecv <- 9
nsend <- 10

fit.s <- fromJSON("output/fit-static.json")
fit <- fromJSON("output/fit-dynamic.json")

cov.s <- get.cov(fit.s)
cov <- get.cov(fit)

tx <- get.transform()
coef.s <- (get.coefs(fit.s) %*% tx)[1:nrecv, 1:nsend]
coef <- (get.coefs(fit) %*% tx)[1:nrecv,1:nsend]

vtx <- get.transform(TRUE)
ix0 <- rep(1:11, 12) + rep((1:12 - 1) * nrow(coef), each=11)
cov.coef.s <- vtx %*% cov.s %*% t(vtx)
cov.coef <- vtx %*% cov[ix0, ix0] %*% t(vtx)
se.coef.s <- matrix(sqrt(pmax(0, diag(cov.coef.s))), 11, 12)[1:nrecv, 1:nsend]
se.coef <- matrix(sqrt(pmax(0, diag(cov.coef))), 11, 12)[1:nrecv, 1:nsend]


recvnames <- list("L", "T", "J", "F", "LJ", "TJ", "LF", "TF", "JF")
sendnames <- c("1", recvnames)

print.coefs <- function(coef, se, include.se = TRUE) {
    coef <- t(coef)
    se <- t(se)
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

sink(file = "tables/group-static.tex")
print.coefs(coef.s, se.coef.s, TRUE)
sink()

sink(file = "tables/group-dynamic.tex")
print.coefs(coef, se.coef, TRUE)
sink()



