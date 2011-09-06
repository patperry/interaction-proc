# figure/static-coefs.R
# ---------------------

source("code/process.R")

nrow <- 9
ncol <- 10

fit.s <- fromJSON("output/fit-static.json")
fit <- fromJSON("output/fit-dynamic.json")

cov.s <- get.cov(fit.s)
cov <- get.cov(fit)

tx <- get.transform()
coef.s <- (get.coefs(fit.s) %*% tx)[1:nrow, 1:ncol]
coef <- (get.coefs(fit) %*% tx)[1:nrow,1:ncol]

vtx <- get.transform(TRUE)
cov.coef.s <- vtx %*% cov.s %*% t(vtx)
cov.coef <- vtx %*% cov[1:132, 1:132] %*% t(vtx)
se.coef.s <- matrix(sqrt(pmax(0, diag(cov.coef.s))), 11, 12)[1:nrow, 1:ncol]
se.coef <- matrix(sqrt(pmax(0, diag(cov.coef))), 11, 12)[1:nrow, 1:ncol]


rownames <- list("L", "T", "J", "F", "LJ", "TJ", "LF", "TF", "JF")
colnames <- c("1", rownames)

print.coefs <- function(coef, se, include.se = TRUE) {
    zstat <- abs(coef / se)
    coef <- formatC(coef, format="f", digits=2)
    se <- formatC(se, format="f", digits=2)

    cat("\\begin{tabular}{l", rep("r", ncol), "}\n", sep='')
    cat("\\toprule\n")
    cat("& \\multicolumn{", ncol, "}{c}{Receiver} \\\\\n", sep='')
    cat("\\cmidrule(l){2-", 1 + ncol, "} \n", sep='')
    cat("Sender", paste("& \\multicolumn{1}{c}{", colnames, "}", sep=''), "\\\\\n")
    cat("\\midrule\n")
    for (i in 1:nrow) {
        cat("\\multirow{2}{*}{", rownames[[i]], "}", sep='')
        for (j in 1:ncol) {
            cat(' &')
            if (i + 1 == j)
                cat("\\cellcolor{Gray}")
            if (zstat[i,j] < qnorm(1 - 0.5 * 1e-3)) {
                cat("\\textcolor{LightGray}{", coef[i,j], "}", sep='') 
            } else {
                cat(coef[i,j])
            }
        }
        cat("\\\\\n")
        if (include.se) {
            for (j in 1:ncol) {
                cat(' &')
                if (i + 1 == j)
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

