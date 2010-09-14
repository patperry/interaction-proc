# coef-table.R
# ------------

source("analysis/iproc-dynamic/iproc.out.R")
staticCount <- 144
penalty <- 1e-4
hess <- hess
coef <- coefs[seq_len(staticCount)]
coefMatrix <- matrix(coef, sqrt(staticCount), sqrt(staticCount))

gender <- rep(kGender, kSeniorityCount * kDepartmentCount)
seniority <- rep(rep(kSeniority, each = kGenderCount), kDepartmentCount)
department <- rep(kDepartment, each = kGenderCount * kSeniorityCount)

varnames1 <- paste(sapply(gender, substr, start = 1, stop = 1),
                   sapply(seniority, substr, start = 1, stop = 1),
                   sapply(department, substr, start = 1, stop = 1),
                   sep="")
varnames2 <- list(varnames1, varnames1)

logwt <- matrix(NA, nrow(coefMatrix), ncol(coefMatrix))
dimnames(logwt) <- varnames2

# get relative individual-level log-intensity
R <- matrix(NA, nrow(coefMatrix), ncol(coefMatrix))
for (j in seq_len(ncol(coefMatrix))) {
    R[,j] <- GetActorVars(gender[j], seniority[j], department[j])
}
S <- R
R[1,] <- 0 # no receiver intercept
etaMatrix <- t(S) %*% coefMatrix %*% R
lambdaMatrix <- exp(etaMatrix)

probMatrix <- lambdaMatrix / rowSums(lambdaMatrix)
dimnames(probMatrix) <- varnames2

