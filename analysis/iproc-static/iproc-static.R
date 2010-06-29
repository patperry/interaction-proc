# iproc-static.R
# --------------

require("RSQLite")

kGender <- c("Female", "Male")
kSeniority <- c("Junior", "Senior")
kDepartment <- c("Legal", "Trading", "Other")
kGenderCount <- length(kGender)
kSeniorityCount <- length(kSeniority)
kDepartmentCount <- length(kDepartment)


kDbDriver <- dbDriver("SQLite")
kDbName <- "data/enron/enron.db"

kEmployeeCount<- (function() {
    sql <- paste("
        SELECT
            gender,
            seniority,
            department,
            COUNT(*) AS total
        FROM
            Employee
        GROUP BY
            gender,
            seniority,
            department
        ORDER BY
            department,
            seniority
    ")
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        rows <- dbGetQuery(conn, sql)
    }, finally = {
        dbDisconnect(conn)
    })
    
    # put department 'Other' at end
    rows <- rbind(rows[rows$department != "Other",],
                  rows[rows$department == "Other",])

    res <- rows$total
    names(res) <- paste(sapply(rows$gender, substr, 1, 1),
                        sapply(rows$seniority, substr, 1, 1),
                        sapply(rows$department, substr, 1, 1),
                        sep = "")
    
    res
})()


staticCount <- 144
varnames1 <- c("(Intercept)", "genF", "senJ", "depL", "depT",
	"genF:senJ", "genF:depL", "genF:depT", "senJ:depL", "senJ:depT",
	"genF:senJ:depL", "genF:senJ:depT")
varnames2 <- paste(rep(varnames1, sqrt(staticCount)),
                   rep(varnames1, each=sqrt(staticCount)), sep="->")

source("analysis/iproc-static/iproc.out.R")
penalty0 <- 1e-4
hess0 <- hess
coef0 <- coefs[seq_len(staticCount)]
coefMatrix0 <- matrix(coef0, sqrt(staticCount), sqrt(staticCount))
fisher0 <- hess0 + diag(penalty0, nrow(hess0))
invFisher0 <- solve(fisher0)
se0 <- sqrt(diag(invFisher0))
se0[seq_len(sqrt(staticCount))] <- NA
deviance0 <- deviance
names(coef0) <- names(se0) <- varnames2
dimnames(coefMatrix0) <- list(varnames1, varnames1)


source("analysis/iproc-dynamic/iproc.out.R")
penalty1 <- 1e-4
hess1 <- hess
coef1 <- coefs[seq_len(staticCount)]
coefMatrix1 <- matrix(coef1, sqrt(staticCount), sqrt(staticCount))
fisher1 <- hess1 + diag(penalty1, nrow(hess1))
invFisher1 <- solve(fisher1)[seq_len(staticCount), seq_len(staticCount)]
se1 <- sqrt(diag(invFisher1))
se1[seq_len(sqrt(staticCount))] <- NA
deviance1 <- deviance
names(coef1) <- names(se1) <- varnames2
dimnames(coefMatrix1) <- list(varnames1, varnames1)

rm(coefs, deviance, effects, hess, recv.intervals, send.intervals)





GetActorVars <- function(gen = kGender,
                         sen = kSeniority,
                         dep = kDepartment)
{
    gen <- lapply(gen, match.arg, kGender)
    sen <- lapply(sen, match.arg, kSeniority)
    dep <- lapply(dep, match.arg, kDepartment)
    
    genF <- "Female" %in% gen
    senJ <- "Junior" %in% sen
    depL <- "Legal" %in% dep
    depT <- "Trading" %in% dep
    
    vars <- c(1, genF, senJ, depL, depT, genF * senJ, genF * depL,
              genF * depT, senJ * depL, senJ * depT, genF * senJ * depL,
              genF * senJ * depT)
    names(vars) <- varnames1
    
    vars
}

GetProbTable <- function(coefMatrix, invFisher)
{
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
    R[,1] <- 0 # no receiver intercept
    etaMatrix.indiv <- S %*% coefMatrix %*% t(R)
    etaCov.indiv <- kronecker(R, S) %*% invFisher %*% t(kronecker(R, S))
    
    # get group-level log-intensity (adjust for self-loops)
    etaMatrix.gp <- (etaMatrix.indiv
                     + log(matrix(kEmployeeCount, byrow = TRUE,
                                  nrow(coefMatrix), ncol(coefMatrix))
                           - diag(1, nrow(coefMatrix))))
    etaCov.gp <- etaCov.indiv
    
    # get (relative) group-level intensity
    lambdaMatrix.gp.logscale <- apply(etaMatrix.gp, 1, max)
    lambdaMatrix.gp <- exp(etaMatrix.gp - lambdaMatrix.gp.logscale)
    lambda.gp <- as.vector(lambdaMatrix.gp)
    lambdaCov.gp <- diag(lambda.gp) %*% etaCov.gp %*% diag(lambda.gp)
    
    # get (relative) gender-level intensities
    female <- gender == "Female"
    male <- !female
    fm <- cbind(female, male)
    lambdaMatrix.fm <- lambdaMatrix.gp %*% fm
    lambda.fm <- as.vector(lambdaMatrix.fm)
    lambdaCov.fm <- (kronecker(t(fm), diag(1, nrow(lambdaMatrix.gp)))
                     %*% lambdaCov.gp
                     %*% t(kronecker(t(fm), diag(1, nrow(lambdaMatrix.gp)))))

    # get gender-level probabilities
    probMatrix.fm <- lambdaMatrix.fm / rowSums(lambdaMatrix.fm)
    grad <- (rbind(cbind(diag( probMatrix.fm[,2]), diag(-probMatrix.fm[,1])),
                   cbind(diag(-probMatrix.fm[,2]), diag( probMatrix.fm[,1])))
             / rowSums(lambdaMatrix.fm))
    probCov.fm <- grad %*% lambdaCov.fm %*% t(grad)

    # get (relative) gender-level log(intensities)
    etaMatrix.fm <- log(lambdaMatrix.fm)
    etaCov.fm <- diag(1/lambda.fm) %*% lambdaCov.fm %*% diag(1/lambda.fm)
    
    # get gender-level log(odds)
    logodds.fm <- etaMatrix.fm[,1] - etaMatrix.fm[,2]
    grad <- kronecker(cbind(1,-1), diag(1, length(logodds.fm)))
    logoddsCov.fm <- grad %*% etaCov.fm %*% t(grad)
    
    # get gender-level odds
    odds.fm <- exp(logodds.fm)
    oddsCov.fm <- diag(odds.fm) %*% logoddsCov.fm %*% diag(odds.fm)
    
    # get average gender-level log(odds)
    wt <- kEmployeeCount
    grad <- rbind(wt * female / sum(wt[female]),
                  wt * male / sum(wt[male]))
    avgLogodds.fm <- grad %*% logodds.fm
    avgLogoddsCov.fm <- grad %*% logoddsCov.fm %*% t(grad)

    # get averarge gender-level odds (Note: these values seem too high)
    grad <- rbind(wt * female / sum(wt[female]),
                  wt * male / sum(wt[male]))
    avgOdds.fm <- grad %*% odds.fm
    avgOddsCov.fm <- grad %*% oddsCov.fm %*% t(grad)
    
    # get average gender-level probabilities
    grad <- rbind(wt * female / sum(wt[female]),
                  wt * male / sum(wt[male]))
    avgProb.fm <- grad %*% probMatrix.fm
    grad <- kronecker(diag(2), grad)
    avgProbCov.fm <- grad %*% probCov.fm %*% t(grad)
    
    # get ratio of average gender-level probabilities 
    ratioAvgProb.fm <- avgProb.fm[,1] / avgProb.fm[,2]
    grad <- cbind(diag(1, 2), diag(-ratioAvgProb.fm)) / avgProb.fm[,2]
    ratioAvgProbCov.fm <- grad %*% avgProbCov.fm %*% t(grad)

    # get log(ratio of average gender-level probabilities)
    logratioAvgProb.fm <- log(ratioAvgProb.fm)
    logratioAvgProbCov.fm <- (diag(1/ratioAvgProb.fm)
                              %*% ratioAvgProbCov.fm
                              %*% (1/ratioAvgProb.fm))
    
    browser()
}

GetGenderTable <- function(coefMatrix)
{
    gender <- rep(kGender, kSeniorityCount * kDepartmentCount)

    probTable <- GetProbTable(coefMatrix)
    
    genderTable <- matrix(NA, kGenderCount, kGenderCount)
    for (j in seq_len(kGenderCount)) {
        for (i in seq_len(kGenderCount)) {
            genderTable[i,j] <-
                (rowSums(probTable[gender == gender[i], gender == gender[j]])
                 %*% (kEmployeeCount[gender == gender[i]]
                      / sum(kEmployeeCount[gender == gender[i]])))
        }
    }
    genderTable
    dimnames(genderTable) <- list(kGender, kGender)
    genderTable
}
