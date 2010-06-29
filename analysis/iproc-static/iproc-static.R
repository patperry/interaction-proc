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

GetProbTable <- function(coefMatrix, weight = FALSE)
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
    for (j in seq_len(ncol(coefMatrix))) {
        to <- GetActorVars(gender[j], seniority[j], department[j])
        for (i in seq_len(nrow(coefMatrix))) {
            from <- GetActorVars(gender[i], seniority[i], department[i])
            logwt[i,j] <- t(from) %*% coefMatrix %*% to
        }
    }
    
    wt <- exp(logwt)
    for (i in seq_len(nrow(wt))) {
        n <- kEmployeeCount
        n[i] <- n[i] - 1 # adjustment for self-loops
        wt[i,] <- wt[i,] * n
    }
    
    prob <- wt / apply(wt, 1, sum)
    
    if (weight) wt else prob                   
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
