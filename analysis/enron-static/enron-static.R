# enron-static.R
# --------------

require("RSQLite")
source("code/multinom.R")

kDbDriver <- dbDriver("SQLite")
kDbName <- "data/enron/enron.db"

kGender <- factor(c("Female", "Male"))
kSeniority <- factor(c("Junior", "Senior"))
kDepartment <- factor(c("Legal", "Trading", "Other"))

kGenderCount <- length(kGender)
kSeniorityCount <- length(kSeniority)
kDepartmentCount <- length(kDepartment)


EmployeeSet <- function () {
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        empCount <- dbGetQuery(conn, "
            SELECT
                MAX(eid) AS max_eid
            FROM 
                Employee")$max_mid

        rows <- dbGetQuery(conn, "
            SELECT
                eid,
                gender,
                seniority,
                department
            FROM
                Employee
            ORDER BY
                eid")
        
        emp <- data.frame(gender = factor(rows$gender, kGender),
                          seniority = factor(rows$seniority, kSeniority),
                          department = factor(rows$department, kDepartment))
        rownames(emp) <- rows$eid
        
    }, finally = {
        dbDisconnect(conn)
    })
    
    emp
}

MessageSet <- function() {
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        msgCount <- dbGetQuery(conn, "
            SELECT
                MAX(mid) AS max_mid
            FROM 
                Message")$max_mid
            
        rows <- dbGetQuery(conn, "
            SELECT
                mid,
                COUNT(*) AS len
            FROM
                Recipient
            GROUP BY
                mid")
                
        msgLength <- rep(0, msgCount)
        msgLength[rows$mid] <- rows$len
        msgOffset <- c(0, cumsum(msgLength)) + 1
        
        rows <- dbGetQuery(conn, "
            SELECT
                M.mid AS mid,
                M.unix_time AS unix_time,
                M.from_eid AS from_eid,
                R.to_eid AS to_eid
            FROM
                Message M,
                Recipient R
            WHERE
                R.mid = M.mid
            ORDER BY
                mid
            ")
        
        msgTime <- rows$unix_time
        msgFrom <- rows$from_eid
        msgTo <- rows$to_eid
        
    }, finally = {
        dbDisconnect(conn)
    })
    
    list(count = msgCount, countWithDupes = sum(msgLength),
         len = msgLength, offset = msgOffset, 
         time = msgTime, from = msgFrom, to = msgTo)
}


msg <- MessageSet()
hist(msg$len, breaks = seq_len(max(msg$len) + 1) - 0.5, prob = TRUE)






Model.static <- function(formula.from, formula.to,
                         data.from, data.to = data.from)
{
    # remove the intercept
    fromMatrix <- model.matrix(formula.from, data.from)[,-1]
    fromCount <- nrow(fromMatrix)
    fromVarCount <- ncol(fromMatrix)

    toMatrix <- model.matrix(formula.to, data.to)
    toCount <- nrow(toMatrix)
    toVarCount <- ncol(toMatrix)

    from <- rep(seq_len(fromCount), toCount)
    to <- rep(seq_len(toCount), each = fromCount)

    fromMatrix <- fromMatrix[from, rep(seq_len(fromVarCount), toVarCount)]
    toMatrix <- toMatrix[to, rep(seq_len(toVarCount), each = fromVarCount)]    
    modelMatrix <- fromMatrix * toMatrix
    rownames(modelMatrix) <- paste(from, to, sep="->")
    colnames(modelMatrix) <- paste(colnames(fromMatrix),colnames(toMatrix), sep="->")

    varCount <- ncol(modelMatrix)

    modelArray <- array(modelMatrix, c(fromCount, toCount, varCount))
    dimnames(modelArray) <- list(seq_len(fromCount), seq_len(toCount),
                                 colnames(modelMatrix))

    model <- list(fromCount = fromCount,
                  toCount = toCount,
                  varCount = varCount,
                  fromVarCount = fromVarCount,
                  toVarCount = toVarCount,
                  matrix = modelMatrix,
                  array = modelArray)
    model
}


Model.dynamic <- function(interval.send, interval.recv, message.set,
                          actor.count = max(message.set$from, message.set$to))
{
    sendInterval <- interval.send
    recvInterval <- interval.recv
    msg <- message.set
    fromCount <- toCount <- actor.count

    sendCount <- length(sendInterval)
    recvCount <- length(recvInterval)
    varCount <- sendCount + recvCount
    messageCount <- msg$count
    messageCountWithDupes <- msg$countWithDupes
    
    modelArray <- array(NA, c(messageCountWithDupes, toCount, varCount))

    tlast <- matrix(-Inf, fromCount, toCount)
    for (m in seq_len(messageCount)) {
        offset <- msg$offset[m]
        len <- msg$len[m]
        from <- msg$from[offset]

        if (len > 0) {
            time <- msg$time[offset]
            m.set <- seq.int(offset, length.out = len)
            
            for (s in seq_len(sendCount)) {
                modelArray[m.set,,s] <-
                    (time - tlast[from,] < sendInterval[s])
            }
            
            for (r in seq_len(recvCount)) {
                modelArray[m.set,,sendCount + r] <-
                    (time - tlast[,from] < recvInterval[r])
            }

            to.set <- msg$to[m.set]
            tlast[from,to.set] <- time
        }
    }
     
    modelMatrix <- matrix(as.vector(modelArray),
                          messageCountWithDupes * toCount,
                          varCount)
                          
    model <- list(fromCount = fromCount,
                  toCount = toCount,
                  varCount = varCount,
                  matrix = modelMatrix,
                  sendInterval = sendInterval,
                  recvInterval = recvInterval)
    model
}


NegLogLik <- function(coef, static, dynamic, msg,
                      self.loops = FALSE, derivatives = TRUE)
{
    fromCount <- static$fromCount
    toCount <- static$toCount
    coef.static <- coef[seq_len(static$varCount)]
    coef.dynamic <- coef[static$varCount + seq_len(dynamic$varCount)]

    fromMatrix <- matrix(0, msg$countWithDupes, fromCount)
    fromMatrix[seq_len(msg$countWithDupes)
               + msg$countWithDupes*(msg$from - 1)] <- 1
    toMatrix <- matrix(0, msg$countWithDupes, toCount)
    toMatrix[seq_len(msg$countWithDupes)
             + msg$countWithDupes*(msg$to - 1)] <- 1

    sendCountMatrix <- crossprod(fromMatrix, toMatrix)
    sendCount <- as.vector(sendCountMatrix)

    suff.static <- colSums((sendCount / sum(sendCount)) * static$matrix)
    suff.dynamic <- colMeans(dynamic$matrix)
    suff <- c(suff.static, suff.dynamic)


    etaMatrix.static <- matrix(static$matrix %*% coef.static,
                               fromCount, toCount)
    etaMatrix.dynamic <- matrix(dynamic$matrix %*% coef.dynamic,
                                ncol = toCount)
    etaMatrix <- etaMatrix.static[msg$from,] + etaMatrix.dynamic

    if (!self.loops) {
        loop <- (seq_len(msg$countWithDupes)
                 + (msg$from - 1) * msg$countWithDupes)
        etaMatrix[loop] <- -Inf
    }

    weightMatrix <- exp(etaMatrix)
    weightSum <- rowSums(weightMatrix)
    probMatrix <- weightMatrix / weightSum
    prob <- as.vector(probMatrix)

    obs <- seq_len(msg$countWithDupes) + (msg$to - 1) * msg$countWithDupes
    eta.obs <- etaMatrix[obs]
    prob.obs <- probMatrix[obs]
    resid <- log(weightSum) - eta.obs
    nll <- mean(resid)

    if (derivatives) {
        probByFromMatrix <- crossprod(fromMatrix, probMatrix)
        probByFrom <- as.vector(probByFromMatrix)
        wt <- prob / msg$countWithDupes

        meanByFrom.dynamic <- array(NA, c(fromCount, toCount, dynamic$varCount))
        for (j in seq_len(toCount)) {
            m.set <- seq.int(1 + (j - 1) * (msg$countWithDupes),
                             length.out = msg$countWithDupes)
            x <- wt[m.set] * dynamic$matrix[m.set,]
            meanByFrom.dynamic[,j,] <- crossprod(fromMatrix, x)
        }
        meanByFrom.dynamic <- matrix(meanByFrom.dynamic,
                                     ncol = dynamic$varCount)

        stats.static <- cov.wt(static$matrix,
                                wt = probByFrom,
                                method = "ML")
        mean.static <- stats.static$center
        cov.static <- stats.static$cov

        mean.dynamic <- colSums(meanByFrom.dynamic)
        grad <- suff - c(mean.static, mean.dynamic)

        wt.sqrt <- sqrt(wt)
        cov.dynamic <- matrix(0, dynamic$varCount, dynamic$varCount)
        for (j in seq_len(toCount)) {
            m.set <- seq.int(1 + (j - 1) * (msg$countWithDupes),
                             length.out = msg$countWithDupes)
            x <- (wt.sqrt[m.set]
                  * sweep(dynamic$matrix[m.set,], 2, mean.dynamic))
            cov.dynamic <- cov.dynamic + crossprod(x)
        }

        # \sum_{ij} p_{ij} s_{ij} d_{ij}^T
        # = \sum_j \sum_f \sum_{i : from(i) = f} p_{ij} s_{fj} d_{ij}^T
        # = \sum_j \sum_f s_fj \sum_{i : from(i) = f} p_{ij} d_{ij}^T
        #
        # \sum_{ij} p_{ij} d_{ij}
        # = \sum_j \sum_f \sum_{i : from(i) = f} p_{ij} d_{ij}^T
        cov.cross <- crossprod(sweep(static$matrix, 2, mean.static),
                               meanByFrom.dynamic)

        fisher <- rbind(cbind(cov.static,     cov.cross),
                        cbind(t(cov.cross), cov.dynamic))
    }
    
    res <- list(nobs = msg$countWithDupes, value = nll)
    if (derivatives) {
        res$grad <- grad
        res$fisher <- fisher
    }
        
    res
}




GetStep <- function(coef, static, dynamic, msg, self.loops = FALSE,
                    penalty = rep(1e-5, length(coef)),
                    alpha = 0.25, beta = 0.5)
{
    nll <- NegLogLik(coef, static, dynamic, msg, self.loops = self.loops)
    penalty <- penalty / nll$nobs
    
    nll0 <- nll$value
    grad <- nll$grad
    fisher <- nll$fisher
    
    f0 <- nll0 + 0.5 * sum(penalty * coef^2)
    grad <- grad + penalty * coef
    hess <- fisher
    diag(hess) <- diag(hess) + penalty
    chol.hess <- chol(hess)
    search <- backsolve(chol.hess,
                        backsolve(chol.hess, grad, transpose = TRUE))
    dec <- as.numeric(grad %*% search)


    t <- 1
    done <- FALSE
    while (!done && t > 5e-2) {
        coef1 <- coef + t * search
        
        if (max(abs(t * search)) < 2) {
            nll1 <- NegLogLik(coef1, static, dynamic, msg,
                              self.loops = self.loops,
                              derivatives = FALSE)$value
            f1 <- nll1 + 0.5 * sum(penalty * coef1^2)

            if (f1 <= f0 + alpha * t * dec)
                done <- TRUE
        }
        
        if (!done)
            t <- t * beta            
    }

    step <- list(coef = coef1, search = search, size = t,
                 decrement = dec, nloglik = nll1, nloglik.pen = f1)    
    step
}

Fit <- function(static, dynamic, msg, self.loops = FALSE,
                start = NULL,
                penalty = rep(1, static$varCount + dynamic$varCount),
                control = glm.control(),
                alpha = 0.25, beta = 0.5)
{
    if (is.null(start) || is.na(start)) { 
        start <- rep(0, static$varCount + dynamic$varCount)
    }

    epsilon <- control$epsilon
    maxit <- control$maxit
    trace <- control$trace
    trace <- TRUE

    it <- 0
    converged <- FALSE
    coef0 <- start
    
    while (!converged && it < maxit) {
            it <- it + 1
            step <- GetStep(coef0, static, dynamic, msg, self.loops,
                            penalty, alpha, beta)
            coef1 <- step$coef

            if (trace) {
                cat("it:", it, " nloglik:", step$nloglik,
                    " nloglik.pen:", step$nloglik.pen,
                    " step size:", step$size,
                    " decrement:", step$dec, "\n")
            }

            if (step$dec < epsilon * (msg$countWithDupes))
                converged <- TRUE

            coef0 <- coef1
        }
    
    if (!converged)
        warning("failed to converged after ", it, " iterations")
    
    coef0
}

emp <- EmployeeSet()
msg <- MessageSet()
formula <- ~ .
static <- Model.static(formula, formula, emp)
dynamic <- Model.dynamic(interval.send = 3600 * 2^seq(-6, 10),
                         interval.recv = 3600 * 2^seq(-6, 10),
                         msg)
self.loops <- FALSE

coef.static <- rnorm(static$varCount)
coef.dynamic <- rnorm(dynamic$varCount)
coef <- c(coef.static, coef.dynamic)


fit <- Fit(static, dynamic, msg, self.loops)
