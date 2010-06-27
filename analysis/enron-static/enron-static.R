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


emp <- model.matrix( ~ ., EmployeeSet())
empCount <- nrow(emp)

from <- rep(seq_len(empCount), times = empCount)
fromVars <- emp[from,]
colnames(fromVars) <- lapply(colnames(emp), function(x)
    paste("from", x, sep="."))
    
to <- rep(seq_len(empCount), each = empCount)    
toVars <- emp[to,]
colnames(toVars) <- lapply(colnames(emp), function(x)
    paste("to", x, sep="."))

pairVars <- as.data.frame(cbind(fromVars, toVars))



varCount <- ncol(pairVars)



sendInterval <- 3600 * c(1, 2, 4, 8)
sendIntervalCount <- length(sendInterval)
recvInterval <- 3600 * c(1, 2, 4, 8)
recvIntervalCount <- length(recvInterval)

beta.stat <- rnorm(varCount)
beta.dyn <- rnorm(sendIntervalCount + recvIntervalCount)

mvars.stat <- as.matrix(pairVars)


mvars.dyn <- matrix(NA, msg$countWithDupes * empCount,
                    sendIntervalCount + recvIntervalCount)
tlast <- matrix(-Inf, empCount, empCount)
for (m in seq_len(msg$count)) {
    offset <- msg$offset[m]
    len <- msg$len[m]
    from <- msg$from[offset]
    to <- msg$to[offset]
    time <- msg$time[offset]

    rows <- seq.int(1 + empCount * (offset - 1),
                    length.out = empCount * len)
    for (i in seq_len(sendIntervalCount)) {    
        mvars.dyn[rows, i] <-
            time - tlast[from,] < sendInterval[i]
    }
    for (i in seq_len(recvIntervalCount)) {
        mvars.dyn[rows, sendIntervalCount + i] <-
            time - tlast[,from] < recvInterval[i]
    }
        
    tlast[from,to] <- time
}

eta.stat.pairs <- matrix(mvars.stat %*% beta.stat, empCount, empCount)
eta.stat <- eta.stat.pairs[msg$from,]

eta.dyn <- matrix(mvars.dyn %*% beta.dyn, ncol = empCount, byrow = TRUE)

eta <- eta.stat + eta.dyn

loop <- seq_len(msg$countWithDupes) + (msg$from - 1) * msg$countWithDupes
eta[loop] <- -Inf

weight <- exp(eta)
weightSum <- weightSum <- rowSums(weight)
prob <- weight / weightSum


obs <- seq_len(msg$countWithDupes) + (msg$to - 1) * msg$countWithDupes
eta.obs <- eta[obs]
prob.obs <- prob[obs]
resid <- log(weightSum) - eta.obs

prob.stat <- matrix(0, empCount, empCount)
wt.obs <- prob.obs / msg$countWithDupes

mvars.dyn.array <- array(mvars.dyn,
                         c(empCount, msg$countWithDupes, ncol(mvars.dyn)))
mean.byfrom.dyn.array <- array(0, c(empCount, empCount, ncol(mvars.dyn)))
for (m in seq_len(msg$countWithDupes)) {
    from <- msg$from[m]
    to <- msg$to[m]
    prob.stat[from,to] <- prob.stat[from,to] + prob.obs[m]
    
    mean.byfrom.dyn.array[from,,] <- (mean.from.dyn.array[from,,]
                                      + prob[m,] * mvars.dyn.array[,m,])
}
mean.byfrom.dyn <- matrix(mean.byfrom.dyn.array, ncol = ncol(mvars.dyn))


stats.stat <- cov.wt(mvars.stat,
                     wt = as.vector(prob.stat),
                     method = "ML")
mean.stat <- stats.stat$center
cov.stat <- stats.stat$cov

stats.dyn <- cov.wt(mvars.dyn,
                    wt = as.vector(t(prob)),
                    method = "ML")
mean.dyn <- stats.dyn$center
cov.dyn <- stats.dyn$cov

# \sum_{ij} p_{ij} s_{ij} d_{ij}^T
# = \sum_j \sum_f \sum_{i : from(i) = f} p_{ij} s_{fj} d_{ij}^T
# = \sum_j \sum_f s_fj \sum_{i : from(i) = f} p_{ij} d_{ij}^T
#
# \sum_{ij} p_{ij} d_{ij}
# = \sum_j \sum_f \sum_{i : from(i) = f} p_{ij} d_{ij}^T

cov.cross <- crossprod(sweep(mvars.stat, 2, mean.stat),
                       mean.byfrom.dyn)

fisher <- rbind(cbind(cov.stat,     cov.cross),
                cbind(t(cov.cross), cov.dyn))


batchSizeDefault <- 512
batchOffset <- (c(seq.int(0, msg$countWithDupes, batchSizeDefault),
                  msg$countWithDupes) + 1)
batchSize <- diff(batchOffset)
batchCount <- length(batchSize)



resid <- rep(NA, msg$countWithDupes)
n <- 0
mean1 <- rep(0, varCount)
mean2 <- matrix(0, varCount, varCount)
prob <- matrix(NA, empCount, msg$countWithDupes)



for (b in seq_len(batchCount)) {
    batch <- seq.int(from = batchOffset[b], length.out = batchSize[b])
    
    cat(".")
    if (batchSize[b] > 0) {
        n.new <- batchSize[b]

        mvars.obs <- as.matrix(pairVars[msg$from[batch]
                               + empCount * (msg$to[batch] - 1),])
        eta <- mvars.obs %*% beta


        mvars <- as.matrix(pairVars[rep(msg$from[batch], each = empCount)
                           + empCount * (seq_len(empCount) - 1),])

        # Un-normalized recv probability; exclude self-messages
        weight <- matrix(exp(mvars %*% beta), nrow = empCount)
        weight[msg$from[batch] + empCount * (seq_len(n.new) - 1)] <- 0
        weightSum <- colSums(weight)

        resid[batch] <- log(weightSum) - eta

        prob.new <- weight / weightSum
        prob[,batch] <- prob.new
        prob.new <- as.vector(prob.new)
        
        mvars.wt <- prob.new * mvars
        mean1.new <- colMeans(mvars.wt);
        
        x1 <- as.vector(mvars.wt)
        x2 <- matrix(rep(t(mvars), each = varCount),
                     nrow = n.new * empCount,
                     ncol = varCount * varCount,
                     byrow = TRUE)
        mvars2.wt <- x1 * x2
        mean2.new <- colMeans(mvars2.wt)
        
        n <- n + n.new
        mean1 <- mean1 + (mean1.new - mean1) * (n.new / n)
        mean2 <- mean2 + (mean2.new - mean2) * (n.new / n)
    }
}
cat("\n")

prob <- t(prob)
