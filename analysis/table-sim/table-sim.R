# table-sim.R
# -----------



Multinom <- function(coef, varset)
{
    count <- nrow(varset)
    coefCount <- ncol(varset)

    weight <- exp(as.numeric(varset %*% coef))
    prob <- weight / sum(weight)
    stats <- cov.wt(varset, wt = prob, method = "ML")
    mean <- stats$center
    fisher  <- stats$cov
    chol.fisher <- chol(fisher)
    multinom <- list(count = count, coefCount = coefCount, coef = coef,
        varset = varset, weight = weight, prob = prob, mean = mean,
        fisher = fisher, chol.fisher = chol.fisher)
    multinom
}

GetProb <- function(coef, varset)
{
    weight <- exp(varset %*% coef)
    prob <- weight / sum(weight)    
    prob
}

NegLogLik <- function(multinom, msg)
{
    varset <- multinom$varset
    coef <- multinom$coef
    weight <- multinom$weight
    resid <- (log(sum(weight)) - (varset %*% coef)[msg$recip])
    
    mean(resid)
}

FitMultinom <- function(msg, varset, start = NULL, control = glm.control())
{
    if (is.null(start) || is.na(start)) {
        coef0 <- rep(0, ncol(varset))
        start <- Multinom(coef0, varset)
    }

    epsilon <- control$epsilon
    maxit <- control$maxit
    trace <- control$trace

    it <- 0
    converged <- FALSE
    
    multinom0 <- start    
    while (!converged && it < maxit) {
        it <- it + 1
        step <- GetStep(multinom0, msg)
        multinom <- step$multinom

        if (trace) {
            cat("it:", it, " nloglik:", step$nloglik,
                " decrement:", step$dec, "\n")
        }

        if (step$dec < epsilon)
            converged <- TRUE
            
        multinom0 <- multinom
    }
    
    if (!converged)
        warning("failed to converged after ", it, " iterations")
    
    multinom
}


GetStep <- function(multinom, msg, alpha = 0.25, beta = 0.5)
{
    nll <- NegLogLik(multinom, msg)
    grad <- msg$summary$varset - multinom$mean
    chol.fisher <- multinom$chol.fisher
    search <- backsolve(chol.fisher,
                        backsolve(chol.fisher, grad, transpose = TRUE))
    dec <- grad %*% search
    
    t <- 1        
    while (t > 1e-10) {
        coef1 <- multinom$coef + t * search
        
        if (max(abs(t * search)) < 5) {
            multinom1 <- Multinom(coef1, multinom$varset)
            nll1 <- NegLogLik(multinom1, msg)

            if (nll1 <= nll + alpha * t * dec)
                break
        }
        
        t <- t * beta            
    }
    
    step <- list(multinom = multinom1, search = search, size = 1,
                 decrement = dec, nloglik = nll1)    
    step
}

SampleMultinom <- function(count, multinom, len = rep(1, count),
                           strategy = c("stepwise", "together", "replace"))
{
    strategy <- match.arg(strategy)
        
    prob <- multinom$prob
    recipCount <- multinom$count

    offset <- c(0, cumsum(len)) + 1
    recip <- sample.int(recipCount, offset[count], replace = TRUE,
                        prob = prob)
    
    if (strategy == "stepwise") {
        for (m in which(len > 1)) {
            l <- len[m]
            b <- offset[m]
            e <- b+l-1
            recip[b:e] <- sample.int(recipCount, l, replace = FALSE,
                                     prob = prob)
        }
    } else if (strategy == "together") {
        for (m in which(len > 1)) {
            l <- len[m]
            b <- offset[m]
            e <- b+l-1
            R <- recip[b:e]
            if (any(duplicated(R))) {
                while (any(duplicated(R))) {
                    R <- sample.int(recipCount, l, replace = TRUE,
                                    prob = prob)
                }
                recip[b:e] <- R
            }

        }
    }
    
    sample <- list(count = count, offset = offset, length = len,
                   recip = recip)
    sample
}

SummarySample <- function(sample, varset)
{
    list(recip = tabulate(sample$recip, nrow(varset)),
         varset = apply(varset[sample$recip,], 2, mean))
}

MessageSet <- function(sample, varset)
{
    msg <- list(count = sample$count, length = sample$length,
                offset = sample$offset, recip = sample$recip,
                summary = SummarySample(sample, varset))
    msg
}

BootFitMultinom <- function(msg, varset, reps = 500,
                            strategy = c("stepwise", "together", "replace"))
{
    multinom0 <- FitMultinom(msg, varset)
    coef0 <- multinom0$coef
    chol.fisher0 <- multinom0$chol.fisher
    count <- msg$count    
    coefCount <- length(coef0)
    
    coef <- matrix(NA, reps, coefCount)
    score <- matrix(NA, reps, coefCount)
    for (r in seq_len(reps)) {
        sample1 <- SampleMultinom(msg$count, multinom0, msg$length,
                                  strategy = strategy)
        msg1 <- MessageSet(sample1, varset)
        multinom1 <- FitMultinom(msg1, varset, start = multinom0)
        coef1 <- multinom1$coef
        chol.fisher1 <- multinom1$chol.fisher
        score1 <- sqrt(count) * (chol.fisher1 %*% (coef1 - coef0))
        coef[r,] <- coef1
        score[r,] <- score1
    }
        
    bias0 <- rep(0, coefCount)
    bias1 <- apply(coef, 2, mean) - coef0
    bias2 <- backsolve(chol.fisher0,
                 apply(score, 2, mean) / sqrt(count))
    
    cov0 <- backsolve(chol.fisher0,
                backsolve(chol.fisher0, transpose = TRUE,
                    diag(1 / count, coefCount)))
    cov1 <- cov(coef)
    cov2 <- backsolve(chol.fisher0,
                t(backsolve(chol.fisher0,
                    cov(score) / count)))
    
    list(coef0 = coef0, coef = coef, score = score,
         bias0 = bias0, bias1 = bias1, bias2 = bias2,
         cov0 = cov0, cov1 = cov1, cov2 = cov2)
}

set.seed(0, "Mersenne-Twister")

recipCount <- 100
coefCount <- 5

coef <- rnorm(coefCount)
varset <- matrix(rnorm(recipCount * coefCount), recipCount)
multinom <- Multinom(coef, varset)

msgCount <- recipCount^2
msgLength <- pmin(1 + rgeom(msgCount, 0.7), recipCount)

strategy <- "stepwise"
sample <- SampleMultinom(msgCount, multinom, msgLength, strategy = strategy)
msg <- MessageSet(sample, varset)
boot <- BootFitMultinom(msg, varset, strategy = strategy)

