# multicast.R
#

sample.set <- function(n, size, prob=NULL, maxit=1e5, warn=TRUE) {
	i <- c()
	done <- FALSE
	it <- 0
	while (!done && it < maxit) {
		it <- it + 1
		i <- sample.int(n, size, replace=TRUE, prob)
        if (size == 1) {
            done <- TRUE
        } else {
    		i <- unique(i)
    		done <- length(i) == size
        }
	}


	if (it == maxit) {
		if (warn)
			warning("Failed to sample after ", maxit, " tries.")
		return(NA)
	}

	i
}


bias.sim1 <- function(n, x, beta, size) {
    eta <- x %*% beta
    eta1 <- eta - max(eta)
    w <- exp(eta1)
    prob <- w / sum(w)
    nrecv <- length(prob)

    y <- matrix(0, n, nrecv)
    for (i in seq_len(n)) {
        j <- sample.set(nrecv, size[i], prob)
        y[i,j] <- 1
    }

    fit <- glm(colSums(y) ~ x - 1, family=poisson)
    beta.est <- fit$coef
    beta.est
}

bias.sim <- function(n, x, beta, size, nreps) {
	beta.est <- matrix(NA, nreps, length(beta))
	for (r in seq_len(nreps)) {
		beta.est[r,] <- bias.sim1(n, x, beta, size)
		cat(".")
	}
	cat("\n")
	beta.est
}

set.seed(0, "Mersenne-Twister")

nrecv <- 1000







nreps <- 30

n <- round(10^(1.25 + .25 * 1:9))
nrecv <- round(10^(1.25 + .25 * 1:8))[4]
dim <- 5
prob.size <- .75

nmax <- max(n)
nrecvmax <- max(nrecv)

xmax <- matrix(rbinom(nrecvmax * dim, 1, .5), nrecvmax, dim)
beta <- c(0.8, -0.4, 0.3, 0.1, 0.2)

sizemax <- pmin(rgeom(nmax, prob.size) + 1, 15)
#sizemax <- rep(1, nmax)


for (j in seq_along(nrecv)) {
	nrecvj <- nrecv[j]
	x <- xmax[1:nrecvj, , drop=FALSE]
    eta <- x %*% beta
    eta1 <- eta - max(eta)
    w <- exp(eta1)
    prob <- w / sum(w)

    beta.est <- array(NA, c(dim, length(n), nreps))

    set.seed(0)
    for (r in seq_len(nreps)) {
        ymax <- matrix(0, nmax, nrecvj)
        for (i in seq_len(nmax)) {
            j <- sample.set(nrecv, sizemax[i], prob)
            ymax[i,j] <- 1
        }

        for (i in seq_along(n)) {
    		ni <- n[i]
            y <- ymax[seq_len(ni),]
            offset <- rep(log(ni), nrecvj)

            fit <- glm(colSums(y) ~ x, family=poisson)
		    beta.est[,i,r] <- fit$coef[-1]
        }
        cat(".")
    }

    biastot <- apply((beta.est - beta)^2, c(2,3), sum)
    biastot.mean <- apply(biastot, 1, mean)
    biastot.se <- apply(biastot, 1, sd) / sqrt(nreps)
}




