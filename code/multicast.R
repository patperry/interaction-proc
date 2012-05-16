# multicast.R
#

sample.set <- function(n, size, prob=NULL, maxit=1e5, warn=TRUE) {
	i <- c()
	done <- FALSE
	it <- 0
	while (!done && it < maxit) {
		it <- it + 1
		i0 <- sample.int(n, size, replace=TRUE, prob)
		i <- unique(i0)
		done <- length(i) == size
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







nreps <- 1000

n <- round(10^(1.25 + .25 * 1:9))
nrecv <- round(10^(1.25 + .25 * 1:8))
dim <- 5
prob.size <- .5

nmax <- max(n)
nrecvmax <- max(nrecv)

xmax <- matrix(2 * rbinom(nrecvmax * dim, 1, .5) - 1, nrecvmax, dim)
beta <- runif(dim, -1, 1)
sizemax <- pmin(rgeom(nmax, prob.size) + 1, 15)

for (j in seq_along(nrecv)) {
	nrecvj <- nrecv[j]
	xj <- xmax[1:nrecvj, , drop=FALSE]
	for (i in seq_along(n)) {
		ni <- n[i]
		sizei <- sizemax[1:ni]

		set.seed(i)
		beta.est <- bias.sim(ni, xj, beta, sizei, nreps)
		bias <- t(t(beta.est) - beta)
		bias <- sqrt(rowSums(bias^2))
		bias.mean <- mean(bias)
		bias.se <- sd(bias) / sqrt(nreps)

		cat("\n nrecv = ", nrecvj, "; n = ", ni, "; bias = ",
		    bias.mean, "(", bias.se, ")\n")
	}
}




