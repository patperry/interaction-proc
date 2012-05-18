# multicast.R
#

sample.set <- function(n, size, prob=NULL, maxit=1e5, warn=TRUE) {
	if (size == 1)
		return(sample.int(n, 1, prob=prob))

	i <- c()
	done <- FALSE
	it <- 0
	while (!done && it < maxit) {
		it <- it + 1
		i <- sample.int(n, size, replace=TRUE, prob)
                i <- unique(i)
                done <- length(i) == size
	}

	if (it == maxit) {
		if (warn) {
			browser()
			warning("Failed to sample after ", maxit, " tries.")
		}
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







nreps <- 100

n <- round(10^(1.25 + .25 * 1:15))
nrecv <- round(10^(1.25 + .25 * 1:7))
dim <- 5
prob.size <- prob.size <- 0.4 # 10^(-.2 * 2:0)

nmax <- max(n)
nrecvmax <- max(nrecv)

xmax <- matrix(rbinom(nrecvmax * dim, 1, .5), nrecvmax, dim)
beta <- rnorm(dim) # c(0.8, -0.4, 0.3, 0.1, 0.2)


biastot.mean <- array(NA, c(length(n), length(nrecv), length(prob.size)))
biastot.se <- array(NA, c(length(n), length(nrecv), length(prob.size)))

for (k in seq_along(prob.size)) {
    set.seed(2012)
    sizemax <- rgeom(nmax, prob.size[k]) + 1
    sizemax <- pmin(sizemax, floor(min(n) / 2))
    #sizemax <- rep(1, nmax)

    for (j in seq_along(nrecv)) {
        nrecvj <- nrecv[j]
        x <- xmax[1:nrecvj, , drop=FALSE]
        eta <- x %*% beta
        eta1 <- eta - max(eta)
        w <- exp(eta1)
        prob <- w / sum(w)

        beta.est <- array(NA, c(dim, length(n), nreps))

	set.seed(1337)
        for (r in seq_len(nreps)) {
            ymax <- matrix(0, nmax, nrecvj)
            for (i in seq_len(nmax)) {
                recv <- sample.set(nrecvj, sizemax[i], prob)
                ymax[i,recv] <- 1
            }

            for (i in seq_along(n)) {
                ni <- n[i]
                y <- ymax[seq_len(ni),]

                fit <- glm(colSums(y) ~ x, family=poisson)
                beta.est[,i,r] <- fit$coef[-1]
            }
	    cat(".")
        }

        biastot <- apply((beta.est - beta)^2, c(2,3), sum)
        biastot.mean[,j,k] <- apply(biastot, 1, mean)
        biastot.se[,j,k] <- apply(biastot, 1, sd) / sqrt(nreps)
    }
    cat("|")
}
cat("\n")


#par(mfrow=c(3,2))



#plot(range(log10(n)), range((sqrt(n) * biastot.mean[,,k])), t="n")
save(n, nrecv, biastot.mean, biastot.se, prob.size, beta, nmax, nrecvmax, dim, file = "output/multicast.rda")


require(RColorBrewer)

pdf("figures/multicast-error.pdf", 6, 6)



palette(brewer.pal(9, "YlGn")[9:3])
par(mfrow=c(1,1))
for (k in seq_along(prob.size)[1]) {
    plot(range(log10(n)), range(log10(biastot.mean[,,k])), t="n", asp=1,
	 main="", # "Multicast Coefficient Estimation",
	 xlab=expression(Log[10]~"Sample Size"),
	 ylab=expression(Log[10]~"Mean Squared Error"))
    axis(3, labels=FALSE)
    axis(4, labels=FALSE)

    for (j in seq_along(nrecv)) {
	    points(log10(n), log10(biastot.mean[,j,k]), col=j)
	    lines(log10(n), log10(biastot.mean[,j,k]), col=j, lty=1)
    }
}
legend("bottomleft", inset=.05,
       lty=rep(1, length(nrecv)), col=seq_along(nrecv),
       legend=format(log10(nrecv - .4), digits=3),
       title=expression(Log[10]~"Receiver Count"))


m <- sum(nrecv < sqrt(max(n)))
ix <- rep(NA, m)
b <- rep(NA, m)
k <- 1
for (j in seq_len(m)) {
	ix[j] <- which(round(sqrt(n)) == nrecv[j])
	b[j] <- biastot.mean[ix[j],j,k]
}

points(log10(n[ix]), log10(b), pch=18, cex=1.5)
lines(log10(n[ix]), log10(b), lty=2)


dev.off()
