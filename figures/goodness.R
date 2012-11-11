# figures/goodness.R
# ------------------


source("code/utils.R")

orange <- rgb(253, 141,  60, max = 255)
purple <- rgb(158, 154, 200, max = 255)
col <- rgb(253, 141, 60, 128, max=255)
palette(c(orange, purple))

fit <- read.h5out("output/fit-dynamic.h5")
fit.s <- read.h5out("output/fit-static.h5")

nobs <- fit$observed
nexp <- fit$fitted
nexp.s <- fit.s$fitted

ix <- row(nobs) != col(nobs)
nobs <- nobs[ix]
nexp <- nexp[ix]
nexp.s <- nexp.s[ix]

resid <- (nobs - nexp) / sqrt(nexp)
resid[nexp == 0 & nobs == 0] <- 0
resid.s <- (nobs - nexp.s) / sqrt(nexp.s)

n <- length(nexp)
ng <- 100

o <- order(nexp)
o.s <- order(nexp.s)
g <- 1 + floor(ng * o / (n + 1))
g.s <- 1 + floor(ng * o.s / (n + 1))

ylim <- range(c(resid, resid.s), na.rm=TRUE)

pdf("figures/nobs-by-nexp.pdf", 8, 4.5)
par(mfrow=c(1, 2))
par(mar=c(5.1, 4.1, 4.1, 1.1))
plot(nexp.s + 1, nobs + 1, log="xy", col=col, xlab="", ylab="Observed Count + 1", main="Static")
abline(0, 1, lty=2)
axis(3, labels=FALSE)
par(mar=c(5.1, 1.1, 4.1, 4.1))
plot(nexp + 1, nobs + 1, log="xy", col=col, xlab="", ylab="", axes=FALSE, main="Static and Dynamic")
abline(0, 1, lty=2)
box()
axis(1)
axis(3, labels=FALSE)
axis(4, labels=FALSE)
mtext("Expected Count + 1", side=1, line=3, at=0.5)
dev.off()

pdf("figures/resid-by-nexp.pdf", 8, 4.5)
par(mfrow=c(1, 2))
par(mar=c(5.1, 4.1, 4.1, 1.1))
plot(order(nexp.s), resid.s, ylim=ylim, col=col, axes=FALSE, xlab="", ylab="Pearson Residual", main="Static")
p <- seq(0, 1, len=5)
q.s <- quantile(nexp.s, p)
box()
axis(1, at=n*p, labels=round(q.s, 2))
axis(1, at=n*p, labels=paste("(", p, ")", sep=''), line=1, lwd=0)
axis(2)
axis(3, at=n*p, labels=FALSE)
par(mar=c(5.1, 1.1, 4.1, 4.1))
plot(order(nexp), resid, ylim=ylim, col=col, axes=FALSE, xlab="", ylab="", main="Static and Dynamic")
box()
q <- quantile(nexp, p)
axis(1, at=n*p, labels=round(q, 2))
axis(1, at=n*p, labels=paste("(", p, ")", sep=''), line=1, lwd=0)
axis(3, at=n*p, labels=FALSE)
axis(4, labels=FALSE)
mtext("Expected Count (Quantile)", side=1, line=3.5, at=0)
dev.off()

