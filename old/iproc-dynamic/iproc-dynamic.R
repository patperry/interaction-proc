# iproc-dynamic.R
# ---------------

source("analysis/iproc-dynamic/iproc.out.R")


staticCount <- 144
sendInterval <- send.intervals
sendCount <- length(sendInterval)
recvInterval <- recv.intervals
recvCount <- length(recvInterval)

penalty <- 1e-4
fisher <- hess + diag(penalty, nrow(hess))
invFisher <- solve(fisher)
invFisher.static <- invFisher[seq_len(staticCount), seq_len(staticCount)]
invFisher.send <- invFisher[staticCount + seq_len(sendCount),
                            staticCount + seq_len(sendCount)]
invFisher.recv <- invFisher[staticCount + sendCount + seq_len(recvCount),
                            staticCount + sendCount + seq_len(recvCount)]


coef.static <- coefs[seq_len(staticCount)]
coef.send <- coefs[staticCount + seq_len(sendCount)]
coef.recv <- coefs[staticCount + sendCount + seq_len(recvCount)]

cum.send <- matrix(0, sendCount, sendCount)
cum.send[row(cum.send) <= col(cum.send)] <- 1
est.send <- cum.send %*% coef.send
se.send <- sqrt(diag(cum.send %*% invFisher.send %*% t(cum.send)))

cum.recv <- matrix(0, recvCount, recvCount)
cum.recv[row(cum.recv) <= col(cum.recv)] <- 1
est.recv <- cum.recv %*% coef.recv
se.recv <- sqrt(diag(cum.recv %*% invFisher.recv %*% t(cum.recv)))


par(mfrow=c(1,1))
pdf("analysis/iproc-dynamic/followup.pdf", 6, 6)
level <- 0.05
col <- rgb(180, 44, 113, max=255)
est <- exp(est.send)
int <- sendInterval
lower <- exp(est.send + qnorm(0.5*level) * se.send)
upper <- exp(est.send - qnorm(0.5*level) * se.send)
r <- 0.2
plot(c(int, int), c(lower, upper), t = "n", log = "xy", main = "",
     xlab = "", ylab = "", axes = FALSE)

segments(int, lower, int, upper, col="darkgray")
segments(2^(log2(int) - r), lower, 2^(log2(int) + r), lower, col="darkgray")
segments(2^(log2(int) - r), upper, 2^(log2(int) + r), upper, col="darkgray")

lines(int, est, col=col)
points(int, est, col=col, pch=16)

axis(1, at = c(30, 30 * 60, 30 * 60 * 60, 30 * 60 * 60 * 60),
     labels = c("30 secs", "30 mins", "30 hours", "75 days"))
axis(2)
axis(3, at = c(30, 30 * 60, 30 * 60 * 60, 30 * 60 * 60 * 60),
    labels = FALSE)
axis(4, labels = FALSE)
box()
dev.off()


par(mfrow=c(1,1))
pdf("analysis/iproc-dynamic/reciprocation.pdf", 6, 6)
level <- 0.05
col <- rgb(180, 44, 113, max=255)
est <- exp(est.recv)
int <- recvInterval
lower <- exp(est.recv + qnorm(0.5*level) * se.recv)
upper <- exp(est.recv - qnorm(0.5*level) * se.recv)
r <- 0.2
plot(c(int, int), c(lower, upper), t = "n", log = "xy", main = "",
     xlab = "", ylab = "", axes = FALSE)

segments(int, lower, int, upper, col="darkgray")
segments(2^(log2(int) - r), lower, 2^(log2(int) + r), lower, col="darkgray")
segments(2^(log2(int) - r), upper, 2^(log2(int) + r), upper, col="darkgray")

lines(int, est, col=col)
points(int, est, col=col, pch=16)

axis(1, at = c(30, 30 * 60, 30 * 60 * 60, 30 * 60 * 60 * 60),
     labels = c("30 secs", "30 mins", "30 hours", "75 days"))
axis(2)
axis(3, at = c(30, 30 * 60, 30 * 60 * 60, 30 * 60 * 60 * 60),
    labels = FALSE)
axis(4, labels = FALSE)
box()
dev.off()

