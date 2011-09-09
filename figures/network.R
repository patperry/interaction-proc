# figures/network.R
# -----------------


source("code/process.R")

orange <- rgb(253, 141,  60, max = 255)
purple <- rgb(158, 154, 200, max = 255)
palette(c(orange, purple))

fit <- fromJSON("output/fit-dynamic.json")
cov <- get.cov(fit)

coef.d <- get.coefs(fit)[-(1:11),1]
cov.d <- cov[12:(nrow(cov)/12), 12:(nrow(cov)/12)]
se.d <- sqrt(diag(cov.d))

ind.ix <- match(c("ISend", "IRecv", "ISend2", "IRecv2", "ISib", "ICosib"),
                names(coef.d))

coef.ind <- coef.d[ind.ix]
se.ind <- se.d[ind.ix]

cbind(coef.ind, se.ind)

intvl <- c(fit$intervals, Inf)
nintvl <- 7 
intvl.str <- c(0, "30m", "2h", "8h", "1.3d", "5.3d", "21.3d",
               expression(infinity))


send <- coef.d[ind.ix[1] + 1:nintvl]
send.se <- se.d[ind.ix[1] + 1:nintvl]
recv <- coef.d[ind.ix[2] + 1:nintvl]
recv.se <- se.d[ind.ix[2] + 1:nintvl]
send2 <- matrix(coef.d[ind.ix[3] + 1:(nintvl^2)], nintvl)
send2.se <- matrix(se.d[ind.ix[3] + 1:(nintvl^2)], nintvl)
recv2 <- matrix(coef.d[ind.ix[4] + 1:(nintvl^2)], nintvl)
recv2.se <- matrix(se.d[ind.ix[4] + 1:(nintvl^2)], nintvl)
sib <- matrix(coef.d[ind.ix[5] + 1:(nintvl^2)], nintvl)
sib.se <- matrix(se.d[ind.ix[5] + 1:(nintvl^2)], nintvl)
cosib <- matrix(coef.d[ind.ix[6] + 1:(nintvl^2)], nintvl)
cosib.se <- matrix(se.d[ind.ix[6] + 1:(nintvl^2)], nintvl)

beta <- send
se <- send.se

plot.dyad1 <- function(beta, se, name=NULL) {
    grid.segments(0:(nintvl-1), beta, 1:nintvl, beta,
                  default.units="native", gp=gpar(col=1))
    #grid.segments(1:(nintvl-1), beta[-nintvl],
    #              1:(nintvl-1), beta[-1],
    #              default.units="native",
    #              gp=gpar(lty=2, col=1))

    grid.segments(0:(nintvl-1), beta + se, 1:nintvl, beta + se,
                  default.units="native",
                  gp=gpar(col=2, lty=2))
    grid.segments(0:(nintvl-1), beta - se, 1:nintvl, beta - se,
                  default.units="native",
                  gp=gpar(col=2, lty=2))
    grid.text(name, y=unit(1, "npc") + unit(3, "lines"),
              gp=gpar(fontface="bold"))
}


plot.dyad <- function() {
    dv <- dataViewport(xscale=extendrange(c(0, nintvl)),
                       yscale=extendrange(c(min((coef.d - se.d)[-ind.ix]),
                                            max((coef.d + se.d)[-ind.ix]))))

    pushViewport(viewport(layout=grid.layout(1, 2)))
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))

    pushViewport(plotViewport(c(4, 4, 4, 1)))
    pushViewport(dv)

    plot.dyad1(send, send.se, name="Send")
    grid.rect()
    grid.xaxis(at=0:nintvl, label=intvl.str)
    grid.yaxis()
    grid.xaxis(at=0:nintvl, label=FALSE, main=FALSE)
    grid.text("Coefficient", x=unit(-3, "lines"), rot=90)

    popViewport(3)
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
    pushViewport(plotViewport(c(4, 1, 4, 4)))
    pushViewport(dv)
    plot.dyad1(recv, recv.se, name="Receive")
    grid.rect()
    grid.xaxis(at=0:nintvl, label=intvl.str)
    grid.xaxis(at=0:nintvl, label=FALSE, main=FALSE)
    grid.yaxis(main=FALSE)
    popViewport(3)
    grid.text("Time Elapsed", y=unit(1, "lines"))
}

pdf("figures/dyad.pdf", 8, 4.5)
palette(c(orange, purple))
grid.newpage()
plot.dyad()
dev.off()


