# figures/network.R
# -----------------


source("code/process.R")

orange <- rgb(253, 141,  60, max = 255)
purple <- rgb(158, 154, 200, max = 255)
palette(c(orange, purple))

fit <- fromJSON("output/fit-dynamic.json")
cov <- get.cov(fit)

intvl <- c(fit$intervals, Inf)
nintvl <- length(intvl)
intvl.str <- c(0, "30m", "2h", "8h", "1.3d", "5.3d", "21.3d",
               expression(infinity))

coef.d <- get.coefs(fit)[-(1:11),1]
cov.d <- cov[12:(nrow(cov)/12), 12:(nrow(cov)/12)]
se.d <- sqrt(diag(cov.d))

ind.ix <- match(c("ISend", "IRecv", "ISend2", "IRecv2", "ISib", "ICosib"),
                names(coef.d))
dyad.ix <- c(ind.ix[1] + 1:nintvl, ind.ix[2] + 1:nintvl)
triad.ix <- c(setdiff(1:length(coef.d), c(ind.ix, dyad.ix)))


coef.ind <- coef.d[ind.ix]
se.ind <- se.d[ind.ix]

cbind(coef.ind, se.ind)



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
                       yscale=extendrange(c(min((coef.d - se.d)[dyad.ix]),
                                            max((coef.d + se.d)[dyad.ix]))))

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



plot.triad1 <- function(beta, se, name="") {
    scale <- max((abs(coef.d) + se.d)[triad.ix]) * 2

    for (j in 1:ncol(beta)) {
        for (i in 1:nrow(beta)) {
            x <- i
            y <- ncol(beta) + 1 - j
            w <- 0.5
            h <- beta[i,j] / scale
            s <- se[i,j] / scale
            
            grid.rect(x, y, w, h, just=c("center", "bottom"),
                      default.units="native", gp=gpar(col=1, fill=1))
            grid.segments(x - 1.5 * w/2, y, x + 1.5 * w/2, y,
                          default.units="native")
            grid.segments(x - w/6, y + h + s, x + w/6, y + h + s,
                          default.units="native", gp=gpar(col=2))
            grid.segments(x - w/6, y + h - s, x + w/6, y + h - s,
                          default.units="native", gp=gpar(col=2))
            grid.segments(x, y + h + s, x, y + h - s,
                          default.units="native", gp=gpar(col=2))
        }
    }
    grid.text(name, nintvl - 1, 0.5,
              gp=gpar(fontface="bold"), default.units="native")
}

plot.triad <- function() {
    dv <- dataViewport(xscale=extendrange(c(0.5, nintvl + 0.5)),
                       yscale=extendrange(c(0.5, nintvl + 0.5)))

    pushViewport(viewport(layout=grid.layout(2, 1, heights=c(0.95, 0.05))))

    pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
    pushViewport(viewport(layout=grid.layout(2, 2)))
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
    pushViewport(plotViewport(c(1,4,4,1)))
    pushViewport(dv)
    plot.triad1(send2, send2.se, "2-Send")
    grid.rect()
    grid.xaxis(at=1:nintvl, label=intvl.str[-1], main=FALSE) 
    grid.yaxis(at=nintvl:1, label=intvl.str[-1]) 
    popViewport(3)

    pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
    pushViewport(plotViewport(c(1,1,4,4)))
    pushViewport(dv)
    plot.triad1(recv2, recv2.se, "2-Receive")
    grid.rect()
    grid.xaxis(at=1:nintvl, label=intvl.str[-1], main=FALSE) 
    grid.yaxis(at=nintvl:1, label=FALSE, main=FALSE)
    popViewport(3)

    pushViewport(viewport(layout.pos.col=1, layout.pos.row=2))
    pushViewport(plotViewport(c(4,4,1,1)))
    pushViewport(dv)
    plot.triad1(sib, sib.se, "Sibling")
    grid.rect()
    grid.xaxis(at=1:nintvl, label=FALSE)
    grid.yaxis(at=nintvl:1, label=intvl.str[-1]) 

    just <- c("center", "bottom")
    gp <- gpar(col=1, fill=1)
    scale <- max((abs(coef.d) + se.d)[triad.ix]) * 2
    y <- -3
    w <- 0.5
    dx <- 0.265

    grid.rect(2 + dx, y, w, -0.3/scale, default.units="native", just=just, gp=gp)
    grid.segments(2 + dx - 1.5*w/2, y, 2 + dx + 1.5*w/2, y, default.units="native")
    grid.text(-0.3, 2 + dx, y + 0.35/scale, just=just, default.units="native")
    grid.rect(4 + dx, y, w, -0.2/scale, default.units="native", just=just, gp=gp)
    grid.segments(4 + dx - 1.5*w/2, y, 4 + dx + 1.5*w/2, y, default.units="native")
    grid.text(-0.2, 4 + dx, y + 0.35/scale, just=just, default.units="native")
    grid.rect(6 + dx, y, w, -0.1/scale, default.units="native", just=just, gp=gp)
    grid.segments(6 + dx - 1.5*w/2, y, 6 + dx + 1.5*w/2, y, default.units="native")
    grid.text(-0.1, 6 + dx, y + 0.35/scale, just=just, default.units="native")
    grid.rect(8 + dx, y, w, 0.0/scale, default.units="native", just=just, gp=gp)
    grid.segments(8 + dx - 1.5*w/2, y, 8 + dx + 1.5*w/2, y, default.units="native")
    grid.text("0.0", 8 + dx, y + 0.35/scale, just=just, default.units="native")
    grid.rect(10 + dx, y, w, 0.1/scale, default.units="native", just=just, gp=gp)
    grid.segments(10 + dx - 1.5*w/2, y, 10 + dx + 1.5*w/2, y, default.units="native")
    grid.text(0.1, 10 + dx, y + 0.35/scale, just=just, default.units="native")
    grid.rect(12 + dx, y, w, 0.2/scale, default.units="native", just=just, gp=gp)
    grid.segments(12 + dx - 1.5*w/2, y, 12 + dx + 1.5*w/2, y, default.units="native")
    grid.text(0.2, 12 + dx, y + 0.35/scale, just=just, default.units="native")
    grid.rect(14 + dx, y, w, 0.3/scale, default.units="native", just=just, gp=gp)
    grid.segments(14 + dx - 1.5*w/2, y, 14 + dx + 1.5*w/2, y, default.units="native")
    grid.text(0.3, 14 + dx, y + 0.35/scale, just=just, default.units="native")

    popViewport(3)

    pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
    pushViewport(plotViewport(c(4,1,1,4)))
    pushViewport(dv)
    plot.triad1(cosib, cosib.se, "Cosibling")
    grid.rect()
    grid.xaxis(at=1:nintvl, label=FALSE)
    grid.yaxis(at=nintvl:1, label=FALSE, main=FALSE)
    popViewport(3)
    popViewport(1)
    grid.text("First Time Elapsed", unit(0.5, "lines"), 0.5, rot=90)
    grid.text("Second Time Elapsed", 0.5, unit(1, "npc") - unit(1, "lines"))
    popViewport(1)
    grid.text("Coefficient", 0.5, unit(1, "lines"))
}



pdf("figures/triad.pdf", 8, 6)
palette(c(orange, purple))
grid.newpage()
plot.triad()
dev.off()

