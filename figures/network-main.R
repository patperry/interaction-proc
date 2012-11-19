# figures/network.R
# -----------------


source("code/utils.R")
require(grid)

orange <- rgb(253, 141,  60, max = 255)
purple <- rgb(158, 154, 200, max = 255)
palette(c(orange, purple))

fit <- read.h5out("output/fit-dynamic-main.h5")
load("output/boot-main.rda")
cov <- get.cov(fit)

intvl <- c(fit$intervals)
nintvl <- length(intvl)
intvl.str <- c(0, "30m", "2h", "8h", "1.3d", "5.3d", "21.3d",
               expression(infinity))

coef <- fit$coef
cov <- get.cov(fit)
se <- sqrt(diag(cov))


nm <- names(coef)
ni <- length(fit$intervals)
i1 <- seq(from=0, by = 1, length.out = ni)
i2 <- seq(from=0, by = 1, length.out = ni^2)

ix.recv <- c(match("IRecv", nm), match("NRecv[1]", nm) + i1)
ix.send <- c(match("ISend", nm), match("NSend[1]", nm) + i1)
ix.recv2 <- c(match("IRecv2", nm), match("NRecv2[1,1]", nm) + i2)
ix.send2 <- c(match("ISend2", nm), match("NSend2[1,1]", nm) + i2)
ix.sib <- c(match("ISib", nm), match("NSib[1,1]", nm) + i2)
ix.cosib <- c(match("ICosib", nm), match("NCosib[1,1]", nm) + i2)
ix <- c(ix.send, ix.recv, ix.send2, ix.recv2, ix.sib, ix.cosib)
ix.stat <- setdiff(seq_along(nm), ix)
ix <- c(ix.stat, ix)

dyad.ix <- c(ix.recv[-1], ix.send[-1])
triad.ix <- c(ix.recv2[-1], ix.send2[-1], ix.sib[-1], ix.cosib[-1])


send <- coef[ix.send[-1]]
send.se <- se[ix.send[-1]]
send.bias <- bias.mean[ix.send[-1]]

recv <- coef[ix.recv[-1]]
recv.se <- se[ix.recv[-1]]
recv.bias <- bias.mean[ix.recv[-1]]

send2 <- matrix(coef[ix.send2[-1]], nintvl, byrow=TRUE)
send2.se <- matrix(se[ix.send2[-1]], nintvl, byrow=TRUE)
send2.bias <- matrix(bias.mean[ix.send2[-1]], nintvl, byrow=TRUE)

recv2 <- matrix(coef[ix.recv2[-1]], nintvl, byrow=TRUE)
recv2.se <- matrix(se[ix.recv2[-1]], nintvl, byrow=TRUE)
recv2.bias <- matrix(bias.mean[ix.recv2[-1]], nintvl, byrow=TRUE)

sib <- matrix(coef[ix.sib[-1]], nintvl, byrow=TRUE)
sib.se <- matrix(se[ix.sib[-1]], nintvl, byrow=TRUE)
sib.bias <- matrix(bias.mean[ix.sib[-1]], nintvl, byrow=TRUE)

cosib <- matrix(coef[ix.cosib[-1]], nintvl, byrow=TRUE)
cosib.se <- matrix(se[ix.cosib[-1]], nintvl, byrow=TRUE)
cosib.bias <- matrix(bias.mean[ix.cosib[-1]], nintvl, byrow=TRUE)


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
                       yscale=extendrange(c(min((coef - bias.mean - se)[dyad.ix]),
                                            max((coef - bias.mean + se)[dyad.ix]))))

    pushViewport(viewport(layout=grid.layout(1, 2)))
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))

    pushViewport(plotViewport(c(4, 4, 4, 1)))
    pushViewport(dv)

    plot.dyad1(send - send.bias, send.se, name="Send")
    grid.rect()
    grid.xaxis(at=0:nintvl, label=intvl.str)
    grid.yaxis()
    grid.xaxis(at=0:nintvl, label=FALSE, main=FALSE)
    grid.text("Coefficient", x=unit(-3, "lines"), rot=90)

    popViewport(3)
    pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
    pushViewport(plotViewport(c(4, 1, 4, 4)))
    pushViewport(dv)
    plot.dyad1(recv - recv.bias, recv.se, name="Receive")
    grid.rect()
    grid.xaxis(at=0:nintvl, label=intvl.str)
    grid.xaxis(at=0:nintvl, label=FALSE, main=FALSE)
    grid.yaxis(main=FALSE)
    popViewport(3)
    grid.text("Time Elapsed", y=unit(1, "lines"))
}

pdf("figures/dyad-main.pdf", 8, 4.5)
palette(c(orange, purple))
grid.newpage()
plot.dyad()
dev.off()



plot.triad1 <- function(beta, beta.se, name="") {
    scale <- max((abs(coef - bias.mean) + se)[triad.ix]) * 2

    for (j in 1:ncol(beta)) {
        for (i in 1:nrow(beta)) {
            x <- j
            y <- ncol(beta) + 1 - i
            w <- 0.5
            h <- beta[i,j] / scale
            s <- beta.se[i,j] / scale

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
    plot.triad1(send2 - send2.bias, send2.se, "2-Send")
    grid.rect()
    grid.xaxis(at=1:nintvl, label=intvl.str[-1], main=FALSE) 
    grid.yaxis(at=nintvl:1, label=intvl.str[-1]) 
    popViewport(3)

    pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
    pushViewport(plotViewport(c(1,1,4,4)))
    pushViewport(dv)
    plot.triad1(recv2 - recv2.bias, recv2.se, "2-Receive")
    grid.rect()
    grid.xaxis(at=1:nintvl, label=intvl.str[-1], main=FALSE) 
    grid.yaxis(at=nintvl:1, label=FALSE, main=FALSE)
    popViewport(3)

    pushViewport(viewport(layout.pos.col=1, layout.pos.row=2))
    pushViewport(plotViewport(c(4,4,1,1)))
    pushViewport(dv)
    plot.triad1(sib - sib.bias, sib.se, "Sibling")
    grid.rect()
    grid.xaxis(at=1:nintvl, label=FALSE)
    grid.yaxis(at=nintvl:1, label=intvl.str[-1]) 

    just <- c("center", "bottom")
    gp <- gpar(col=1, fill=1)
    scale <- max((abs(coef - bias.mean) + se)[triad.ix]) * 2
    y <- -3
    w <- 0.5
    dx <- 0.265

    grid.rect(2 + dx, y, w, -0.45/scale, default.units="native", just=just, gp=gp)
    grid.segments(2 + dx - 1.5*w/2, y, 2 + dx + 1.5*w/2, y, default.units="native")
    grid.text("-0.45 ", 2 + dx, y + 0.6/scale, just=just, default.units="native")

    grid.rect(4 + dx, y, w, -0.30/scale, default.units="native", just=just, gp=gp)
    grid.segments(4 + dx - 1.5*w/2, y, 4 + dx + 1.5*w/2, y, default.units="native")
    grid.text("-0.30 ", 4 + dx, y + 0.6/scale, just=just, default.units="native")

    grid.rect(6 + dx, y, w, -0.15/scale, default.units="native", just=just, gp=gp)
    grid.segments(6 + dx - 1.5*w/2, y, 6 + dx + 1.5*w/2, y, default.units="native")
    grid.text("-0.15 ", 6 + dx, y + 0.6/scale, just=just, default.units="native")

    grid.rect(8 + dx, y, w, 0.0/scale, default.units="native", just=just, gp=gp)
    grid.segments(8 + dx - 1.5*w/2, y, 8 + dx + 1.5*w/2, y, default.units="native")
    grid.text("0.00", 8 + dx, y + 0.6/scale, just=just, default.units="native")

    grid.rect(10 + dx, y, w, 0.15/scale, default.units="native", just=just, gp=gp)
    grid.segments(10 + dx - 1.5*w/2, y, 10 + dx + 1.5*w/2, y, default.units="native")
    grid.text(" 0.15 ", 10 + dx, y + 0.6/scale, just=just, default.units="native")

    grid.rect(12 + dx, y, w, 0.30/scale, default.units="native", just=just, gp=gp)
    grid.segments(12 + dx - 1.5*w/2, y, 12 + dx + 1.5*w/2, y, default.units="native")
    grid.text(" 0.30 ", 12 + dx, y + 0.6/scale, just=just, default.units="native")

    grid.rect(14 + dx, y, w, 0.45/scale, default.units="native", just=just, gp=gp)
    grid.segments(14 + dx - 1.5*w/2, y, 14 + dx + 1.5*w/2, y, default.units="native")
    grid.text(" 0.45 ", 14 + dx, y + 0.6/scale, just=just, default.units="native")

    popViewport(3)

    pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
    pushViewport(plotViewport(c(4,1,1,4)))
    pushViewport(dv)
    plot.triad1(cosib - cosib.bias, cosib.se, "Cosibling")
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



pdf("figures/triad-main.pdf", 8, 6)
palette(c(orange, purple))
grid.newpage()
plot.triad()
dev.off()

