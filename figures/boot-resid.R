# figures/boot-resid.R
# --------------------

source("code/boot.R")
source("analysis/dynamic.R")

pdffile <- "figures/boot-resid.pdf"
pdfout <- TRUE

purple <- rgb(158, 154, 200, max = 255)
orange <- rgb(253, 141,  60, max = 255)

color.points <- orange
color.segments <- purple


margin <- 0.82
padding <- margin/2

if (pdfout) {
    width.fig <- 8.3
    height.fig <- 0.5 * width.fig - margin + padding
    pdf(pdffile, width = width.fig, height = height.fig)
}

fin <- par()$fin
width = fin[1] - 2 * margin
height = fin[2] - margin - padding

par(las = 1)
par(plt = c(c(margin, margin + width)/fin[1],
            c(margin, margin + height)/fin[2]), new = FALSE)

bias <- GetBias()
coefs1 <- coefs - bias$est
coefs1.se <- sqrt(bias$est.se^2 + coefs.se^2)

ndynamic <- length(intervals.send) + length(intervals.recv)
nstatic <- length(coefs) - ndynamic
ix.dynamic <- seq_len(ndynamic)
ix.static <- ndynamic + seq_len(nstatic)
ix <- c(ix.static, ix.dynamic)

nreps <- bias$nreps
bias.se <- bias$est.se
bias.sd <- sqrt(bias$nreps) * bias$est.se
bias <- bias$est


par(las = 1)

plot(bias[ix] / coefs.se[ix], t = "n", axes = FALSE,
     xlab = "Coefficient Index",
     ylab = "Normalized Residual") 
abline(v = nstatic + 1, lty = 2, col = "gray")
xaxis.at <- c(1, 73, 145)
axis(1, at = xaxis.at, labels = TRUE)
axis(3, at = xaxis.at, labels = FALSE)
axis(2, labels = TRUE)
axis(4, labels = FALSE)
box()

segments(seq_along(bias), ((bias - bias.sd) / coefs.se)[ix],
         seq_along(bias), ((bias + bias.sd) / coefs.se)[ix],
         col = color.segments)
segments(seq_along(bias) - 0.45, ((bias - bias.sd) / coefs.se)[ix],
         seq_along(bias) + 0.45, ((bias - bias.sd) / coefs.se)[ix],
         col = color.segments)
segments(seq_along(bias) - 0.45, ((bias + bias.sd) / coefs.se)[ix],
         seq_along(bias) + 0.45, ((bias + bias.sd) / coefs.se)[ix],
         col = color.segments)

points(bias[ix] / coefs.se[ix],
       pch = 16, cex = 0.5, col = color.points)

if (pdfout) dev.off()
