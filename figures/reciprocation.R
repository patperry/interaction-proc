# figures/reciprocation.R
# -----------------------

pdffile <- "figures/reciprocation.pdf"
pdfout <- TRUE

# purple
color.points <- rgb(158, 154, 200, max = 255)

# orange
color.segments <- rgb(253, 141,  60, max = 255)

margin <- 0.82
padding <- margin/2

if (pdfout) {
    width.fig <- 4.15 * 1.25
    height.fig <- width.fig - margin + padding
    pdf(pdffile, width = width.fig, height = height.fig)
}

fin <- par()$fin
width = fin[1] - 2 * margin
height = fin[2] - margin - padding

par(las = 1)
par(plt = c(c(margin, margin + width)/fin[1],
            c(margin, margin + height)/fin[2]), new = FALSE)


sys.source("analysis/dynamic.R",
    dynamic <- new.env(parent = baseenv()))

intervals <- dynamic$intervals.recv
coefs <- with(dynamic,
              coefs[seq.int(from = length(intervals.send) + 1, 
                            length.out = length(intervals.recv))])
coefs.se <- with(dynamic,
                 coefs.se[seq.int(from = length(intervals.send) + 1, 
                                  length.out = length(intervals.recv))])

log.intervals = log(intervals)
plot(c(log.intervals, log.intervals, max(log.intervals)),
     c(coefs - coefs.se, coefs + coefs.se, 0),
     t = "n",
     axes = FALSE,
     xlab = "Time of Most Recent Receive",
     ylab = "Relative Send Intensity")

# bar.width <- 0.1
# segments(log.intervals, coefs - coefs.se,
#          log.intervals, coefs + coefs.se,
#          col = color.segments, lwd = 2)
# segments(log.intervals - bar.width, coefs - coefs.se,
#          log.intervals + bar.width, coefs - coefs.se,
#          col = color.segments, lwd = 2)
# segments(log.intervals - bar.width, coefs + coefs.se,
#          log.intervals + bar.width, coefs + coefs.se,
#          col = color.segments, lwd = 2)

points(log.intervals, coefs - coefs.se, col = color.segments, pch = 16, cex=0.5)
points(log.intervals, coefs + coefs.se, col = color.segments, pch = 16, cex=0.5)
segments(c(par()$usr[1],log.intervals), c(coefs - coefs.se, 0),
         c(log.intervals, par()$usr[2]), c(coefs - coefs.se, 0),
         col = color.segments, lty = 2)
segments(c(par()$usr[1],log.intervals), c(coefs + coefs.se, 0),
         c(log.intervals, par()$usr[2]), c(coefs + coefs.se, 0),
         col = color.segments, lty = 2)



points(log.intervals, coefs, col = color.points, pch = 16, cex=0.75)
# lines(log.intervals, coefs, col = color.points)
segments(c(par()$usr[1],log.intervals), c(coefs, 0),
         c(log.intervals, par()$usr[2]), c(coefs, 0),
         col = color.points, lwd = 2)
         

xaxis.at = log(c(30, 30 * 60, 30 * 60 * 60, 75 * 24 * 60 * 60))
xaxis.labels = c("30 secs", "30 mins", "30 hours", "75 days")
axis(1, at = xaxis.at, labels = xaxis.labels)
axis(3, at = xaxis.at, labels = FALSE)

yaxis.at = log(c(1, 10, 100, 1000))
yaxis.labels = c("1", "10", "100", "1000")
axis(2, at = yaxis.at, labels = yaxis.labels)
axis(4, at = yaxis.at, labels = FALSE)

box()

if (pdfout) dev.off()
