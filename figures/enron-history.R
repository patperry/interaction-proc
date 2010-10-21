# figures/enron-history.R
# -----------------------

source("code/message.R")

pdffile <- "figures/enron-history.pdf"
pdfout <- TRUE

nbins <- 75
col.hist    <- rgb(158, 154, 200, max = 255) # purple
col.density <- rgb(253, 141,  60, max = 255) # orange

margin <- 0.82
padding <- margin/2

if (pdfout) {
    width.fig <- 8.3
    height.fig <- (width.fig + margin - padding)/2
    pdf(pdffile, width = width.fig, height = height.fig)
}

fin <- par()$fin
width = fin[1] - 2 * margin
height = fin[2] - margin - padding

par(las = 1)
par(plt = c(c(margin, margin + width)/fin[1],
            c(margin, margin + height)/fin[2]), new = FALSE)

par(las = 1)


msgs <- GetMessages(duplicate = FALSE)
time <- msgs$time
secs <- as.numeric(time)
n <- length(secs)

h <- hist(secs, nbins, plot = FALSE)
d <- density(secs)


plot(h, col = col.hist, axes = FALSE,
     xlab = "Year",
     ylab = "Message Frequency",
     main = "")
lines(den$x, n * mean(diff(h$breaks))  * den$y, col = col.density, lwd = 2)

axis(2)
axis(4, labels = FALSE)

at <- c(as.numeric(as.POSIXct("1999-1-1")),
        as.numeric(as.POSIXct("2000-1-1")),
        as.numeric(as.POSIXct("2001-1-1")),
        as.numeric(as.POSIXct("2002-1-1")))
labels <- c("1999", "2000", "2001", "2002")
axis(1, at = at, labels = labels)
axis(3, at = at, labels = FALSE)
box()

if (pdfout) dev.off()
