# figures/send-intensities.R
# --------------------------

source("code/employee.R")
source("code/message.R")

emp <- GetEmployees()
msg <- GetMessages()

pdffile <- "figures/send-intensities.pdf"
pdfout <- TRUE

# 'Purples' scheme from ColorBrewer2
colorRGB <- colorRamp(c(rgb(239, 237, 245, max = 255),
                        rgb(188, 189, 200, max = 255),
                        rgb(117, 107, 177, max = 255)), space = "Lab")
color <- function(q) rgb(colorRGB(q), max = 255)


nmsg <- rep(NA, length(emp$eid))
for (i in seq_along(emp$eid)) {
    eid <- emp$eid[i]
    nmsg[i] <- sum(msg$from == eid)
}

margin <- 0.82
padding <- margin/2

if (pdfout) {
    width.fig <- 8.3
    height.fig <- (width.fig + margin - padding)/2
    pdf(pdffile, width = width.fig, height = height.fig)
}

fin <- par()$fin
width = (fin[1] - 3 * margin - padding)/2
height = fin[2] - 2 * margin

par(las = 1)
par(plt = c(c(margin, margin + width)/fin[1],
            c(margin, margin + height)/fin[2]), new = FALSE)

q <- rank(nmsg)/(1 + length(nmsg))
plot(q, log2(nmsg), col = color(q),
     xlab = "Fraction of Senders",
     ylab = expression("Log"[2]*" Number of Messages"))
axis(3, labels = FALSE)
axis(4, labels = FALSE)

par(plt = c(c(margin + width + padding + margin,
              margin + width + padding + margin + width)/fin[1],
            c(margin, margin + height)/fin[2]), new = TRUE)


xlim = range(msg$time)
ylim = c(0, 5)
plot(xlim, ylim, t = "n", xlab = "Date", ylab = "Relative Send Intensity")
axis(3, labels = FALSE,
     at = as.POSIXct(c("1999-01-01", "2000-01-01", "2001-01-01", "2002-01-01")))
axis(4, labels = FALSE)

for (i in seq_along(emp$eid)) {
    eid <- emp$eid[i]
    msg.e <- subset(msg, from == eid, time.secs, drop = TRUE)

    if (length(msg.e) > 1) {
        d <- density(msg.e)
        x <- as.POSIXct(d$x, origin = attr(msg, "time.origin"))
        # y <- 60 * 60 * 24 * 365 * d$n * d$y
        y <- 60 * 60 * 24 * 365 * d$y
        lines(x, y, col = color(q[i]))
    }
}

if (pdfout) dev.off()

