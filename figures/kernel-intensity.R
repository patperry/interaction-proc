# figures/kernel-intensity.R
# --------------------------

set.seed(0, "Mersenne-Twister")

source("code/employee.R")
source("code/message.R")

emp <- GetEmployees()
msg <- GetMessages()

pdffile <- "figures/kernel-intensity.pdf"
pdfout <- TRUE

# 'Oranges' scheme from ColorBrewer2, last 3 colors from 4-color scheme
colorRGB <- colorRamp(c(rgb(253, 190, 133, max = 255),
                        rgb(253, 141, 60, max = 255),
                        rgb(217, 71, 1, max = 255)),
                      space = "Lab", bias = 1)
color <- function(q) rgb(colorRGB(q), max = 255)

# purple
color.points <- rgb(158, 154, 200, max = 255)


if (pdfout) {
    width.fig <- 8.3
    height.fig <- width.fig / 2
    pdf(pdffile, width = width.fig, height = height.fig)
}

par(las = 1)
par(mar = c(4, 4, 1, 4) + 0.1)

nmsg <- rep(NA, length(emp$eid))
for (i in seq_along(emp$eid)) {
    eid <- emp$eid[i]
    nmsg[i] <- sum(msg$from == eid)
}

q <- rank(nmsg)/(1 + length(nmsg))


eid <- emp$eid[1]
msg.e <- subset(msg, from == eid, time.secs, drop = TRUE)

if (length(msg.e) > 1) {
    d <- density(msg.e)
    x <- as.POSIXct(d$x, origin = attr(msg, "time.origin"))
    y <- 60 * 60 * 24 * 365 * d$n * d$y
    
    xlim = range(msg$time)
    ylim = range(y)
    
    plot(xlim, ylim, t = "n",
         xlab = "Year",
         ylab = "Send Intensity", axes = FALSE)
    axis(1, labels = c("1999", "2000", "2001", "2002"),
         at = as.POSIXct(c("1999-01-01", "2000-01-01", "2001-01-01",
                           "2002-01-01")))
    axis(3, labels = FALSE,
         at = as.POSIXct(c("1999-01-01", "2000-01-01", "2001-01-01",
                           "2002-01-01")))
    axis(2, labels = TRUE,
         at = c(0, 100, 200, 300))
    axis(4, labels = FALSE,
         at = c(0, 100, 200, 300))
    box()

    lines(x, y, col = color(q[i]))
    points(as.POSIXct(msg.e, origin = attr(msg, "time.origin")),
          jitter(rep(0, length(msg.e)), d$n),
          col = color.points, cex = 0.5)
} else {
    stop("employee must have at least 2 messages")
}

if (pdfout) dev.off()
