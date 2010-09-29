# figures/recipient-counts.R
# --------------------------

pdffile <- "figures/recipient-counts.pdf"
pdfout <- TRUE

source("code/message.R")

msg <- GetMessages(duplicate = FALSE)

# purple
color.points <- rgb(158, 154, 200, max = 255)

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

par(las = 1)

l <- msg$length
q <- rank(l, ties.method = "first")/(1 + length(l))

plot(q, log2(l), col = color.points,
     xlab = "Fraction of Messages",
     ylab = expression("Log"[2]*" Number of Recipients"))
axis(3, labels = FALSE)
axis(4, labels = FALSE)
# axis(4, labels = 2^(0:6), at=0:6)

if (pdfout) dev.off()
