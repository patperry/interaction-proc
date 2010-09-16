# figures/toy-network.R
# ---------------------

require("igraph")
set.seed(0, "Mersenne-Twister")

pdffile <- "figures/toy-network.pdf"
pdfout <- TRUE

width <- 4.15
height <- width

orange <- rgb(253, 141,  60, max = 255)
purple <- rgb(158, 154, 200, max = 255)
colors <- c(orange, purple)


i <- c(0, 0, 0, 1, 2, 2,  3, 4, 4, 5,  5, 6,  6, 7,  9)
j <- c(1, 2, 3, 9, 6, 7, 10, 5, 8, 6, 11, 9, 11, 8, 11)
g <- graph.edgelist(cbind(i, j))

V(g)$color <- colors[c(2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 1, 2)]
V(g)$label <- NA

if (pdfout) pdf(pdffile, width = width, height = height)

par(mar = c(0,0,0,0))
plot(g, layout = layout.fruchterman.reingold, edge.width = 2)

if (pdfout) dev.off()
