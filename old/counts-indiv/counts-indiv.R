# counts-indiv.R
# --------------

require("RSQLite")
require("igrpah")

source("analysis/counts-indiv/iproc.out.R")

kDbDriver <- dbDriver("SQLite")
kDbName <- "data/enron/enron.db"
kEmployeeCount <- 156

sql <- paste("
        SELECT
            M.from_eid AS from_eid,
            R.to_eid AS to_eid,
            COUNT(*) AS msg_count
        FROM
            Recipient R,
            Message M
        WHERE
            R.mid = M.mid
        GROUP BY
            M.from_eid,
            R.to_eid
    ")
conn <- dbConnect(kDbDriver, dbname = kDbName)
tryCatch({
    rows <- dbGetQuery(conn, sql)
}, finally = {
    dbDisconnect(conn)
})

actual <- matrix(0, kEmployeeCount, kEmployeeCount)
actual[rows$from_eid + kEmployeeCount * (rows$to_eid - 1)] <- rows$msg_count

pearson <- (actual - expected) / sqrt(expected)
pearson[expected == 0] <- 0

raw <- (actual - expected)
plot(svd(raw)$v[,1], svd(raw)$u[,1], t="n")
text(svd(raw)$v[,1], svd(raw)$u[,1], seq_len(kEmployeeCount))

plot(svd(raw)$d^2)


pink <- rgb(180, 44, 113, max = 255)
green <- rgb(50, 113, 40, max = 255)
pdf("analysis/counts-indiv/pearson.pdf")
plot(as.numeric(pearson), col = green, cex=0.5, xlab="", ylab="", axes = FALSE)
axis(1, at = 1 + kEmployeeCount * 25* 1:5, labels = 25 * (1:5))
axis(2)
axis(3, at = 1 + kEmployeeCount * 25* 1:5, labels = FALSE)
axis(4, labels = FALSE)
box()
dev.off()


g <- graph.adjacency(pearson > qnorm(0.99))
set.seed(0, "Mersenne-Twister")
pdf("analysis/counts-indiv/resid-graph.pdf")
plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 2, vertex.label = NA,
     vertex.color = green, vertex.frame.color = green,
     edge.arrow.size=0.4, edge.color = "darkgray")
dev.off()

d <- svd(pearson)$d[1]
u <- svd(pearson)$u[,1]
v <- svd(pearson)$v[,1]
x <- sqrt(d) * v
y <- sqrt(d) * u


pdf("analysis/counts-indiv/scree.pdf")
plot(svd(pearson)$d^2 / sum(svd(pearson)$d^2), col = pink,
     xlab = "", ylab = "")
axis(3, label=FALSE)
axis(4, label=FALSE)
dev.off()



pdf("analysis/counts-indiv/latent.pdf")
plot(x, y, t="n", xlab="", ylab="")
text(x, y, seq_len(kEmployeeCount), col = green)
axis(3, label=FALSE)
axis(4, label=FALSE)
dev.off()


