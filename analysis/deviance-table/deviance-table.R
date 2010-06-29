# deviance-table.R
# ----------------

messageCountWithDupes <- 38388
receiverCount <- 156

deviance.null <- 2 * log(receiverCount - 1) * messageCountWithDupes
df.null <- messageCountWithDupes

source("analysis/iproc-static/iproc.out.R")
staticCount <- length(coefs)

deviance.static <- deviance
df.static <- staticCount - sqrt(staticCount)

source("analysis/iproc-dynamic/iproc.out.R")
deviance.dynamic <- deviance
df.dynamic <- length(send.intervals) + length(recv.intervals)


cat("deviance.null: ", deviance.null, "df:", df.null, "\n")
cat("deviance.static: ", deviance.static, "df:", df.static, "\n")
cat("deviance.dynamic: ", deviance.dynamic, "df:", df.dynamic, "\n")
