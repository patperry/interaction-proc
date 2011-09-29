# code/agent.R


adjmatrix <- function(messages, nsender, nreceiver = nsender) {
    x <- matrix(0, nsender, nreceiver)
    for (i in seq_len(nrow(messages))) {
        s <- messages$sender.id[[i]]
        r <- messages$receiver.id[[i]]
        x[s,r] <- x[s,r] + 1
    }
    x
}

write.pajek <- function(x, file = "", weighted = TRUE) {
    n <- nrow(x)

    cat("*vertices ", n, "\n", file = file, sep = '')
    cat("*arcs\n", file = file)
    for (i in which(x > 0)) {
        s <- ((i - 1) %%  n) + 1
        r <- ((i - 1) %/% n) + 1
        e <- ifelse(weighted, x[i], 1)
        cat(s, r, e, file = file, sep = ' ')
        cat("\n", file = file)
    }
}


require(iproc)
data(enron)

# write employee covariates
mm <- model.matrix(~ gender + department + seniority, enron$employees)[,-1]
for (i in seq_len(ncol(mm))) {
    fn <- paste("data/enron/enron-", colnames(mm)[[i]], ".dat", sep='')
    write.table(mm[,i], fn, row.names=FALSE, col.names=FALSE)
}


# write message networks
n <- nrow(enron$employees)
dt <- as.difftime(1, units="weeks")

t0 <- as.POSIXlt(min(enron$messages$time))
t0 <- t0 - as.difftime(t0$wday, units="days")
t0 <- as.POSIXct(format(t0, "%Y-%m-%d"), tz = attr(t0, "tzone"))

tmax <- max(enron$messages$time) + as.difftime(1, units="secs")
kmax <- ceiling(as.numeric(tmax - t0 + as.difftime(1, units="secs"), units="weeks"))

fn.fmt <- paste("data/enron/enron-w%0", ceiling(log10(kmax)), ".0f.net", sep='')

for (k in seq_len(kmax)) {
    msgs <- subset(enron$messages, t0 + (k - 1) * dt < time & time < t0 + k * dt,
                   select = c(sender.id, receiver.id))
    x <- adjmatrix(msgs, n)

    fn <- sprintf(fn.fmt, k)
    f <- file(fn, "w")
    write.pajek(x, file = f)
    close(f)
}


require(igraph)
require(RSiena)


# read in actor covariates
files <- list.files("data/enron", pattern="enron-.*[.]dat")
vars <- gsub("(^enron-|.dat$)", "", files)
data <- as.list(rep(NA, length(vars)))
for (i in seq_along(vars)) {
    data[[i]] <- read.table(paste("data/enron", files[[i]], sep='/'), col.names=c(vars[[i]]))
}
data <- do.call(data.frame, data)

covars <- list()
for (i in seq_len(ncol(data))) {
    covars <- c(list(coCovar(data[,i])), covars)
}
names(covars) <- colnames(data)


# read in message networks
files <- list.files("data/enron", pattern="enron-w[[:digit:]]+[.]net")
netarray <- as.list(rep(NA, length(files)))

for (i in seq_along(files)) {
    net <- read.graph(paste("data/enron", files[[i]], sep='/'), format="pajek")
    x <- get.adjacency(net, sparse = TRUE)
    netarray[[i]] <- x
}
nets <- sienaNet(netarray[153:157])

#n <- nrow(x)
#netarray <- array(c(netarray, recursive=TRUE), dim=c(n, n, length(netarray)))
# nets <- sienaNet(netarray[,,26:183])


args <- c(list(messages = nets), covars)
mydata <- do.call(sienaDataCreate, args)

# specify effects
myeff <- getEffects(mydata)

# fit model
mymodel <- sienaModelCreate(useStdInits = FALSE, seed=1337L)
ans1 <- siena07(mymodel, data = mydata, effects = myeff, batch = FALSE, verbose = TRUE)



source("code/message.R")
require(RSiena)
require(RJSONIO)
require(Matrix)

msgs <- GetMessages()
n <- length(emp) 

emp <- fromJSON("data/enron/employees.json")
department <- factor(sapply(emp, function(x) x$department))
seniority <- factor(sapply(emp, function(x) x$seniority))
gender <- factor(sapply(emp, function(x) x$gender))

v1 <- rep(1, length(emp))
vL <- as.numeric(department == "Legal")
vT <- as.numeric(department == "Trading")
vJ <- as.numeric(seniority == "Junior")
vF <- as.numeric(gender == "Female")
vLJ <- vL * vJ
vTJ <- vT * vJ
vLF <- vL * vF
vTF <- vT * vF
vJF <- vJ * vF


jacard <- function(x1, x2, k=10) {
    n11 <- sum((x1 >= k) * (x2 >= k))
    n01 <- sum((x1 < k) * (x2 >= k))
    n10 <- sum((x1 >= k) * (x2 < k))
    n11/(n11 + n01 + n10)
}

window <- function(msgs, n, lo, hi) {
    lo <- as.POSIXct(lo)
    hi <- as.POSIXct(hi)
    ix <- lo <= msgs$time & msgs$time < hi
    as.matrix(spMatrix(n, n, msgs$from[ix], msgs$to[ix], rep(1, nrow(msgs[ix,]))))
}

x1 <- window(msgs, n, "2001-01-01", "2001-05-01")
x2 <- window(msgs, n, "2001-05-01", "2001-09-01")
x3 <- window(msgs, n, "2001-09-01", "2002-01-01")

mynet1 <- sienaNet(0 + (array(c(x1, x2, x3), dim=c(dim(x1), 3)) >= 1))
mydata <- sienaDataCreate(mynet1,
                          coDyadCovar(v1 %*% t(vL)),
                          coDyadCovar(vL %*% t(vL)),
                          coDyadCovar(vT %*% t(vL)),
                          coDyadCovar(vJ %*% t(vL)),
                          coDyadCovar(vF %*% t(vL)),
                          coDyadCovar(v1 %*% t(vT)),
                          coDyadCovar(vL %*% t(vT)),
                          coDyadCovar(vT %*% t(vT)),
                          coDyadCovar(vJ %*% t(vT)),
                          coDyadCovar(vF %*% t(vT)),
                          coDyadCovar(v1 %*% t(vJ)),
                          coDyadCovar(vL %*% t(vJ)),
                          coDyadCovar(vT %*% t(vJ)),
                          coDyadCovar(vJ %*% t(vJ)),
                          coDyadCovar(vF %*% t(vJ)),
                          coDyadCovar(v1 %*% t(vF)),
                          coDyadCovar(vL %*% t(vF)),
                          coDyadCovar(vT %*% t(vF)),
                          coDyadCovar(vJ %*% t(vF)),
                          coDyadCovar(vF %*% t(vF)))
myeff <- getEffects(mydata)

myeff[substr(myeff$effectName, 1, nchar('coDyadCovar')) == 'coDyadCovar'
      & nchar(myeff$effectName) == nchar('coDyadCovar(v1 %*% t(vL))')
      & myeff$type == 'eval', 'include'] <- TRUE
myeff[myeff$effectName == 'outdegree (density)' & myeff$type == 'endow', 'include'] <- TRUE
myeff[myeff$effectName=='reciprocity' & myeff$type == 'eval', 'include'] <- TRUE
myeff[myeff$effectName=='transitive triplets' & myeff$type == 'eval', 'include'] <- TRUE
myeff[myeff$effectName=='3-cycles' & myeff$type == 'eval', 'include'] <- TRUE

mymodel <- sienaModelCreate(useStdInits = FALSE, projname = 'enron')
system.time(ans1 <- siena07(mymodel, data=mydata, effects=myeff, batch=FALSE, verbose=FALSE))





