# tables/homophily.R
# ------------------

source("code/process.R")

emp <- fromJSON("data/enron/employees.json")
fit <- fromJSON("output/fit-dynamic.json")

seniority <- factor(sapply(emp, function(x) x$seniority))
gender <- factor(sapply(emp, function(x) x$gender))
department <- factor(sapply(emp, function(x) x$department))

nobs <- jmat(fit$observed)

jun <- seniority == "Junior"
sen <- !jun

tab <- matrix(c(sum(nobs[jun,jun]), sum(nobs[jun,sen]),
                sum(nobs[sen,jun]), sum(nobs[sen,sen])), 2, byrow=TRUE)
dimnames(tab) <- list(c("J", "S"), c("J", "S"))


iJ <- c(1, 0, 1, 0)
jJ <- c(1, 1, 0, 0)
jS <- c(0, 0, 1, 1)

y <- as.vector(tab)
nJ <- sum(jun)
nS <- sum(sen)
offset <- c(log(nJ - 1), log(nJ), log(nS), log(nS - 1))

model0 <- glm(y ~ iJ + jJ, family=poisson, offset=offset)
model <- glm(y ~ iJ + jJ + iJ * jJ, family=poisson, offset=offset)

