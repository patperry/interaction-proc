# figures/ergm.R
# --------------

source("code/process.R")
require("ergm")

emp <- fromJSON("data/enron/employees.json")
fit <- fromJSON("output/fit-dynamic.json")
nobs <- jmat(fit$observed)

department <- factor(sapply(emp, function(x) x$department))
seniority <- factor(sapply(emp, function(x) x$seniority))
gender <- factor(sapply(emp, function(x) x$gender))

v1 <- rep(1, nrow(nobs))
vL <- as.numeric(department == "Legal")
vT <- as.numeric(department == "Trading")
vJ <- as.numeric(seniority == "Junior")
vF <- as.numeric(gender == "Female")
vLJ <- vL * vJ
vTJ <- vT * vJ
vLF <- vL * vF
vTF <- vT * vF
vJF <- vJ * vF

vertex.attr = list(vL, vT, vJ, vF)
vertex.attrnames = list("L", "T", "J", "F")


net10 <- network(nobs >= 10,
                 vertex.attr=vertex.attr,
                 vertex.attrnames=vertex.attrnames)
model <- ergm(net10 ~ edges
                    + nodemix("D") + nodemix("S") + nodemix("G")
                    + mutual
                    + ctriple + ttriple,
              burnin=100000,
              MCMCsamplesize=100000)

model <- ergm(net10 ~ sender
                    + edgecov(v1 %*% t(vL))
                    + edgecov(vL %*% t(vL))
                    + edgecov(vT %*% t(vL))
                    + edgecov(vJ %*% t(vL))
                    + edgecov(vF %*% t(vL))
                    + edgecov(v1 %*% t(vT))
                    + edgecov(vL %*% t(vT))
                    + edgecov(vT %*% t(vT))
                    + edgecov(vJ %*% t(vT))
                    + edgecov(vF %*% t(vT))
                    + edgecov(v1 %*% t(vJ))
                    + edgecov(vL %*% t(vJ))
                    + edgecov(vT %*% t(vJ))
                    + edgecov(vJ %*% t(vJ))
                    + edgecov(vF %*% t(vJ))
                    + edgecov(v1 %*% t(vF))
                    + edgecov(vL %*% t(vF))
                    + edgecov(vT %*% t(vF))
                    + edgecov(vJ %*% t(vF))
                    + edgecov(vF %*% t(vF))
                    + mutual
                    + ctriple + ttriple,
             burnin=1000000,
             MCMCsamplesize=100000)

model1 <- ergm(net10 ~ sender
+ edgecov(v1 %*% t(vL)) + edgecov(vL %*% t(vL)) + edgecov(vT %*% t(vL)) + edgecov(vJ %*% t(vL)) + edgecov(vF %*% t(vL))
    + edgecov(vLJ %*% t(vL)) + edgecov(vTJ %*% t(vL)) + edgecov(vLF %*% t(vL)) + edgecov(vTF %*% t(vL)) + edgecov(vJF %*% t(vL))
+ edgecov(v1 %*% t(vT)) + edgecov(vL %*% t(vT)) + edgecov(vT %*% t(vT)) + edgecov(vJ %*% t(vT)) + edgecov(vF %*% t(vT))
    + edgecov(vLJ %*% t(vT)) + edgecov(vTJ %*% t(vT)) + edgecov(vLF %*% t(vT)) + edgecov(vTF %*% t(vT)) + edgecov(vJF %*% t(vT))
+ edgecov(v1 %*% t(vJ)) + edgecov(vL %*% t(vJ)) + edgecov(vT %*% t(vJ)) + edgecov(vJ %*% t(vJ)) + edgecov(vF %*% t(vJ))
    + edgecov(vLJ %*% t(vJ)) + edgecov(vTJ %*% t(vJ)) + edgecov(vLF %*% t(vJ)) + edgecov(vTF %*% t(vJ)) + edgecov(vJF %*% t(vJ))
+ edgecov(v1 %*% t(vF)) + edgecov(vL %*% t(vF)) + edgecov(vT %*% t(vF)) + edgecov(vJ %*% t(vF)) + edgecov(vF %*% t(vF))
    + edgecov(vLJ %*% t(vF)) + edgecov(vTJ %*% t(vF)) + edgecov(vLF %*% t(vF)) + edgecov(vTF %*% t(vF)) + edgecov(vJF %*% t(vF))
+ edgecov(v1 %*% t(vLJ)) + edgecov(vL %*% t(vLJ)) + edgecov(vT %*% t(vLJ)) + edgecov(vJ %*% t(vLJ)) + edgecov(vF %*% t(vLJ))
    + edgecov(vLJ %*% t(vLJ)) + edgecov(vTJ %*% t(vLJ)) + edgecov(vLF %*% t(vLJ)) + edgecov(vTF %*% t(vLJ)) + edgecov(vJF %*% t(vLJ))
+ edgecov(v1 %*% t(vTJ)) + edgecov(vL %*% t(vTJ)) + edgecov(vT %*% t(vTJ)) + edgecov(vJ %*% t(vTJ)) + edgecov(vF %*% t(vTJ))
    + edgecov(vLJ %*% t(vTJ)) + edgecov(vTJ %*% t(vTJ)) + edgecov(vLF %*% t(vTJ)) + edgecov(vTF %*% t(vTJ)) + edgecov(vJF %*% t(vTJ))
+ edgecov(v1 %*% t(vLF)) + edgecov(vL %*% t(vLF)) + edgecov(vT %*% t(vLF)) + edgecov(vJ %*% t(vLF)) + edgecov(vF %*% t(vLF))
    + edgecov(vLJ %*% t(vLF)) + edgecov(vTJ %*% t(vLF)) + edgecov(vLF %*% t(vLF)) + edgecov(vTF %*% t(vLF)) + edgecov(vJF %*% t(vLF))
+ edgecov(v1 %*% t(vTF)) + edgecov(vL %*% t(vTF)) + edgecov(vT %*% t(vTF)) + edgecov(vJ %*% t(vTF)) + edgecov(vF %*% t(vTF))
    + edgecov(vLJ %*% t(vTF)) + edgecov(vTJ %*% t(vTF)) + edgecov(vLF %*% t(vTF)) + edgecov(vTF %*% t(vTF)) + edgecov(vJF %*% t(vTF))
+ edgecov(v1 %*% t(vJF)) + edgecov(vL %*% t(vJF)) + edgecov(vT %*% t(vJF)) + edgecov(vJ %*% t(vJF)) + edgecov(vF %*% t(vJF))
    + edgecov(vLJ %*% t(vJF)) + edgecov(vTJ %*% t(vJF)) + edgecov(vLF %*% t(vJF)) + edgecov(vTF %*% t(vJF)) + edgecov(vJF %*% t(vJF))
                     + mutual
                     + ctriple + ttriple) 


sm <- summary(model)
ix.stat <- match("edgecov.v1 %*% t(vL)", rownames(sm$coefs)) + 0:19

coef.s <- matrix(sm$coefs[ix.stat, "Estimate"], 5, 4)
colnames(coef.s) <- c("L", "T", "J", "F")
rownames(coef.s) <- c("1", colnames(coef.s))

se.s <- matrix(sm$coefs[ix.stat, "Std. Error"], 5, 4)
dimnames(se.s) <- dimnames(coef.s)

ix.dyn <- match("mutual", rownames(sm$coefs)) + 0:2
coef.d <- sm$coefs[ix.dyn, "Estimate"]
names(coef.d) <- rownames(sm$coefs)[ix.dyn]
se.d <- sm$coefs[ix.dyn, "Std. Error"]
names(se.d) <- names(coef.d)

