# tables/gender-effects.R
# -----------------------

source("code/counts.R")
source("code/boot.R")

file.static <- "tables/gender-effects-static.tex"
file.dynamic <- "tables/gender-effects-dynamic.tex"
file.dynamic.bc <- "tables/gender-effects-dynamic-bc.tex"


groups <- c("FLJ", "FLS", "FTJ", "FTS", "FOJ", "FOS",
            "MLJ", "MLS", "MTJ", "MTS", "MOJ", "MOS")
ngroups <- length(groups)
is.female <- sapply(groups, function(x) substr(x, 1, 1) == "F")

emp.counts <- rep(NA, ngroups)
send.counts <- rep(NA, ngroups)
for (i in seq_along(groups)) {
    g <- groups[i]
    emp.counts[i] <- GetEmployeeCount(gender = substr(g, 1, 1),
                                      department = substr(g, 2, 2),
                                      seniority = substr(g, 3, 3))
    send.counts[i] <- GetRecipientCount(from.gender = substr(g, 1, 1),
                                        from.department = substr(g, 2, 2),
                                        from.seniority = substr(g, 3, 3))
}

GetCoefs <- function(filename = "analysis/dynamic.R")
{
    sys.source(filename,
        env <- new.env(parent = baseenv()))
    
    coefs <- with(env,
                  coefs[seq.int(from = ( length(intervals.send)
                                       + length(intervals.recv)
                                       + 1),
                                to = length(coefs))])
    coefs.cov <- with(env,
                      coefs.cov[seq.int(from = ( length(intervals.send)
                                               + length(intervals.recv)
                                               + 1),
                                    to = length(coefs)),
                                seq.int(from = ( length(intervals.send)
                                               + length(intervals.recv)
                                               + 1),
                                        to = length(coefs)),
                                drop = FALSE])

    list(coefs = coefs, coefs.cov = coefs.cov)
}

GetGroupGenderProbs <- function(group.id, coefs, coefs.cov)
{
    # offset the coefficients by the number of recipipients in each group
    g <- groups[group.id]
    offset <- log(emp.counts)
    offset[group.id] <- log(emp.counts[group.id] - 1)  # no self-loops
    
    # get the coeficients and their covariance
    ix.g <- group.id + seq.int(from = 0, length.out = ngroups) * ngroups
    coefs.g <- coefs[ix.g] + offset
    coefs.cov.g <- coefs.cov[ix.g,ix.g]
    
    # get the relative rates to each group
    rates.g <- exp(coefs.g)
    rates.cov.g <- (diag(rates.g, ngroups)
                    %*% coefs.cov.g
                    %*% diag(rates.g, ngroups))  # delta method
                    
    # get marginal rates to each gender
    fm <- rbind(is.female, !is.female)
    rates.fm <- fm %*% rates.g
    rates.fm.cov <- fm %*% rates.cov.g %*% t(fm)
    
    # normalize the rates
    probs.fm <- as.numeric(rates.fm / sum(rates.fm))
    h <- c(rates.fm[2], -rates.fm[1]) / (sum(rates.fm))^2
    probs.fm.var <- as.numeric(rbind(h) %*% rates.fm.cov %*% cbind(h))
    names(probs.fm) <- c("Female", "Male")
    
    list(probs = probs.fm, var = probs.fm.var)
}

GetGroupGenderProbCovEntry <- function(group.id1, group.id2, coefs, coefs.cov)
{
    offset1 <- log(emp.counts)
    offset1[group.id1] <- log(emp.counts[group.id1] - 1)  # no self-loops
    offset2 <- log(emp.counts)
    offset2[group.id2] <- log(emp.counts[group.id2] - 1)  # no self-loops

    ix.g1 <- group.id1 + seq.int(from = 0, length.out = ngroups) * ngroups
    coefs.g1 <- coefs[ix.g1] + offset1
    ix.g2 <- group.id2 + seq.int(from = 0, length.out = ngroups) * ngroups
    coefs.g2 <- coefs[ix.g2] + offset2
    
    coefs.g <- c(coefs.g1, coefs.g2)
    coefs.cov.g <- coefs.cov[c(ix.g1, ix.g2), c(ix.g1, ix.g2)]

    # get the relative rates to each group
    rates.g <- exp(coefs.g)
    rates.cov.g <- (diag(rates.g, 2*ngroups)
                    %*% coefs.cov.g
                    %*% diag(rates.g, 2*ngroups))  # delta method

    # get marginal rates to each gender
    fm <- rbind(is.female, !is.female)
    fm2 <- rbind(cbind(fm, matrix(0, nrow(fm), ncol(fm))),
                 cbind(matrix(0, nrow(fm), ncol(fm)), fm))
                
    rates.fm <- fm2 %*% rates.g
    rates.fm.cov <- fm2 %*% rates.cov.g %*% t(fm2)
    
    # normalize the rates
    probs.fm1 <- as.numeric(rates.fm[1:2] / sum(rates.fm[1:2]))
    probs.fm2 <- as.numeric(rates.fm[3:4] / sum(rates.fm[3:4]))
    h1 <- c(rates.fm[2], -rates.fm[1]) / (sum(rates.fm[1:2]))^2
    h2 <- c(rates.fm[4], -rates.fm[3]) / (sum(rates.fm[3:4]))^2
    probs.fm.cov <- as.numeric(rbind(h1) %*% rates.fm.cov[1:2,3:4] %*% cbind(h2))
}

GetGroupGenderProbCov <- function(group.ids, coefs, coefs.cov)
{
    sigma <- matrix(NA, length(group.ids), length(group.ids))
    for (i in seq_along(group.ids)) {
        for (j in seq_along(group.ids)) {
            g1 <- group.ids[i]
            g2 <- group.ids[j]
            sigma[i,j] <- GetGroupGenderProbCovEntry(g1, g2, coefs, coefs.cov)
        }
    }
    
    sigma
}

GetGenderProbs <- function(group.ids, coefs, coefs.cov)
{
    wts <- send.counts[group.ids]
    wts <- wts / sum(wts)
    
    probs <- matrix(NA, length(group.ids), 2)
    vars <- rep(NA, length(group.ids))
    
    
    for (i in seq_along(group.ids)) {
        bias.g <- GetGroupGenderProbs(group.ids[i], coefs, coefs.cov)
        probs[i,] <- bias.g$probs
    }
    sigma <- GetGroupGenderProbCov(group.ids, coefs, coefs.cov)
    
    probs.wt <- as.numeric(rbind(wts) %*% probs)
    names(probs.wt) <- c("Female", "Male")
    var.wt <- as.numeric(rbind(wts) %*% sigma %*% cbind(wts))
    
    list(probs = probs.wt, se = sqrt(var.wt))
}

PrintGenderTable <- function(file, coefs, coefs.cov)
{
    f <- GetGenderProbs(which(is.female), coefs, coefs.cov)
    ff <- format(round(f$probs[1], 3), nsmall = 3)
    fm <- format(round(f$probs[2], 3), nsmall = 3)
    f.se <- paste("(", format(round(f$se, 3), nsmall = 3), ")", sep = "")
    
    m <- GetGenderProbs(which(!is.female), coefs, coefs.cov)
    mf <- format(round(m$probs[1], 3), nsmall = 3)
    mm <- format(round(m$probs[2], 3), nsmall = 3)
    m.se <- paste("(", format(round(m$se, 3), nsmall = 3), ")", sep = "")
    
    cat(file = file, 
        sep="", "
\\begin{tabular}{lr@{ }r@{}cr@{ }r}
    \\toprule
    & \\multicolumn{5}{c}{\\textbf{To}} \\\\
    \\cmidrule(l){2-6}
    \\textbf{From} & \\multicolumn{2}{c}{Female} && \\multicolumn{2}{c}{Male} \\\\
    \\midrule
    ", paste("Female", ff, f.se, "", fm, f.se, sep = " & "), " \\\\
    ", paste("Male",   mf, m.se, "", mm, m.se, sep = " & "), " \\\\
    \\bottomrule
\\end{tabular}
")
}

dynamic <- GetCoefs("analysis/dynamic.R")
PrintGenderTable(file.dynamic, dynamic$coefs, dynamic$coefs.cov)

static <- GetCoefs("analysis/static.R")
PrintGenderTable(file.static, static$coefs, static$coefs.cov)

bias <- GetBias()
bias <- bias$est[seq.int(from = length(bias$est) - ngroups^2 + 1,
                         to = length(bias$est))]
PrintGenderTable(file.dynamic.bc, dynamic$coefs - bias, dynamic$coefs.cov)
