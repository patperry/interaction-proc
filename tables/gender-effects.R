# tables/gender-effects.R
# -----------------------

source("code/counts.R")

file.static <- "tables/gender-effects-static.tex"
file.dynamic <- "tables/gender-effects-dynamic.tex"


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
    h <- c(rates.fm[2], rates.fm[-1]) / (sum(rates.fm))^2
    probs.fm.var <- as.numeric(rbind(h) %*% rates.fm.cov %*% cbind(h))
    names(probs.fm) <- c("Female", "Male")
    
    list(probs = probs.fm, se = sqrt(probs.fm.var))
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
        vars[i] <- (bias.g$se)^2
    }
    
    probs.wt <- as.numeric(rbind(wts) %*% probs)
    names(probs.wt) <- c("Female", "Male")
    var.wt <- sum(wts^2 * vars)
    
    list(probs = probs.wt, se = sqrt(var.wt))
}

PrintGenderTable <- function(file, coefs, coefs.cov)
{
    f <- GetGenderProbs(which(is.female), coefs, coefs.cov)
    ff <- format(round(f$probs[1], 2), nsmall = 2)
    fm <- format(round(f$probs[2], 2), nsmall = 2)
    f.se <- paste("(", format(round(f$se, 2), nsmall = 2), ")", sep = "")
    
    m <- GetGenderProbs(which(!is.female), coefs, coefs.cov)
    mf <- format(round(m$probs[1], 2), nsmall = 2)
    mm <- format(round(m$probs[2], 2), nsmall = 2)
    m.se <- paste("(", format(round(m$se, 2), nsmall = 2), ")", sep = "")
    
    cat(file = file, 
        sep="", "
\\begin{tabular}{lr@{ }r@{\\,\\,}cr@{ }r}
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
