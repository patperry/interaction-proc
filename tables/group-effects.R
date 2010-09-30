# tables/group-effects.R
# ----------------------

file <- "tables/group-effects.tex"

sys.source("analysis/dynamic.R",
    dynamic <- new.env(parent = baseenv()))
coefs <- with(dynamic,
              coefs[seq.int(from = ( length(intervals.send)
                                   + length(intervals.recv)
                                   + 1),
                            to = length(coefs))])
coefs.se <- with(dynamic,
                 coefs.se[seq.int(from = ( length(intervals.send)
                                         + length(intervals.recv)
                                         + 1),
                                  to = length(coefs))])

groups <- c("FSL", "MSL", "FST", "MST", "FSO", "MSO",
            "FJL", "MJL", "FJT", "MJT", "FJO", "MJO")
ngroups <- length(groups)

coefs <- matrix(coefs, ngroups, ngroups)
dimnames(coefs) <- list(groups, groups)

coefs.se <- matrix(coefs.se, ngroups, ngroups)
dimnames(coefs.se) <- list(groups, groups)

effects <- exp(coefs)
effects.se <- exp(coefs) * coefs.se  # delta method approximation

cat.table <- function(file = file, append = FALSE, top = TRUE, bottom = TRUE,
                      sends = seq_len(ngroups), recvs = seq_len(ngroups)) {

cat(file = file, append = append, sep = "", "
\\begin{tabular}{l", rep("l@{\\,\\,\\,}r", length(sends)), "}")
if (top) {
    cat(file = file, append = TRUE, "\n\\toprule")

    cat(append = TRUE, file = file, sep = "",
        "\n    & \\multicolumn{", 2 * length(sends), "}{c}{\\textbf{Sender}} \\\\")
    cat(append = TRUE, file = file,
        "\n    \\cmidrule(lr){2-", 1 + 2 * length(sends),"}")

    cat(append = TRUE, file = file, "\n\\textbf{Receiver}")
} else {
    cat(append = TRUE, file = file, "\n\\phantom{\\textbf{Receiver}} &\\\\")
    #cat(append = TRUE, file = file, "\n\\\\")    
}

for (i in sends) {
    cat(append = TRUE, file = file, sep = "",
        "\n    & \\multicolumn{2}{c}{\\textnormal{", groups[[i]], "}}")
}
cat(append = TRUE, file = file, " \\\\")

#cat(append = TRUE, file = file, "\n\\midrule")
if (top) cat(append = TRUE, file = file, "\n    \\cmidrule(lr){1-1}")
for (i in seq_along(sends)) {
    cat(append = TRUE, file = file, sep = "",
        "\n    \\cmidrule(lr){", 2*i, "-", 2*i + 1, "}")
}

for (j in recvs) {
    eff.fmt <- format(round(effects[sends,j], 2), digits = 3)
    for (i in seq_along(sends)) {
        if (effects[sends[i],j] == max(effects[sends[i],]))
            eff.fmt[i] <- paste("\\textbf{", eff.fmt[i], "}", sep = "")
    }
    eff.se.fmt <- paste("(",
                        format(round(effects.se[sends,j], 2), digits = 2),
                        ")",
                        sep = "")

    cat(append = TRUE, file = file, sep = "",
        "\n    \\textnormal{", groups[[j]], "} & ")
    cat(append = TRUE, file = file, sep = "",
        do.call(paste, c(as.list(rbind(eff.fmt, eff.se.fmt)), sep = " & ")))
    cat(append = TRUE, file = file, sep = "", " \\\\")
}

if (bottom) cat(file = file, append = TRUE, "\n\\bottomrule")
cat(append = TRUE, file = file, "\n\\end{tabular}")
}

cat.table(file = file, append = FALSE, top = TRUE,  bottom = FALSE, sends = 1:6)
cat(file = file, append = TRUE, "\n")
cat.table(file = file, append = TRUE,  top = FALSE, bottom = TRUE,  sends = 7:12)
