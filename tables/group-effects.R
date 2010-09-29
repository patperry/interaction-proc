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

groups <- c("FJL", "MJL", "FSL", "MSL",
            "FJT", "MJT", "FST", "MST",
            "FJO", "MJO", "FSO", "MSO")
ngroups <- length(groups)

coefs <- matrix(coefs, ngroups, ngroups)
dimnames(coefs) <- list(groups, groups)

coefs.se <- matrix(coefs.se, ngroups, ngroups)
dimnames(coefs.se) <- list(groups, groups)

effects <- exp(coefs)
effects.se <- exp(coefs) * coefs.se  # delta method approximation

cat.table <- function(file = file, append = FALSE, top = TRUE, bottom = TRUE,
                      rows = seq_len(ngroups), cols = seq_len(ngroups)) {

cat(file = file, append = append, sep = "", "
\\begin{tabular}{l", rep("r", 2 * length(cols)), "}")
if (top) {
    cat(file = file, append = TRUE, "\n\\toprule")

    cat(append = TRUE, file = file, sep = "",
        "\n    & \\multicolumn{", 2 * length(cols), "}{c}{\\textbf{Receiver}} \\\\")
    cat(append = TRUE, file = file,
        "\n    \\cmidrule(l){2-", 1 + 2 * length(cols),"}")

    cat(append = TRUE, file = file, "\n\\textbf{Sender}")
} else {
    cat(append = TRUE, file = file, "\n\\phantom{\\textbf{Sender}} &\\\\")
    cat(append = TRUE, file = file, "\n\\\\")    
}

for (j in cols) {
    cat(append = TRUE, file = file, sep = "",
        "\n    & \\multicolumn{2}{c}{\\textnormal{", groups[[j]], "}}")
}
cat(append = TRUE, file = file, " \\\\")

if (top) cat(append = TRUE, file = file, "\n    \\cmidrule(l){1-1}")
for (j in seq_along(cols)) {
    cat(append = TRUE, file = file, sep = "",
        "\n    \\cmidrule(lr){", 2*j, "-", 2*j + 1, "}")
}
# cat(append = TRUE, file = file, " \\\\")

for (i in rows) {
    eff.fmt <- format(round(effects[i,cols], 2), digits = 2)
    eff.se.fmt <- paste("(", format(round(effects.se[i,cols], 2), digits = 2), ")",
                        sep = "")

    cat(append = TRUE, file = file, sep = "",
        "\n    \\textnormal{", groups[[i]], "} & ")
    cat(append = TRUE, file = file, sep = "",
        do.call(paste, c(as.list(rbind(eff.fmt, eff.se.fmt)), sep = " & ")))
    cat(append = TRUE, file = file, sep = "", " \\\\")
}

if (bottom) cat(file = file, append = TRUE, "\n\\bottomrule")
cat(append = TRUE, file = file, "\n\\end{tabular}")
}

cat.table(file = file, append = FALSE, top = TRUE,  bottom = FALSE, cols = 1:6)
cat(file = file, append = TRUE, "\n")
cat.table(file = file, append = TRUE,  top = FALSE, bottom = TRUE,  cols = 7:12)
