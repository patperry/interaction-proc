# tables/gender-send-counts.R
# ---------------------------

source("code/counts.R")

file <- "tables/gender-send-counts.tex"

ff <- GetRecipientCount(from.gender = "Female", to.gender = "Female")
mf <- GetRecipientCount(from.gender = "Male",   to.gender = "Female")
fm <- GetRecipientCount(from.gender = "Female", to.gender = "Male")
mm <- GetRecipientCount(from.gender = "Male",   to.gender = "Male")

cat(file = file, sep="", "
\\begin{tabular}{lrr}
    \\toprule
    & \\multicolumn{2}{c}{\\textbf{To}} \\\\
    \\cmidrule(l){2-3}
    \\textbf{From} & Female & Male  \\\\
    \\midrule
    ", paste("Female", ff, fm, sep = "&"), " \\\\
    ", paste("Male",   mf, mm, sep = "&"), " \\\\
    \\bottomrule
\\end{tabular}
")
