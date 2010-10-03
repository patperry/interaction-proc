# tables/deviance.R
# -----------------

file <- "tables/deviance.tex"

sys.source("analysis/dynamic.R",
    dynamic <- new.env(parent = baseenv()))
sys.source("analysis/static.R",
    static <- new.env(parent = baseenv()))
    

dev.null <- round(static$dev.null)
df.null <- static$df.null

df.static <- static$df.effective
dev.static <- round(static$dev.null - static$dev.resid)
df.resid.static <- static$df.null - static$df.effective
dev.resid.static <- round(static$dev.resid)

df.dynamic <- dynamic$df.effective - static$df.effective
dev.dynamic <- round(static$dev.resid - dynamic$dev.resid)
df.resid.dynamic <- dynamic$df.null - dynamic$df.effective
dev.resid.dynamic <- round(dynamic$dev.resid)


cat(file = file, sep="", "
\\begin{tabular}{lrrrr}
    \\toprule
    \\textbf{Term}
        & \\textbf{Df}
        & \\textbf{Deviance}
        & \\textbf{Resid. Df}
        & \\textbf{Resid. Dev} \\\\
    \\midrule
    ", paste("Null", "", "", df.null, dev.null, sep = " & "), " \\\\
    ", paste("Static", df.static, dev.static, df.resid.static, dev.resid.static, sep = " & "), " \\\\
    ", paste("Dynamic", df.dynamic, dev.dynamic, df.resid.dynamic, dev.resid.dynamic, sep = " & "), " \\\\
    \\bottomrule
\\end{tabular}
")
