# tables/employee-summary.R
# -------------------------

source("code/counts.R")

file <- "tables/employee-summary.tex"

f <- GetEmployeeCount(gender = "Female")
m <- GetEmployeeCount(gender = "Male")
l <- GetEmployeeCount(department = "Legal")
t <- GetEmployeeCount(department = "Trading")
o <- GetEmployeeCount(department = "Other")
j <- GetEmployeeCount(seniority = "Junior")
s <- GetEmployeeCount(seniority = "Senior")

cat(file = file, sep = "", "
\\begin{tabular}{lrclrclr}
    \\toprule
    \\multicolumn{2}{c}{\\textbf{Gender}}
    & & \\multicolumn{2}{c}{\\textbf{Department}}
    & & \\multicolumn{2}{c}{\\textbf{Seniority}} \\\\
    \\cmidrule{1-2} \\cmidrule{4-5} \\cmidrule{7-8}
    ", paste("Female",  f, "", "Legal"  , l, "", "Junior",  j, sep = "&"), "\\\\
    ", paste("Male"  ,  m, "", "Trading", t, "", "Senior",  s, sep = "&"), "\\\\
    ", paste(""      , "", "", "Other"  , o, "", ""      , "", sep = "&"), "\\\\
    \\bottomrule
\\end{tabular}
")
