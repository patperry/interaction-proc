# tables/employee-summary.R
# -------------------------

source("code/counts.R")

file <- "tables/employee-summary.tex"

f <- GetEmployeeCount(gender = "Female")
m <- GetEmployeeCount(gender = "Male")
j <- GetEmployeeCount(seniority = "Junior")
s <- GetEmployeeCount(seniority = "Senior")
l <- GetEmployeeCount(department = "Legal")
t <- GetEmployeeCount(department = "Trading")
o <- GetEmployeeCount(department = "Other")

cat(file = file, sep = "", "
\\begin{tabular}{lrclrclr}
    \\toprule
    \\multicolumn{2}{c}{\\textbf{Gender}}
    & & \\multicolumn{2}{c}{\\textbf{Seniority}}
    & & \\multicolumn{2}{c}{\\textbf{Department}} \\\\
    \\cmidrule{1-2} \\cmidrule{4-5} \\cmidrule{7-8}
    ", paste("Female", f,  "", "Junior", j, "", "Legal",   l, sep = "&"), "\\\\
    ", paste("Male",   m,  "", "Senior", s, "", "Trading", t, sep = "&"), "\\\\
    ", paste(""    ,   "", "", "",       "",  "", "Other", o, sep = "&"), "\\\\
    \\bottomrule
\\end{tabular}
")
