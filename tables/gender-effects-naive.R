# tables/gender-effects-naive.R
# -----------------------------

source("code/employee.R")
source("code/message.R")

file <- "tables/gender-effects-naive.tex"

kMaxRecipients <- 10

emps <- GetEmployees()
gender <- emps$gender

msgs <- GetMessages(duplicate = FALSE)

sendCount <- matrix(0, 2, 2)


for (m in seq_len(msgs$count)) {
    if (msgs$length[m] <= kMaxRecipients) {
        s <- msgs$from[m]
        i <- ifelse (gender[s] == "Female", 1, 2)
        for (k in seq_len(msgs$length[m])) {
            r <- msgs$to[msgs$offset[m] + k - 1]
            j <- ifelse (gender[r] == "Female", 1, 2)
            
            sendCount[i,j] <- sendCount[i,j] + 1
        }
    }
}

ff <- sendCount[1,1] / (sendCount[1,1] + sendCount[1,2])
fm <- 1 - ff
f.se <- sqrt(ff * fm / sum(sendCount[1,]))

mf <- sendCount[2,1] / (sendCount[2,1] + sendCount[2,2])
mm <- 1 - mf
m.se <- sqrt(mf * mm / sum(sendCount[2,]))


ff <- format(round(ff, 3), nsmall = 3)
fm <- format(round(fm, 3), nsmall = 3)
f.se <- paste("(", format(round(f.se, 3), nsmall = 3), ")", sep = "")
mf <- format(round(mf, 3), nsmall = 3)
mm <- format(round(mm, 3), nsmall = 3)
m.se <- paste("(", format(round(m.se, 3), nsmall = 3), ")", sep = "")

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
