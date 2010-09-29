# counts.R
# --------

require("RSQLite")

kDbDriver <- dbDriver("SQLite")
kDbName <- "data/enron/enron.db"

kGender <- factor(c("Female", "Male"))
kSeniority <- factor(c("Junior", "Senior"))
kDepartment <- factor(c("Legal", "Trading", "Other"))

kGenderCount <- length(kGender)
kSeniorityCount <- length(kSeniority)
kDepartmentCount <- length(kDepartment)

kEmployee <- (function() {
    sql <- paste("
        SELECT
            *
        FROM
            Employee
    ")
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        employee <- dbGetQuery(conn, sql)
    }, finally = {
        dbDisconnect(conn)
    })
    
    employee$gender <- factor(employee$gender, kGender)
    employee$seniority <- factor(employee$seniority, kSeniority)
    employee$department <- factor(employee$department, kDepartment)
    employee
})()

kEmployeeCount <- nrow(kEmployee)

GetEmployeeCount <- function(gender = kGender,
                             seniority = kSeniority,
                             department = kDepartment)
{
    InSet <- function(set, domain) {
        set <- lapply(as.character(set), match.arg, domain)
        paste("IN('",
              do.call(paste, c(set, sep="', '")),
              "')",
              sep="")
    }
    
    sql <- paste("
        SELECT
            COUNT(*) AS employee_count
        FROM
            Employee E
        WHERE
            E.gender", InSet(gender, kGender),"
            AND E.seniority", InSet(seniority, kSeniority),"
            AND E.department", InSet(department, kDepartment),"
    ")
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        rows <- dbGetQuery(conn, sql)
    }, finally = {
        dbDisconnect(conn)
    })
    
    count <- rows$employee_count
    count
}

kEmployeeCount <- GetEmployeeCount()


GetRecipientCount <- function(from.gender = kGender,
                              from.seniority = kSeniority,
                              from.department = kDepartment,
                              to.gender = kGender,
                              to.seniority = kSeniority,
                              to.department = kDepartment)
{
    InSet <- function(set, domain) {
        set <- lapply(as.character(set), match.arg, domain)
        paste("IN('",
              do.call(paste, c(set, sep="', '")),
              "')",
              sep="")
    }
    
    sql <- paste("
        SELECT
            COUNT(*) AS recipient_count
        FROM
            Message M,
            Recipient R,
            Employee F,
            Employee T
        WHERE
            M.mid = R.mid
            AND M.from_eid = F.eid
            AND R.to_eid = T.eid
            AND F.gender", InSet(from.gender, kGender),"
            AND F.seniority", InSet(from.seniority, kSeniority),"
            AND F.department", InSet(from.department, kDepartment),"
            AND T.gender", InSet(to.gender, kGender),"
            AND T.seniority", InSet(to.seniority, kSeniority),"
            AND T.department", InSet(to.department, kDepartment),"
    ")
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        rows <- dbGetQuery(conn, sql)
    }, finally = {
        dbDisconnect(conn)
    })
    
    count <- rows$recipient_count
    count
}


GetSendCounts <- function(from.eid)
{
    sql <- paste("
        SELECT
            R.to_eid AS to_eid,
            COUNT(*) AS receive_count
        FROM
            Message M,
            Recipient R
        WHERE
            M.mid = R.mid
            AND M.from_eid =", from.eid,"
        GROUP BY
            R.to_eid
        ")
        
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        res <- dbGetQuery(conn, sql)
    }, finally = {
        dbDisconnect(conn)
    })

    res
}


group_count <- kGenderCount * kSeniorityCount * kDepartmentCount
gender <- rep(kGender, group_count / kGenderCount)
seniority <- rep(rep(kSeniority, each = kGenderCount),
                 group_count / kGenderCount / kSeniorityCount)
department <- rep(kDepartment, each = kGenderCount * kSeniorityCount)

recipient_count <- matrix(NA, group_count, group_count)

for (i in seq_len(group_count)) {
    for (j in seq_len(group_count)) {
        recipient_count[i,j] <- GetRecipientCount(from.gender = gender[i],
            from.seniority = seniority[i], from.department = department[i],
            to.gender = gender[j], to.seniority = seniority[j],
            to.department = department[j])
    }
}

employee_count <- rep(NA, group_count)
for (k in seq_len(group_count)) {
    employee_count[k] <- GetEmployeeCount(gender = gender[k],
        seniority = seniority[k], department = department[k])
}

send_count <- rep(NA, group_count)
for (k in seq_len(group_count)) {
    send_count[k] <- GetRecipientCount(from.gender = gender[k],
        from.seniority = seniority[k], from.department = department[k])
}
