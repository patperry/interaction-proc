# code/employee.R
# ---------------

source("code/common.R")

GetEmployees <- function() {
    sql <- paste("
        SELECT
            eid,
            gender,
            seniority,
            department
        FROM
            Employee
    ")
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        emp.raw <- dbGetQuery(conn, sql)
    }, finally = {
        dbDisconnect(conn)
    })
    
    emp <- data.frame(eid = emp.raw$eid,
                      gender = factor(emp.raw$gender, kGender),
                      senioirity = factor(emp.raw$seniority, kSeniority),
                      department = factor(emp.raw$department, kDepartment))
    emp
}
