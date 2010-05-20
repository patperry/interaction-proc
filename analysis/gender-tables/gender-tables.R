# gender-tables.R
# ---------------

require("RSQLite")


kGender <- c("Female", "Male")
kSeniority <- c("Junior", "Senior")
kDepartment <- c("Legal", "Trading", "Other")


GetGenderTable <- function(send.sen = kSeniority,
                           send.dep = kDepartment,
                           recv.sen = kSeniority,
                           recv.dep = kDepartment)
{
    InSet <- function(set) {
        paste("IN('",
              do.call(paste, c(as.list(set), sep="', '")),
              "')",
              sep="")
    }

    send.sen <- lapply(send.sen, match.arg, kSeniority)
    send.dep <- lapply(send.dep, match.arg, kDepartment)
    recv.sen <- lapply(recv.sen, match.arg, kSeniority)
    recv.dep <- lapply(recv.dep, match.arg, kDepartment)
    
    drv <- dbDriver("SQLite")
    db <- dbConnect(drv, dbname = "data/enron/enron.db")
    sql <- paste("
        SELECT
            F.gender AS send_gender,
            T.gender AS recv_gender,
            COUNT(*) AS total
        FROM
            Message M,
            Recipient R,
            Employee F,
            Employee T
        WHERE
            M.mid = R.mid
            AND M.from_eid = F.eid
            AND R.to_eid = T.eid",
            "AND F.seniority", InSet(send.sen),
            "AND F.department", InSet(send.dep),
            "AND T.seniority", InSet(recv.sen),
            "AND T.department", InSet(recv.dep),"
        GROUP BY
            F.genF,
            T.genF
    ")
    rows <- dbGetQuery(db, sql)

    levels <- kGender
    send.gen <- as.integer(factor(rows$send_gen, levels))
    recv.gen <- as.integer(factor(rows$recv_gen, levels))
    total <- rows$total
    
    res <- matrix(0, 2, 2)
    dimnames(res) <- list(levels, levels)    
    res[send.gen + 2 * (recv.gen - 1)] <- total
    
    res
}

