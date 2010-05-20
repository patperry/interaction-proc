# gender-tables.R
# ---------------

require("RSQLite")

kDbDriver <- dbDriver("SQLite")
kDbName <- "data/enron/enron.db"

kGender <- c("Female", "Male")
kSeniority <- c("Junior", "Senior")
kDepartment <- c("Legal", "Trading", "Other")


GetGenderTable <- function(send.sen = kSeniority,
                           send.dep = kDepartment,
                           recv.sen = kSeniority,
                           recv.dep = kDepartment)
{
    InSet <- function(set, domain) {
        set <- lapply(set, match.arg, domain)
        paste("IN('",
              do.call(paste, c(set, sep="', '")),
              "')",
              sep="")
    }

    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
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
                AND R.to_eid = T.eid","
                AND F.seniority", InSet(send.sen, kSeniority),"
                AND F.department", InSet(send.dep, kDepartment),"
                AND T.seniority", InSet(recv.sen, kSeniority),"
                AND T.department", InSet(recv.dep, kDepartment),"
            GROUP BY
                F.gender,
                T.gender
        ")
        rows <- dbGetQuery(conn, sql)

    }, finally = {
        dbDisconnect(conn)
    })

    levels <- kGender
    nlevels <- length(levels)

    send.gen <- match(rows$send_gen, levels)
    recv.gen <- match(rows$recv_gen, levels)
    total <- rows$total

    res <- matrix(0, nlevels, nlevels)
    dimnames(res) <- list(levels, levels)
    res[send.gen + nlevels * (recv.gen - 1)] <- total

    res
}

