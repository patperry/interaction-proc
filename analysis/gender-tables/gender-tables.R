# gender-tables.R
# ---------------

require("RSQLite")
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "data/enron/enron.db")
query <- dbSendQuery(con, "
    SELECT
        F.genF,
        T.genF,
        COUNT(*) AS total
    FROM
        Message M,
        Recipient R,
        Employee F,
        Employee T
    WHERE
        M.mid = R.mid
        AND M.from_eid = F.eid
        AND R.to_eid = T.eid
    GROUP BY
        F.genF, T.genF
")
res <- fetch(query)
dbClearResult(query)
