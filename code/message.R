# code/messages.R
# ---------------

source("code/common.R")

GetMessages <- function(duplicate = TRUE) {
    if (duplicate) {
        msg <- GetMessagesDupe()
    } else {
        msg <- GetMessagesNoDupe()
    }
    unix.time <- msg$unix.time
    msg$unix.time <- NULL
    msg$time <- as.POSIXct(unix.time, origin = "1970-01-01")
    msg$time.secs <- unix.time - min(unix.time)
    
    time.origin <- if(length(msg$time) > 0) (msg$time[1]) else NA
    
    if (duplicate) {
        msg.raw <- msg
        msg <- data.frame(mid = msg.raw$mid,
                          time = msg.raw$time,
                          time.secs = msg.raw$time.secs,
                          from = msg.raw$from,
                          to = msg.raw$to)
        attr(msg, "time.origin") <- time.origin
        msg
    } else {
        attr(msg, "time.origin") <- time.origin        
        msg
    }
}

GetMessagesDupe <- function() {
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        msg <- dbGetQuery(conn, "
            SELECT
                M.mid AS mid,
                M.unix_time AS unix_time,
                M.from_eid AS from_eid,
                R.to_eid AS to_eid
            FROM 
                Message M,
                Recipient R
            WHERE
                M.mid = R.mid
            ORDER BY
                M.mid
            ")
    }, finally = {
        dbDisconnect(conn)
    })

    list(count = length(msg$mid),
         mid = msg$mid,
         unix.time = msg$unix_time,
         from = msg$from_eid,
         to = msg$to_eid
         )
}

GetMessagesNoDupe <- function() {
    conn <- dbConnect(kDbDriver, dbname = kDbName)
    tryCatch({
        msg <- dbGetQuery(conn, "
            SELECT
                M.mid AS mid,
                M.unix_time AS unix_time,
                M.from_eid AS from_eid,
                COUNT(*) AS length
            FROM 
                Message M,
                Recipient R
            WHERE
                M.mid = R.mid
            GROUP BY
                M.mid
            ORDER BY
                M.mid
            ")
        
        to_eid <- dbGetQuery(conn, "
            SELECT
                to_eid
            FROM
                Recipient
            ORDER BY
                mid
            ")$to_eid
    }, finally = {
        dbDisconnect(conn)
    })
    
    list(count = length(msg$mid),
         mid = msg$mid,
         length = msg$length,
         offset = c(0, cumsum(msg$length)) + 1,
         unix.time = msg$unix_time,
         from = msg$from_eid,
         to = to_eid
         )    
}

