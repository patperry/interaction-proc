# code/boot.R
# -----------

GetBoot <- function(datadir = "analysis/boot", nreps = 500) {
    files <- list.files(datadir, full.names = TRUE)

    sys.source(files[1],
        boot1 <- new.env(parent = baseenv()))

    p <- length(boot1$coefs)

    coefs <- matrix(NA, nreps, p)
    wt <- rep(NA, n)

    rep <- 0
    for (r in seq_len(n)) {
        sys.source(files[r],
            bootrep <- new.env(parent = baseenv()))

        if (bootrep$log.wt == 0) {
            rep <- rep + 1
            coefs[rep,] <- bootrep$coefs

            if (rep == nreps)
                break
        }
    }

    if (rep != nreps) {
        warning("Only got ", rep, " replciates")
    }

    coefs <- coefs[seq_len(rep),,drop = FALSE]
    list(coefs = coefs)
}
