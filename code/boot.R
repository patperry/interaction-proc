# code/boot.R
# -----------

GetBoot <- function(datadir = "analysis/boot", nreps = 500) {
    files <- list.files(datadir, full.names = TRUE)

    sys.source(files[1],
        boot1 <- new.env(parent = baseenv()))

    p <- length(boot1$coefs)

    coefs <- matrix(NA, nreps, p)

    rep <- 0
    for (r in seq_along(files)) {
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

GetBias <- function()
{
    sys.source("analysis/dynamic.R",
        dynamic <- new.env(parent = baseenv()))

    boot <- GetBoot()
    nrep <- nrow(boot$coefs)

    intervals.send <- dynamic$intervals.send
    intervals.recv <- dynamic$intervals.recv
    coefs <- dynamic$coefs
    coefs.se <- dynamic$coefs.se

    bias <- colMeans(boot$coefs) - coefs
    bias.se <- sqrt(apply(boot$coefs, 2, var) * ((nrep-1)/nrep)
                    / nrep)

    list(est = bias, est.se = bias.se, nreps = nrow(boot$coefs))
}
