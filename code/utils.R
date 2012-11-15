require(rhdf5)



read.h5out <- (function() {
    tri <- function(ap, uplo = c("UPPER", "LOWER")) {
        uplo <- match.arg(uplo)
        nel <- length(ap)
        dim <- floor(sqrt(2 * nel))
        a <- matrix(0, dim, dim)

        if (uplo == "UPPER") {
            a[row(a) >= col(a)] <- ap
        } else {
            a[row(a) <= col(a)] <- ap
        }
        t(a)
    }

    symm <- function(ap, uplo) {
        u <- tri(ap, uplo)
        a <- u + t(u)
        diag(a) <- diag(u)
        a
    }

    read.info.uplo <- function(file) {
        f <- H5Fopen(file)
        d <- H5Dopen(f, "information")
        a <- H5Aopen(d, "uplo")
        x <- H5Aread(a)
        H5Aclose(a)
        H5Dclose(d)
        H5Fclose(f)
        x
    }

    function(file) {
        data <- h5dump(file)
        names <- data$variate_names

        names(data$coefficients) <- names
        colnames(data$constraints) <- names

        imatp <- data$information
        uplo <- read.info.uplo(file)
        data$information <- symm(imatp, uplo)
        dimnames(data$information) <- list(names, names)

        names(data$score) <- names
        data$variate_names <- NULL

        names(data) <- gsub("_", ".", names(data))

        data
    }
})()



get.cov <- (function() {
    get.kkt <- function(data) {
        imat <- data$information
        C <- data$constraints
        n <- length(data$coefficients)
        k <- length(data$duals)
        N <- n + k

        i <- seq_len(n)
        j <- n + seq_len(k)
        K <- matrix(NA, N, N)
        K[i, i] <- imat
        K[i, j] <- C
        K[j, i] <- t(C)
        K[j, j] <- 0
        K
    }

    function(data, duals=FALSE) {
        K <- get.kkt(data)
        cov <- solve(K)
        if (!duals) {
            n <- length(data$coefficients)
            i <- seq_len(n)
            cov <- cov[i, i]
        }
        cov
    }
})()

