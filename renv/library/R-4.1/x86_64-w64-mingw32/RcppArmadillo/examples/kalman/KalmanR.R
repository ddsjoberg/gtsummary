KalmanR <- function(pos) {

    kalmanfilter <- function(z) {
        ## predicted state and covariance
        xprd <- A %*% xest
        pprd <- A %*% pest %*% t(A) + Q

        ## estimation
        S <- H %*% t(pprd) %*% t(H) + R
        B <- H %*% t(pprd)

        kalmangain <- t(solve(S, B))

        ## estimated state and covariance
        ## assigned to vars in parent env
        xest <<- xprd + kalmangain %*% (z - H %*% xprd)
        pest <<- pprd - kalmangain %*% H %*% pprd

        ## compute the estimated measurements
        y <- H %*% xest
    }

    dt <- 1
    A <- matrix( c( 1, 0, dt, 0, 0, 0,  # x
                   0, 1, 0, dt, 0, 0,   # y
                   0, 0, 1, 0, dt, 0,   # Vx
                   0, 0, 0, 1, 0, dt,   # Vy
                   0, 0, 0, 0, 1,  0,   # Ax
                   0, 0, 0, 0, 0,  1),  # Ay
                6, 6, byrow=TRUE)
    H <- matrix( c(1, 0, 0, 0, 0, 0,
                   0, 1, 0, 0, 0, 0),
                2, 6, byrow=TRUE)
    Q <- diag(6)
    R <- 1000 * diag(2)
    N <- nrow(pos)
    Y <- matrix(NA, N, 2)

    xest <- matrix(0, 6, 1)
    pest <- matrix(0, 6, 6)

    for (i in 1:N) {
        Y[i,] <- kalmanfilter(t(pos[i,,drop=FALSE]))
    }
    invisible(Y)
}
