## Improved version supplied by one referee, not shown in paper
KalmanRimp <- function(pos) {

    kalmanfilter <- function(z) {
        ## predicted state and covriance
        xprd <- xest %*% A
        pprd <- crossprod(pest %*% A, A) + Q

        ## estimation
        B <- crossprod(H, pprd)
        S <- B %*% H + R

        ##  kalmangain <- (S \ B)'
        kalmangain <- solve(S, B)

        ## estimated state and covariance, assign to vars in parent env
        xest <<- xprd + (z - xprd %*% H) %*% kalmangain
        pest <<- pprd - pprd %*% H %*% kalmangain

        ## compute the estimated measurements
        y <- xest %*% H
    }

    dt <- 1
    A <- matrix( c( 1, 0, dt, 0, 0, 0,  # x
                   0, 1, 0, dt, 0, 0,   # y
                   0, 0, 1, 0, dt, 0,   # Vx
                   0, 0, 0, 1, 0, dt,   # Vy
                   0, 0, 0, 0, 1,  0,   # Ax
                   0, 0, 0, 0, 0,  1),  # Ay
                6, 6, byrow=FALSE)
    H <- matrix( c(1, 0, 0, 0, 0, 0,
                   0, 1, 0, 0, 0, 0),
                6, 2, byrow=FALSE)
    Q <- diag(6)
    R <- 1000 * diag(2)

    N <- nrow(pos)
    y <- matrix(NA, N, 2)

    xest <- matrix(0, 1, 6)
    pest <- matrix(0, 6, 6)

    for (i in 1:N) {
        y[i,] <- kalmanfilter(pos[i,,drop=FALSE])
    }

    invisible(y)
}
