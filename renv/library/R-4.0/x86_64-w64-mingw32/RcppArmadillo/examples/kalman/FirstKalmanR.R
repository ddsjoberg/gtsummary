FirstKalmanR <- function(pos) {

    kalmanfilter <- function(z) {
        dt <- 1
        A <- matrix(c( 1, 0, dt, 0, 0, 0, 0, 1, 0, dt, 0, 0,  # x,  y
                       0, 0, 1, 0, dt, 0, 0, 0, 0, 1, 0, dt,  # Vx, Vy
                       0, 0, 0, 0, 1,  0, 0, 0, 0, 0, 0,  1), # Ax, Ay
                    6, 6, byrow=TRUE)
        H <- matrix( c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
                    2, 6, byrow=TRUE)
        Q <- diag(6)
        R <- 1000 * diag(2)

        xprd <- A %*% xest		    # predicted state and covriance
        pprd <- A %*% pest %*% t(A) + Q

        S <- H %*% t(pprd) %*% t(H) + R     # estimation
        B <- H %*% t(pprd)
        kalmangain <- t(solve(S, B))

        ## estimated state and covariance, assign to vars in parent env
        xest <<- xprd + kalmangain %*% (z - H %*% xprd)
        pest <<- pprd - kalmangain %*% H %*% pprd

        ## compute the estimated measurements
        y <- H %*% xest
    }
    xest <- matrix(0, 6, 1)
    pest <- matrix(0, 6, 6)

    N <- nrow(pos)
    y <- matrix(NA, N, 2)
    for (i in 1:N) {
        y[i,] <- kalmanfilter(t(pos[i,,drop=FALSE]))
    }
    invisible(y)
}
