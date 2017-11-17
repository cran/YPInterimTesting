ypinterim.default <- function(time, event, group, spenfun, critvalue = NULL, repnum = 10000,
    bound = 50, ...) {

    nmonitoring <- length(spenfun)
    spenfunvalue <- c(0, spenfun)

    y_iv <- as.matrix(time)
    n <- nrow(y_iv)

    for (i_mon in 1:nmonitoring) {
        each_Y_iv <- y_iv[, i_mon]
        if (length(unique(each_Y_iv)) != length(each_Y_iv)) {
            y_iv[, i_mon] <- each_Y_iv + runif(n, 0, 1e-50)
        }
    }

    delta_iv <- as.matrix(event) == 1
    z_i <- as.matrix(group) == 1

    o_all <- apply(y_iv, 2, function(x) {
        order(x)
    })
    quan1 <- quan2 <- v1 <- v2 <- matrix(NA, nrow = n, ncol = nmonitoring)
    fitbeta_all <- NULL

    for (o_i in 1:nmonitoring) {
        oy_iv <- y_iv[o_all[, o_i], o_i]
        od_iv <- delta_iv[o_all[, o_i], o_i]
        oz_i <- z_i[o_all[, o_i]]
        fitbeta <- fun_estimate(oy = oy_iv, od = od_iv, oz = oz_i, bound = bound)
        fitbeta_all <- cbind(fitbeta_all, fitbeta)
        gamma1 <- exp(-oz_i * fitbeta[1])
        gamma2 <- exp(-oz_i * fitbeta[2])
        K <- n:1
        Lambda1 <- cumsum(od_iv * gamma1/K)
        Lambda2 <- cumsum(od_iv * gamma2/K)
        P <- exp(-Lambda2)
        PL <- c(1, P[1:(n - 1)])
        R <- cumsum(PL * od_iv * gamma1/K)/P

        tt1 <- exp(fitbeta[1])
        tt2 <- exp(fitbeta[2])

        psi1 <- (1 + R)/(1/tt1 + 1/tt2 * R)
        psi2 <- 1/psi1

        yl <- K
        yzl <- cumsum(oz_i[n:1])[n:1]

        quan1[, o_i] <- od_iv * psi1 * (oz_i - yzl/yl)
        quan2[, o_i] <- od_iv * psi2 * (oz_i - yzl/yl)

        v1[, o_i] <- psi1^2 * (yzl/yl - (yzl/yl)^2) * od_iv
        v2[, o_i] <- psi2^2 * (yzl/yl - (yzl/yl)^2) * od_iv
    }

    sd1_den <- sqrt(colSums(v1))
    sd2_den <- sqrt(colSums(v2))
    w1 <- colSums(quan1)/sd1_den
    w2 <- colSums(quan2)/sd2_den

    testvalue <- apply(cbind(w1, w2), 1, FUN = function(x) max(abs(x)))

    c_names <- paste("c", 1:nmonitoring, sep = "")

    if (!is.null(critvalue)) {
        for (cc in 1:nmonitoring) {
            if (cc < nmonitoring)
                assign(c_names[cc], critvalue[cc])
            if (cc == nmonitoring)
                assign(c_names[cc], NULL)
        }
    } else {
        for (cc in 1:nmonitoring) assign(c_names[cc], NULL)
    }


    for (c_i in 1:nmonitoring) {
        if (is.null(get(c_names[c_i]))) {
            mquan1 <- as.matrix(quan1[, 1:c_i])
            mquan2 <- as.matrix(quan2[, 1:c_i])

            t_tilde <- awlrstat(repnum = repnum, n = n, quan1 = mquan1,
                quan2 = mquan2, sd1 = sd1_den[1:c_i], sd2 = sd2_den[1:c_i])

            if (c_i == 1) {
                target_quantile <- t_tilde[, 1]
            } else {
                ind <- 1
                for (con_i in 1:(c_i - 1)) {
                  ind <- (t_tilde[, con_i] <= get(c_names[con_i])) * ind
                }
                target_quantile <- t_tilde[as.logical(ind), c_i]
            }
            level_quantile <- 1 - ((spenfunvalue[c_i + 1] - spenfunvalue[c_i])/(1 -
                spenfunvalue[c_i]))
            assign(c_names[c_i], quantile(target_quantile, level_quantile))
        }
    }

    pvalue <- colMeans((t_tilde - matrix(testvalue, nrow = 1)[rep(1, repnum),
        ]) > 0)
    c_values <- unlist(mget(c_names))
    names(c_values) <- c_names
    colnames(fitbeta_all) <- 1:nmonitoring
    rownames(fitbeta_all) <- c("b1", "b2")

    result <- list()
    result$teststat <- testvalue
    result$pvalue <- pvalue
    result$critvalue <- c_values
    result$beta <- fitbeta_all
    result$bound <- bound
    result$repnum <- repnum
    result$spenfun <- spenfun
    result$dspenfun <- diff(c(0, spenfun))
    result$call <- match.call()

    class(result) <- "ypinterim"

    result
}
