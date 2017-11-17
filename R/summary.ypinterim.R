summary.ypinterim <- function(object, ...) {
    x <- object
    if (class(x) != "ypinterim")
        stop("Please use the object from the 'ypinterim' function")

    digit <- paste("%.", max(3, getOption("digits") - 3), "f", sep = "")
    x_teststat <- sprintf(digit, x$teststat)
    x_pvalue <- fun_less(sprintf(digit, x$pvalue))
    x_critvalue <- sprintf(digit, x$critvalue)
    x_bound <- x$bound
    x_repnum <- x$repnum
    x_length <- length(x_teststat)
    x_names <- paste("Look", 1:x_length)
    x_spenfun <- fun_less(sprintf(digit, x$dspenfun))
    cx_spenfun <- sprintf(digit, x$spenfun)

    sresult <- cbind(x_repnum, x_spenfun, cx_spenfun, x_critvalue, x_teststat, x_pvalue)
    colnames(sresult) <- c("repnum", "alpha","cum_alpha", "bound", "teststat", "nominal_pvalue")
    rownames(sresult) <- paste("Look", "(", 1:x_length, ")", sep = "")

    as.data.frame(sresult)
}
