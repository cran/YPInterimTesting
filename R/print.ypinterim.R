print.ypinterim <- function(x, ...) {

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

    cat("\n===========================================================\n")
    cat("       Interim Analysis For Surivival Outcomes")
    cat("\n===========================================================\n")

    name_nominal_p <- rep("       TestStat (nominal p-value):", length(x_names))
    name_nominal_p[1] <- "       TestStat (nominal p-value*):"

    for (ilook in 1:length(x_names)) {
        cat("    <", x_names[ilook], ">", "\n")
        cat("       Alpha allocated (cumulative):", paste(x_spenfun[ilook]," (", cx_spenfun[ilook], ")", sep=""), "\n")
        cat(name_nominal_p[ilook], x_teststat[ilook], paste("(", x_pvalue[ilook],
            ")", sep = ""), "\n")
        cat("       Bound:", x_critvalue[ilook], "\n")
        if (ilook != length(x_names))
            cat("-------------------------------------------------------\n")
    }
    cat("===========================================================\n")
    cat("* The probability of the test statistic exceeding the critical value\n")
    cat(" at that look, regardless of the test behavior at other looks.\n")
}

