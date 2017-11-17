\name{YPInterimTesting-package}
\alias{YPInterimTesting-package}
\docType{package}
\title{
	Interim Monitoring Using Adaptively Weighted Log-rank Test in Clinical Trials
}
\description{
This package provides monitoring boundary for interim testing using the adaptively weighted log-rank test developed by Yang and Prentice (2010). A re-sampling method is used instead of the originally proposed method. It is shown in Yang (2017) that the adaptively weighted log-rank test is asymptotically fully efficient when the limiting censoring distributions in two groups are equal. For a given spending function, the main function of the package \code{ypinterim} provides stopping boundaries at the interim looks, along with nominal p-values.

The object being returned by the function \code{\link{ypinterim}} can be formatted to a table using the function \code{\link{summary}}.
}

\details{
\tabular{ll}{
Package: \tab YPInterimTesting\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2017-10-31\cr
License: \tab GPL (>= 3)\cr
}
}
\author{
Daewoo Pak and Song Yang
}
\references{
Yang, S. and Prentice, R. (2010), Improved Logrank-Type Tests for Survival Data Using Adaptive Weights. Biometrics, 66: 30–38.

Yang, S . Interim  monitoring using the adaptively weighted log-rank test in clinical trials for survival outcomes. 2017. Pre-print.
}

\keyword{Survival analysis, Clinical trials, Interim analysis, Yang and Prentice model}
\seealso{
\code{\link{ypinterim}}
}
