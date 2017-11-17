\name{ypinterim}
\alias{ypinterim}
\alias{ypinterim.default}
\alias{summary.ypinterim}
\alias{print.ypinterim}
\title{The main function of the package performing the adaptively weighted log-rank test for interim monitoring}
\usage{
\method{ypinterim}{default}(time, event, group, spenfun, critvalue = NULL,
  repnum = 10000, bound = 50, ...)
}
\arguments{
\item{...}{for S4 method only.}

\item{time}{the numeric matrix of event times for all interim looks. The columns of the matrix represent interim looks in date order.}

\item{event}{the numeric matrix of right-censoring indicators corresponding to \code{time} (event = 1, censored = 0). The dimension of the matrix should be equivalent to that of \code{time}.}

\item{group}{the numeric vector of the group indicator (treatment = 1, control = 0).}

\item{spenfun}{the numeric vector of the values for the spending function \eqn{\alpha(t)}. Must be user-specified.}

\item{critvalue}{the vector of the critical values for the prior interim look. If it is not supplied, the value at each interim look will be obtained using the re-sampling method. The length of vector should be at least one less than the number of columns of \code{time}.}

\item{repnum}{the number of replications for a normal resampling approximation. The default value is 10000.}

\item{bound}{the boundary (-\code{bound}, \code{bound}) for estimating the parameters relevant to a short-term and a long-term hazard ratio in Yang and Prentice model (Yang and Prentice, 2005). These parameters are \eqn{\beta_1} and \eqn{\beta_2} in their notations. The default boundary is \eqn{(-50, 50)}.}
}

\description{
Provides stopping boundaries and nominal p-values at the interim looks using the adaptively weighted log-rank test developed by Yang and Prentice (2010).
}
\details{
The object being returned by the function \code{\link{ypinterim}} can be formatted to a table using the function \code{\link{summary}}.
}

\references{
Yang, S. and Prentice, R. (2010), Improved Logrank-Type Tests for Survival Data Using Adaptive Weights. Biometrics, 66: 30–38.

Yang, Song, and Ross Prentice. "Semiparametric analysis of short-term and long-term hazard ratios with two-sample survival data." Biometrika 92.1 (2005): 1-17.
}

\examples{
library(YPInterimTesting)
data(virtual_data)
spenfun <- c(1.3E-5, 4.4E-4, 0.003, 0.008)

result <- ypinterim(time=virtual$time, event=virtual$event, group=virtual$group, spenfun=spenfun)
result
summary(result)
}
