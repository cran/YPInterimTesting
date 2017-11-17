\name{ virtual}
\alias{ virtual}
\docType{data}
\title{virtual Data Set for the example}
\description{
This virtual data set is created to show how to utilize the package.
}
\usage{data(virtual_data)}
\format{
  The data has the following information:
  \describe{
    \item{\code{time}}{the numeric matrix of virtual event times for all interim looks.}
    \item{\code{event}}{the numeric matrix of right-censoring indicators corresponding to \code{time} (event = 1, censored = 0).}
    \item{\code{group}}{the numeric vector of the group indicator (treatment = 1, control = 0).}
  }
}

\examples{
library(YPInterimTesting)
data(virtual_data)
time <- virtual$time
colnames(time) # Check the order of the column names.
head(time)
}
\seealso{\code{\link{ypinterim}}}
\keyword{Data}

