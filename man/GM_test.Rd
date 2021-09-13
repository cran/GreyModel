\name{GM_test}
\alias{GM_test}
\title{
Grey Model Test for Data Suitability
}

\description{
The GM_test function test the suitability of data for Grey modelling.
}

\usage{
GM_test(data)
}

\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
}

\details{
On the considered time series data, this function computes the ratio sequence. The data is suitable for grey modelling if the sequence value falls between 0.1345 and 7.389 (Hsu and Wang, 2007).
}

\value{
  \item{Test_Result }{If the data is suitable for grey modelling, “data is suitable for Grey modelling” will be printed; otherwise, “data is not suitable for Grey modelling” will be printed.}
}

\references{
Hsu, L. and Wang, C. (2007). Forecasting the output of integrated circuit industry using a grey model improved by the Bayesian analysis. Technological Forecasting and Social Change, 74, 843--853.
}

\seealso{
GM, fcast_grey
}

\examples{
xt <- c(640,724,813,1145,1509,2122,1883,2413,2834,4235,7144,5269)
GM_test(data=xt)
}

\keyword{ Grey Model }
