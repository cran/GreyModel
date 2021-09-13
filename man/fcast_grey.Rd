\name{fcast_grey}
\alias{fcast_grey}
\title{
Grey Model Forecast
}

\description{
The fcast_grey function computes the h step ahead forecast values.
}

\usage{
fcast_grey(data, h=3)
}

\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
  \item{h}{
The forecast horizon.
}
}

\details{
This function returns the fitted Grey model's h step ahead forecasted values.
}

\value{
  \item{Forecasted_value }{h step ahead forecasted values of the fitted Grey Model.}
}

\references{
Hsu, L. and Wang, C. (2007). Forecasting the output of integrated circuit industry using a grey model improved by the Bayesian analysis. Technological Forecasting and Social Change, 74, 843--853.

Ou, S. (2012). Forecasting agricultural output with an improved grey forecasting model based on the genetic algorithm. Computer and Electronics in Agriculture, 85, 33--39.

Wang, C. and Hsu, L. (2008). Using genetic algorithms grey theory to forecast high technology industrial output. Applied Mathematics and Computation, 195, 256--263.
}

\seealso{
GM, GM_test
}

\examples{
xt <- c(640,724,813,1145,1509,2122,1883,2413,2834,4235,7144,5269)
fcast_grey(data=xt)
}

\keyword{ Grey Model }
