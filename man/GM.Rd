\name{GM}
\alias{GM}
\title{
Grey Model Fitting
}

\description{
The GM function fit GM (1, 1) model for time series data.
}

\usage{
GM(data)
}

\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
}

\details{
In situations where there are limited observations available for modelling, grey modelling may be employed (Hsu and Wang, 2007). Using the OLS approach, this function calculates the parameters (a and b) of the GM (1, 1) model. Additionally, this function returns the model's fitted values and different evaluation criteria.
}

\value{
  \item{a }{Grey model parameter}
  \item{b }{Grey model parameter}
  \item{MAE_Grey }{Mean Absolute Error (MAE) of fitted Grey model}
  \item{MAPE_Grey }{Mean Absolute Percentage Error (MAPE) of fitted Grey model}
  \item{MSE_Grey }{Mean Square Error (MSE) of fitted Grey model}
  \item{RMSE_Grey }{Root Mean Square Error (RMSE) of fitted Grey model}
  \item{fitted }{Fitted values of Grey model}
}

\references{
Hsu, L. and Wang, C. (2007). Forecasting the output of integrated circuit industry using a grey model improved by the Bayesian analysis. Technological Forecasting and Social Change, 74, 843--853.

Mao, M. and Chirwa, E. C. (2006). Application of grey model GM(1, 1) to vehicle fatality risk estimation. Technological Forecasting and Social Change, 73, 588--605.
}

\seealso{
GM_test, fcast_grey
}

\examples{
xt <- c(640,724,813,1145,1509,2122,1883,2413,2834,4235,7144,5269)
GM(xt)
}

\keyword{ Grey Model }
