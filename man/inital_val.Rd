\name{inital_val}
\alias{inital_val}
\title{Initial values for fitting EXPAR model}
\description{Determines the initial values to be used during parameter estimation.}
\usage{inital_val(ts_data, order)}
\arguments{
  \item{ts_data}{A univarite time series data, to which an EXPAR model is to be fitted.}
\item{order}{Order \eqn{p} of the EXPAR model to be considered.}
}
\details{This function determines the initial values required for optimising the parameters of EXPAR model. The initial values are extracted from an ARIMA\eqn{(p,d,0)} model with the same order and appropriate differencing. These will act as default initial values in the \code{optimise_EXPAR()} and \code{best_EXPAR()} functions, unless specified by user. Without these values, the optimisation process may fail.
}
\value{It returns the initial values of the EXPAR model over which optimisation is to be done parameter estimation.}
\examples{
datats <- ts(egg_price_index[,3], start = c(2013, 1), frequency = 12)
inital_val(datats, 2)
}
