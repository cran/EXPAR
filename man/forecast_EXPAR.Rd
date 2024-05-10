\name{forecast_EXPAR}
\alias{forecast_EXPAR}
\title{Forecasting from EXPAR model}
\description{Generates out-of-sample forecasts from any fitted EXPAR model.}
\usage{forecast_EXPAR(EXPAR_model, h = 1)}
\arguments{
\item{EXPAR_model}{A fitted EXPAR model.}
\item{h}{Number of periods for forecasting. Defaults to one-step ahead forecast.}
}
\details{Forecasts from EXPAR model are generated iteratively using mathematical form of EXPAR model, considering fitted values and residuals while putting future error terms as zero.}
\value{Vector of forecasted values}
\examples{
datats <- ts(egg_price_index[,3], start = c(2013, 1), frequency = 12)
best_model = best_EXPAR(datats)
forecast_EXPAR(best_model, h = 5)
}
