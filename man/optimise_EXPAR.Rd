\name{optimise_EXPAR}
\alias{optimise_EXPAR}
\title{Fitting of EXPAR model with given order}
\description{Fits an EXPAR model of given order to the data by RSS minimisation}
\usage{optimise_EXPAR(ts_data, order, init, opt_method = "BFGS")}
\arguments{
\item{ts_data}{A univarite time series data, to which an EXPAR model of given order is to be fitted.}
\item{order}{Order \eqn{p} of the EXPAR model to be considered.}
\item{init}{The initial values for optimisation. If omitted, initial values are decided using \code{initial_val()}.
}
\item{opt_method}{The optimization algorithm to be used for RSS minimization. Corresponds to arguments from \code{optim()} in \code{stats} library. Defaults to the Broyden–Fletcher–Goldfarb–Shanno (BFGS) algorithm.}
}
\details{This function estimates the parameters an EXPAR\eqn{(p)} model by minimizing RSS and returns a fit with the optimised parameters. The fit is returned using the function \code{fit_EXPAR()}, with the parameters inputted being the optimized ones. Available optimisation techniques include possible arguments from \code{method} in \code{optim()}.
}
\value{Returns the fitted EXPAR model as a list with the following components,
\item{series}{The data used for fitting the model.}
\item{order}{Order \eqn{p} of the fitted EXPAR model.}
\item{n}{Number of observations in \code{series}.}
\item{k}{Number of parameters in the model.}
\item{par}{Parameters of the fitted model.}
\item{Fitted}{Fitted values obtained from the model.}
\item{Residuals}{Residuals of the fitted model.}
\item{RSS}{The residual sum of squares.}
\item{AIC}{Akaike information criterion, evaluated from \code{RSS}.}
\item{AIC_c}{Corrected Akaike information criterion, evaluated from \code{RSS}.}
\item{BIC}{Bayesian information criterion, evaluated from \code{RSS}.}
\item{counts}{\code{counts} returned by \code{optim()}}
\item{convergence}{\code{convergence} returned by \code{optim()}}
\item{message}{\code{message} returned by \code{optim()}}
}
\examples{
datats <- ts(egg_price_index[,3], start = c(2013, 1), frequency = 12)
optimise_EXPAR(datats, 2, opt_method = "BFGS")
}
