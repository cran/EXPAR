\name{best_EXPAR}
\alias{best_EXPAR}
\title{Fitting of EXPAR model}
\description{Searches for the best EXPAR model among many orders (defaults upto 5), compares them using information criterion and returns the best fit.}
\usage{best_EXPAR(ts_data, max.p = 5, ic = "AIC", opt_method = "BFGS")}
\arguments{
\item{ts_data}{A univarite time series data, to which an EXPAR model is to be fitted.}
\item{max.p}{The maximum order upto which models are to be searched for comparison.}
\item{ic}{Information criterion to be used for model selection: Akaike information criterion (\code{"AIC"}), corrected Akaike information criterion (\code{"AIC_c"}) or Bayesian information criterion (\code{"BIC"}).}
\item{opt_method}{The optimization algorithm to be used for RSS minimization. Corresponds to arguments from \code{optim()} in stats. Defaults to the Broyden–Fletcher–Goldfarb–Shanno (BFGS) algorithm.}
}
\details{
Fits \code{max.p} number of EXPAR models to the given dataset by minimisation of RSS using \code{optimise_EXPAR()} and returns the best model among the evaluated ones. Model selection is based on the information critera given in \code{ic}.

The various information criterion are calculated (estimated) from RSS as,
\deqn{\textup{AIC} = 2k + n\log(\frac{\textup{RSS}}{n})}
\deqn{\textup{AIC}_\textup{c} = \textup{AIC} + \frac{2k(k+1)}{n-k-1}}
\deqn{\textup{BIC} = k\log(n) + n\log(\frac{\textup{RSS}}{n})}
where, \eqn{n,k} are the number of observations and the number of parameters, respectively.
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
\author{Saikath Das, Bishal Gurung, Achal Lama and KN Singh}
\references{
Haggan and Ozaki (1981). Modelling nonlinear random vibrations using an amplitude-dependent autoregressive time series model. Biometrika, 68(1):189-199. <doi:10.1093/biomet/68.1.189>.

Gurung (2015). An exponential autoregressive (EXPAR) model for the forecasting of all India annual rainfall. Mausam, 66(4):847-849. <doi:10.54302/mausam.v66i4.594>.
}
\examples{
datats <- ts(egg_price_index[,3], start = c(2013, 1), frequency = 12)
best_EXPAR(datats)
}
