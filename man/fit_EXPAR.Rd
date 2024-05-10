\name{fit_EXPAR}
\alias{fit_EXPAR}
\title{Fitting of EXPAR model with known parameters}
\description{Fits an EXPAR model to the dataset for given parameter values, without any optimisation.}
\usage{fit_EXPAR(ts_data, par)}
\arguments{
\item{ts_data}{A univariate data to which the model is to be fitted, preferably (but not limited to) a \code{ts} object.}
\item{par}{The parameter vector. Any value from the parametric space of EXPAR model is permissible. Number of required parameters is \eqn{2p+1}, where \eqn{p} is order of the model. Entries are arranged consecutively, for example, for order 2, the form of input is \code{c(Phi1,Phi2,Pi1,Pi2,Gamma)}.}
}
\details{This function uses the form of EXPAR model to generate consecutive fitted observations as theoretical values obtained from the model. The form of EXPAR model employed is given by,
\deqn{y_{t+1}=\sum_{i=1}^{p}[({\phi_i+\pi_i e^{-\gamma y_t^2}})\ y_{t-i+1}\ ]+\epsilon_{t+1}}
where, \eqn{\gamma>0} and \eqn{\{\epsilon_t\}} is white noise process with zero mean and constant variance \eqn{\sigma_e^2}

The process of fitting involves using actual observations in the dataset in the RHS of above equation and obtaining the fitted values \eqn{y_{t+1}}. No optimization is done as parameter estimation is omitted. It is useful for simulation of data with given order and parameters.
}
\value{Returns the fitted EXPAR model as a list with the following components,
\item{Fitted}{Fitted values obtained from LHS of the aforementioned model. Due to obvious reasons, fits of the first \eqn{p} observations are not obtained.}
\item{Residuals}{The residuals \eqn{\epsilon_{t+1}}, computed by iterative fitting.}
\item{RSS}{The residual sum of squares.}
\item{AIC}{Akaike information criterion, evaluated from \code{RSS}.}
\item{AIC_c}{Corrected Akaike information criterion, evaluated from \code{RSS}.}
\item{BIC}{Bayesian information criterion, evaluated from \code{RSS}.}
}
\examples{
datats <- ts(egg_price_index[,3], start = c(2013, 1), frequency = 12)
fit_EXPAR(datats, par = c(0.45,0.68,0.48))
}
