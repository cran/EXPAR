optimise_EXPAR <- function(ts_data, order, init, opt_method = "BFGS") {

  p <- order
  N <- length(ts_data)
  k <- 2*p+1
  if(missing(init)) init <- inital_val(ts_data = ts_data, order = order)

  # Parameter estimation using optim()
  RSS <- function(par) {
    return(fit_EXPAR(ts_data = ts_data, par = par)$RSS)
  }
  opt_result <- optim(par = init, fn = RSS, method = opt_method)

  # Save parameter estimates
  ar_par_phi <- opt_result$par[1:p]
  ar_par_pi <- opt_result$par[(p+1):(2*p)]
  ar_scale_par <- opt_result$par[2*p+1]
  par <- c(ar_par_phi, ar_par_pi, ar_scale_par)
  names(par) <- c(paste0("Phi",1:p), paste0("Pi",1:p), "Gamma")
  model.fit <- fit_EXPAR(ts_data = ts_data, par = opt_result$par)
  out <- list(series = deparse(substitute(ts_data)), order = p, n = N, k = k,
              par = par, Fitted = model.fit$Fitted, Residuals = model.fit$Residuals,
              RSS = opt_result$value, AIC = model.fit$AIC,
              AIC_c = model.fit$AIC_c, BIC = model.fit$BIC,
              counts = opt_result$counts, convergence = opt_result$convergence,
              message = opt_result$message)
  return(out)
}
