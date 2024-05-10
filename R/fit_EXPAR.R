fit_EXPAR <- function(ts_data, par) {

  p <- (length(par)-1)/2
  if(p%%1 != 0) stop("Invalid model order/parameters")
  N <- length(ts_data)
  k <- length(par)

  ar_par_phi <- par[1:p]
  ar_par_pi <- par[(p+1):(2*p)]
  ar_scale_par <- par[2*p+1]
  Predicted <- rep(NA, times = p)
  Residuals <- rep(0, times = p)
  ar_part <- NULL
  for (t in (p:(N-1))) {
    for (i in 1:p){
      ar_part[i] <- (ar_par_phi[i] + (ar_par_pi[i]*exp(-1*(ar_scale_par)*((ts_data[t])^2))))*ts_data[t-i+1]
    }
    Predicted[t+1] <- sum(ar_part)
    Residuals[t+1] <- (ts_data[t+1] - Predicted[t+1])
  }
  RSS <- sum(Residuals^2)
  AIC <- 2*k + N*log(RSS/N)
  AIC_c <- AIC + (2*k*(k+1)/(N-k-1))
  BIC <- k*log(N) + N*log(RSS/N)
  model.fit <- list(Fitted = Predicted, Residuals = Residuals,
                    RSS = RSS, AIC = AIC, AIC_c = AIC_c, BIC = BIC)
  return(model.fit)
}
