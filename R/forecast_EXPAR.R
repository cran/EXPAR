forecast_EXPAR <- function(EXPAR_model, h = 1) {

  p <- EXPAR_model$order
  N <- h+p
  k <- 2*p+1
  par <- unname(unlist(na.omit(as.data.frame(unlist(EXPAR_model$par)))))
  fit_val <- EXPAR_model$Fitted
  resid_val <- EXPAR_model$Residuals
  ar_par_phi <- par[1:p]
  ar_par_pi <- par[(p+1):(2*p)]
  ar_scale_par <- par[2*p+1]
  lags <- (length(fit_val)-p+1):length(fit_val)
  Predicted <- fit_val[lags]
  Residuals <- resid_val[lags]
  ar_part <- NULL
  for (t in (1:h)) {
    for (i in 1:p){
      ar_part[i] <- (ar_par_phi[i] + (ar_par_pi[i]*exp(-1*(ar_scale_par)*((Predicted[t])^2))))*Predicted[length(Predicted)-i+1]
    }
    Predicted[length(Predicted)+1] <- sum(c(ar_part))
    Residuals[length(Residuals)+1] <- 0
  }
  predictions <- Predicted[(length(lags)+1):length(Predicted)]
  return(predictions)
}
