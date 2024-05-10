inital_val <- function(ts_data, order) {
  p <- order
  N <- length(ts_data)
  k <- 2*(p+1)
  d <- auto.arima(ts_data, max.q = 0, seasonal = F)$arma[6]
  arima_param <- unname((Arima(ts_data, order = c(p,d,0)))$coef,)
  arima_init <- c(arima_param[1:p], rep(0.5,p+1))
  return(arima_init)
}
