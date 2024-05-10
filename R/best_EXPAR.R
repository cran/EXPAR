best_EXPAR <- function(ts_data, max.p = 5, ic = "AIC", opt_method = "BFGS") {

  AIC <- AIC_c <- BIC <- p <- NULL
  model.test <- list()
  for (i in 1:max.p) {
    model.test[[i]] <- optimise_EXPAR(ts_data, order = i,
                                      opt_method = opt_method)
    AIC[i] <- model.test[[i]]$AIC
    AIC_c[i] <- model.test[[i]]$AIC_c
    BIC[i] <- model.test[[i]]$BIC
    p[i] <- i
  }
  df <- data.frame("p" = p, "AIC" = AIC, "AIC_c" = AIC_c, "BIC" = BIC)
  if (ic=="AIC") {
    order_best <- df[which(df$AIC == min(df$AIC)), 1]
  } else if (ic=="AIC_c") {
    order_best <- df[which(df$AIC_c == min(df$AIC_c)), 1]
  } else  if (ic=="BIC") {
    order_best <- df[which(df$BIC == min(df$BIC)), 1]
  }
  best_model <- model.test[[order_best]]
  message(paste0("Best model is of order p=", order_best,
                 " on the basis of minimum ", ic, "."))
  return(best_model)
}
