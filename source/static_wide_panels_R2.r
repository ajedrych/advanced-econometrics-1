
static_wide_panels_R2 = function(model) {
  # written by dr Rafal Wozniak, Faculty of Economic Sciences, University of Warsaw
  # 2019-04-17
  #
  # arguments:
  # ------------------
  # model - model estimated by plm function
  
  # check if it is of class 'plm'
  
  beta_hat = model$coefficients
  iisvar = attr(model$residuals, "index")[,1]
  
  y = model$model[,1]
  x = as.matrix(sapply(model$model[,-1], as.numeric))
  y_hat = predict(model)
  y_mean = aggregate(x = y, by=list(iisvar), FUN=mean)
  x_mean = aggregate(x = x, by=list(iisvar), FUN=mean)
  y_times = aggregate(x = y, by = list(iisvar), FUN = length)
  y_bar = cbind(kronecker(y_mean[1,2], rep(1, times=y_times[1,2])))
  x_bar = matrix(rep(as.matrix(sapply(x_mean[1,-1], as.numeric)), times=y_times[1,2]), nrow = y_times[1,2], byrow = TRUE)
  
  for(i in 2:length(y_times$Group.1)) {
    y_bar = rbind(y_bar, cbind(kronecker(y_mean[i,2], rep(1, times=y_times[i,2]))))
    x_bar = rbind(x_bar, matrix(rep(as.matrix(sapply(x_mean[i,-1], as.numeric)), 
                                    times=y_times[i,2]), nrow = y_times[i,2], byrow = TRUE))
  }
  
  R2_within = cor(y-y_bar, x%*%beta_hat-x_bar%*%beta_hat)^2
  R2_between = cor(x = y_bar, y = x_bar%*%beta_hat)^2
  R2_overall = cor(x = y, y = x%*%beta_hat)^2
  R2_stats = list('within'=R2_within, 'between'=R2_between, 'overall'=R2_overall)
  
  cat("\n R-squared: ",
      "\n          within  = ", R2_within,
      "\n          between = ", R2_between,
      "\n          overall = ", R2_overall, "\n")
  
  return(R2_stats)
}
