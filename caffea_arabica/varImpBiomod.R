### Calculate variable importance
varImpBiomod <- function(model, varnames, data, n=10) {
  # This calculation of variable importance is based on
  # the Biomod-Tutorial by Wilfried Thuiller
  varImp <- numeric(length(varnames))
  names(varImp) <- varnames
  base.pred <- predict(model, type="response", newdata=data)
  for (var in varnames) {
    varimp.test <- data
    tmp <- numeric(n)
    for (i in 1:n) {
      varimp.test[[var]] <- sample(varimp.test[[var]])
      tmp.pred <- predict(model, type="response", newdata=varimp.test)
      tmp[i] <- cor(base.pred, tmp.pred)
    }	
    varImp[var] <- mean(tmp)
  }
  1-varImp
}