#########CLEAR ENVIRONMENT & IMPORT LIBRARIES###########
# clear Global Environment
rm(list = ls())

install.packages("tseries")
library(tseries)
install.packages("fGarch")
library(fGarch) 
install.packages("rugarch")
library(rugarch)
install.packages("Hmisc")
library(Hmisc)
install.packages("openxlsx")
library(openxlsx)


######New Library for Auto Arima##########
install.packages("forecast")
library(forecast)



####################IMPORT DATA########################
dim(WTI_M)
y <- WTI_M$WTI
x1 <- WTI_M$WTI_LAG
x2 <- WTI_M$`Equity Market Volatility`
x3 <- WTI_M$`Economic Policy Uncertainty Europe`
x4 <- WTI_M$'NY Business Conditions'
x5 <- WTI_M$'3 Month Treasury Bill'
x6 <- WTI_M$`Infectious Disease Tracker`
x7 <- WTI_M$Nickel
x8 <- WTI_M$COV19
x9 <- WTI_M$RUWAR





##################CREATE TIMESERIES###################
# Create a time series object and plot
y=ts(y, frequency=12, start = c(2001,10))
y
plot(y, type="l", col="blue")

###############EXPLORE LINEAR RELATIONSHIPS############
# Correlation coefficients
cor(cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9))
# Correlation coefficients and p-values
rcorr(as.matrix(cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9))) 
# Scatterplot of all variables
pairs(cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9))



##################SELECTING FEATURES###################
# 1a.RUN MULTIPLE REGRESSION MODEL SELECTION TECHNIQUES
# Fit the full model
fitall <- lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9)
summary(fitall)


# Backward Elimination method
stepBE<-step(fitall, 
             scope=list(lower = ~ 1,
                        upper = ~ x1+x2+x3+x4+x5+x6+x7+x8+x9),
             direction="backward")
stepBE


# Forward Selection method
fitnull<-lm(y ~ 1)
stepFS<-step(fitnull, 
             scope=list(lower = ~ 1,
                        upper = ~ x1+x2+x3+x4+x5+x6+x7+x8+x9),
             direction="forward")
stepFS



# Stepwise Selection method
stepSS<-step(fitnull, 
             scope=list(lower = ~ 1,
                        upper = ~ x1+x2+x3+x4+x5+x6+x7+x8+x9),
             direction="both")
stepSS



#Present the full picture
print("Cumulative Results")
stepBE
stepFS
stepSS



####################ANALYZING THE DATA######################
# Summary Statistics and plots
plot(y, type="l", main="monthly returns")

hist(y, main="histogram of returns")

qqnorm(y,main="Normal QQplot of y")
qqline(y) 

acf(y, 50, main="ACF of returns") 
print("Lag 1 out of bounds use of AR(1)")

pacf(y, 50, main="PACF of returns") 
print("Barely out of bounds maybe use MA(1) component")

acf(y^2,50, main="ACF of squared returns")
print("Out of bounds for lag 1,2")

pacf(y^2, 50, main="PACF of squared returns")
print("Out of bounds for lag 1,2,3")
print("Garch(1,1) might solve it but is possible that we have to go further")


#Independence Tests
Box.test(y,lag=12,type="Ljung") 
Box.test(y^2,lag=12,type="Ljung") 
print("Residuals are independently distributed")


# Normality Test
jarque.bera.test(y)
shapiro.test(y) 
print("Y not following a Normal distribution")





#################STEP 1:MULTIPLE REGRESSION###############
MRres <- lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9)
summary(MRres)

# Diagnostic tests for the residuals
# Autocorrelation of the residuals
acf(MRres$residuals, 36)
pacf(MRres$residuals, 36)
print("Within Bounds")

# Autocorrelation of the squared residuals
acf(MRres$residuals^2, 36)
pacf(MRres$residuals^2, 36)
print("Lag 2 for acf, lags 2,4 for pacf")

# Normality test
jarque.bera.test(MRres$residuals)
shapiro.test(MRres$residuals) 
qqnorm(MRres$residuals) 
qqline(MRres$residuals) 
print("The residuals do not follow normal distribution")





#################STEP 3:ADD ARMA and/or GARCH (Full Variables)###############
X<-matrix(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9),ncol=9)   
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE, archm = TRUE, external.regressors = X),
                   distribution.model = "std"
)
spec

modelres <- ugarchfit(spec = spec, data = y)
modelres


modelres@fit$matcoef
infocriteria(modelres)



#################STEP 4:Updating ARMA-GARCH (Important Variables)###############
X<-matrix(cbind(x1,x3,x6,x8,x9),ncol=5)   
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                   distribution.model = "std"
)
spec

modelres <- ugarchfit(spec = spec, data = y)
modelres


modelres@fit$matcoef
infocriteria(modelres)



#################STEP 5:Further updating ARMA-GARCH (Important Variables)###############
X<-matrix(cbind(x1,x3,x8,x9),ncol=4)   
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                   distribution.model = "std"
)
spec

modelres <- ugarchfit(spec = spec, data = y)
modelres


modelres@fit$matcoef
infocriteria(modelres)



#################STEP 6:Further updating ARMA-GARCH (Important Variables)###############
X<-matrix(cbind(x1),ncol=1)  
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,0)),
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                   distribution.model = "std"
)
spec

modelres <- ugarchfit(spec = spec, data = y)
modelres


modelres@fit$matcoef
infocriteria(modelres)


#Check Residuals
# Extract residuals
res = modelres@fit$residuals


sigmaHat = modelres@fit$sigma
plot(sigmaHat)

#standardised residuals
res = res/sigmaHat

# Plot the residuals
plot(res, main="Residuals of the GARCH Model")

acf(res, main="ACF of Residuals")
pacf(res, main="PACF of Residuals")

acf(res^2, main="ACF of Squared Residuals")
pacf(res^2, main="PACF of Squared Residuals")







###########################Prediction(ARIMA prediction + no garch)##################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 24)]
x1_test <- x1[(length(x1) - 23):length(x1)]


y_train <- y[1:(length(y) - 24)]
y_test <- y[(length(y) - 23):length(y)]

# Create an empty vector to store predictions
predictions <- numeric(0)


# Iterate through the test set
for (i in 1:24) {
  
  X_train <- cbind(x1_train)
  
  # Fit an ARIMA model with multiple external regressors
  arima_model <- Arima(y_train, order = c(0, 0, 0), xreg = X_train)
  X_test_i <- cbind(x1_test[i])
  
  # Forecast the next value using the ARIMA model with multiple external regressors
  arima_forecast <- forecast(arima_model, xreg = X_test_i)
  next_value <- arima_forecast$mean[1]
  
  # Store the combined forecast
  predictions <- c(predictions, next_value)
  
  # Update the training set for the next iteration
  if(i < 24) {  # Prevents out of bounds error on the last iteration
    x1_train <- c(x1_train, x1_test[i])
    y_train <- c(y_train, y_test[i])
  }
}

# Print or store the predictions as needed
forecast <- predictions
print(forecast)

# Plot true vs. predicted values
plot(y_test, type = "l", col = "red", lwd = 2, ylab = "Observations", main = "True vs Predicted")
lines(forecast, type = "l", col = "blue", lwd = 2)


# Calculate errors
errors <- forecast - y_test
mae <- mean(abs(errors))
mape <- mean(abs(errors / y_test)) * 100
mse <- mean(errors^2)
rmse <- sqrt(mse)
cat("\n MAE:", mae, "\nMAPE:", mape, "\nMSE:", mse, "\nRMSE:", rmse,"\n")


#Save
write.xlsx(data.frame(Numeric_Variable = forecast), "WTI_M_ARIMA_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "WTI_M_ARIMA_errors.txt")







##################################Prediction(sGarch)########################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 24)]
x1_test <- x1[(length(x1) - 23):length(x1)]

y_train <- y[1:(length(y) - 24)]
y_test <- y[(length(y) - 23):length(y)]


# Create an empty vector to store predictions
predictions <- numeric(0)


# Iterate through the test set
for (i in 1:24) {
  X <- matrix(cbind(x1_train), ncol = 1) 
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)), 
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                     distribution.model = "std"
  )
  modelres <- ugarchfit(spec = spec, data = y_train)
  forecast <- ugarchforecast(modelres, n.ahead = 1, external.forecasts = list(mregfor = matrix(x1_test[i], ncol = 1)))#external.forecasts + mregfor
  predictions <- c(predictions, forecast@forecast$seriesFor[1])
  

  if(i < 24){  
    x1_train <- c(x1_train, x1_test[i])
    y_train <- c(y_train, y_test[i])
  }
  
}


# Print or store the predictions as needed
forecast <- predictions
print(forecast)
# Plot true vs. predicted values
plot(y_test, type = "l", col = "red", lwd = 2, ylab = "Observations", main = "True vs Predicted")
lines(forecast, type = "l", col = "blue", lwd = 2)


# Calculate errors
errors <- forecast - y_test
mae <- mean(abs(errors))
mape <- mean(abs(errors / y_test)) * 100
mse <- mean(errors^2)
rmse <- sqrt(mse)
cat("\n MAE:", mae, "\nMAPE:", mape, "\nMSE:", mse, "\nRMSE:", rmse,"\n")


#Save
write.xlsx(data.frame(Numeric_Variable = forecast), "WTI_M_sGARCH_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "WTI_M_sGARCh_errors.txt")


















#################Residuals eGARCH###############
X<-matrix(cbind(x1,x3),ncol=2)  
spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder=c(1,1)),
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                   distribution.model = "std"
)
spec

modelres <- ugarchfit(spec = spec, data = y)
modelres


modelres@fit$matcoef
infocriteria(modelres)


#Check Residuals
# Extract residuals
res = modelres@fit$residuals


sigmaHat = modelres@fit$sigma
plot(sigmaHat)

#standardised residuals
res = res/sigmaHat

# Plot the residuals
plot(res, main="Residuals of the GARCH Model")

acf(res, main="ACF of Residuals")
pacf(res, main="PACF of Residuals")

acf(res^2, main="ACF of Squared Residuals")
pacf(res^2, main="PACF of Squared Residuals")


###########################Prediction(eGARCH)##################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 24)]
x1_test <- x1[(length(x1) - 23):length(x1)]

x3_train <- x3[1:(length(x3) - 24)]
x3_test <- x3[(length(x3) - 23):length(x3)]

y_train <- y[1:(length(y) - 24)]
y_test <- y[(length(y) - 23):length(y)]


# Create an empty vector to store predictions
predictions <- numeric(0)


# Iterate through the test set
for (i in 1:24) {
  X <- matrix(cbind(x1_train,x3_train), ncol = 2) 
  spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), 
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                     distribution.model = "std"
  )
  modelres <- ugarchfit(spec = spec, data = y_train)
  forecast <- ugarchforecast(modelres, n.ahead = 1, external.forecasts = list(mregfor = matrix(cbind(x1_test[i],x3_test[i]), ncol = 2)))#external.forecasts + mregfor
  predictions <- c(predictions, forecast@forecast$seriesFor[1])
  
  
  if(i < 24){  
    x1_train <- c(x1_train, x1_test[i])
    x3_train <- c(x3_train, x3_test[i])
    y_train <- c(y_train, y_test[i])
  }
  
}


# Print or store the predictions as needed
forecast <- predictions
print(forecast)
# Plot true vs. predicted values
plot(y_test, type = "l", col = "red", lwd = 2, ylab = "Observations", main = "True vs Predicted")
lines(forecast, type = "l", col = "blue", lwd = 2)


# Calculate errors
errors <- forecast - y_test
mae <- mean(abs(errors))
mape <- mean(abs(errors / y_test)) * 100
mse <- mean(errors^2)
rmse <- sqrt(mse)
cat("\n MAE:", mae, "\nMAPE:", mape, "\nMSE:", mse, "\nRMSE:", rmse,"\n")


#Save
write.xlsx(data.frame(Numeric_Variable = forecast), "WTI_M_eGARCH_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "WTI_M_eGARCh_errors.txt")







#################Residuals gjrGARCH###############
X<-matrix(cbind(x1,x3),ncol=2)  
spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder=c(1,1)),
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                   distribution.model = "norm"
)
spec

modelres <- ugarchfit(spec = spec, data = y)
modelres


modelres@fit$matcoef
infocriteria(modelres)


#Check Residuals
# Extract residuals
res = modelres@fit$residuals


sigmaHat = modelres@fit$sigma
plot(sigmaHat)

#standardised residuals
res = res/sigmaHat

# Plot the residuals
plot(res, main="Residuals of the GARCH Model")

acf(res, main="ACF of Residuals")
pacf(res, main="PACF of Residuals")

acf(res^2, main="ACF of Squared Residuals")
pacf(res^2, main="PACF of Squared Residuals")


###########################Prediction(gjrGARCH)##################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 24)]
x1_test <- x1[(length(x1) - 23):length(x1)]

x3_train <- x3[1:(length(x3) - 24)]
x3_test <- x3[(length(x3) - 23):length(x3)]

y_train <- y[1:(length(y) - 24)]
y_test <- y[(length(y) - 23):length(y)]


# Create an empty vector to store predictions
predictions <- numeric(0)


# Iterate through the test set
for (i in 1:24) {
  X <- matrix(cbind(x1_train,x3_train), ncol = 2) 
  spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), #11
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),#00
                     distribution.model = "norm"#std
  )
  modelres <- ugarchfit(spec = spec, data = y_train)
  forecast <- ugarchforecast(modelres, n.ahead = 1, external.forecasts = list(mregfor = matrix(cbind(x1_test[i],x3_test[i]), ncol = 2)))#external.forecasts + mregfor
  predictions <- c(predictions, forecast@forecast$seriesFor[1])
  
  
  if(i < 24){  
    x1_train <- c(x1_train, x1_test[i])
    x3_train <- c(x3_train, x3_test[i])
    y_train <- c(y_train, y_test[i])
  }
  
}


# Print or store the predictions as needed
forecast <- predictions
print(forecast)
# Plot true vs. predicted values
plot(y_test, type = "l", col = "red", lwd = 2, ylab = "Observations", main = "True vs Predicted")
lines(forecast, type = "l", col = "blue", lwd = 2)


# Calculate errors
errors <- forecast - y_test
mae <- mean(abs(errors))
mape <- mean(abs(errors / y_test)) * 100
mse <- mean(errors^2)
rmse <- sqrt(mse)
cat("\n MAE:", mae, "\nMAPE:", mape, "\nMSE:", mse, "\nRMSE:", rmse,"\n")



#Save
write.xlsx(data.frame(Numeric_Variable = forecast), "WTI_M_gjrGARCH_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "WTI_M_gjrGARCh_errors.txt")











#################Residuals iGARCH###############
X<-matrix(cbind(x1,x3),ncol=2)  
spec <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder=c(1,1)),
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                   distribution.model = "norm"
)
spec

modelres <- ugarchfit(spec = spec, data = y)
modelres


modelres@fit$matcoef
infocriteria(modelres)


#Check Residuals
# Extract residuals
res = modelres@fit$residuals


sigmaHat = modelres@fit$sigma
plot(sigmaHat)

#standardised residuals
res = res/sigmaHat

# Plot the residuals
plot(res, main="Residuals of the GARCH Model")

acf(res, main="ACF of Residuals")
pacf(res, main="PACF of Residuals")

acf(res^2, main="ACF of Squared Residuals")
pacf(res^2, main="PACF of Squared Residuals")





##########################Prediction(iGARCH)##################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 24)]
x1_test <- x1[(length(x1) - 23):length(x1)]

x3_train <- x3[1:(length(x3) - 24)]
x3_test <- x3[(length(x3) - 23):length(x3)]

y_train <- y[1:(length(y) - 24)]
y_test <- y[(length(y) - 23):length(y)]


# Create an empty vector to store predictions
predictions <- numeric(0)


# Iterate through the test set
for (i in 1:24) {
  X <- matrix(cbind(x1_train,x3_train), ncol = 2) 
  spec <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), 
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                     distribution.model = "std"
  )
  modelres <- ugarchfit(spec = spec, data = y_train)
  forecast <- ugarchforecast(modelres, n.ahead = 1, external.forecasts = list(mregfor = matrix(cbind(x1_test[i],x3_test[i]), ncol = 2)))#external.forecasts + mregfor
  predictions <- c(predictions, forecast@forecast$seriesFor[1])
  
  
  if(i < 24){  
    x1_train <- c(x1_train, x1_test[i])
    x3_train <- c(x3_train, x3_test[i])
    y_train <- c(y_train, y_test[i])
  }
  
}


# Print or store the predictions as needed
forecast <- predictions
print(forecast)
# Plot true vs. predicted values
plot(y_test, type = "l", col = "red", lwd = 2, ylab = "Observations", main = "True vs Predicted")
lines(forecast, type = "l", col = "blue", lwd = 2)


# Calculate errors
errors <- forecast - y_test
mae <- mean(abs(errors))
mape <- mean(abs(errors / y_test)) * 100
mse <- mean(errors^2)
rmse <- sqrt(mse)
cat("\n MAE:", mae, "\nMAPE:", mape, "\nMSE:", mse, "\nRMSE:", rmse,"\n")



#Save
write.xlsx(data.frame(Numeric_Variable = forecast), "WTI_M_iGARCH_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "WTI_M_iGARCh_errors.txt")



