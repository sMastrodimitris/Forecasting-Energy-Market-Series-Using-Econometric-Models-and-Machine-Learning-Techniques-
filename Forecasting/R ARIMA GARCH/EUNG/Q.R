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
dim(EUNG_Q)
y <- EUNG_Q$EUNG
x1 <- EUNG_Q$EUNG_LAG1
x2 <- EUNG_Q$EUNG_LAG2
x3 <- EUNG_Q$EUNG_LAG3
x4 <- EUNG_Q$EUNG_LAG4
x5 <- EUNG_Q$USAGDP
x6 <- EUNG_Q$`Equity Market Volatility`
x7 <- EUNG_Q$"3 Month Treasury Bill"
x8 <- EUNG_Q$"Infectious Disease Tracker"
x9 <- EUNG_Q$COV19
x10 <- EUNG_Q$RUWAR





##################CREATE TIMESERIES###################
# Create a time series object and plot
y=ts(y, frequency=4, start = c(1996,07)) #end = c(2023,07)
y
plot(y, type="l", col="blue")


###############EXPLORE LINEAR RELATIONSHIPS############
# Correlation coefficients
cor(cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))
# Correlation coefficients and p-values
rcorr(as.matrix(cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))) 
# Scatterplot of all variables
pairs(cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))



##################SELECTING FEATURES###################
# 1a.RUN MULTIPLE REGRESSION MODEL SELECTION TECHNIQUES
# Fit the full model
fitall <- lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
summary(fitall)


# Backward Elimination method
stepBE<-step(fitall, 
             scope=list(lower = ~ 1,
                        upper = ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10),
             direction="backward")
stepBE


# Forward Selection method
fitnull<-lm(y ~ 1)
stepFS<-step(fitnull, 
             scope=list(lower = ~ 1,
                        upper = ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10),
             direction="forward")
stepFS



# Stepwise Selection method
stepSS<-step(fitnull, 
             scope=list(lower = ~ 1,
                        upper = ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10),
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
print("Lags 1,5,6,9,10 out of bounds")

pacf(y, 50, main="PACF of returns") 
print("Lags 1,5,6,9 out of bounds")

acf(y^2,50, main="ACF of squared returns")
print("Many out of bounds")

pacf(y^2, 50, main="PACF of squared returns")
print("Lags 1,3,4 out of bounds")


#Independence Tests
Box.test(y,lag=12,type="Ljung") 
Box.test(y^2,lag=12,type="Ljung") 
print("No autocorrelation and no partial autocorrelation")


# Normality Test
jarque.bera.test(y)
shapiro.test(y) 
print("Y not following a Normal distribution")





#################STEP 1:MULTIPLE REGRESSION###############
MRres <- lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
summary(MRres)

# Diagnostic tests for the residuals
# Autocorrelation of the residuals
acf(MRres$residuals, 16)
pacf(MRres$residuals, 16)
print("6,9 out of bounds")

# Autocorrelation of the squared residuals
acf(MRres$residuals^2, 16)
print("1,3,9 out of bounds")
pacf(MRres$residuals^2, 16)
print("Lag 9 out of bounds")

# Normality test
jarque.bera.test(MRres$residuals)
shapiro.test(MRres$residuals) 
qqnorm(MRres$residuals) 
qqline(MRres$residuals) 
print("The residuals do not follow normal distribution")





#################STEP 3:ADD ARMA and/or GARCH (Full Variables)###############
X<-matrix(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),ncol=10)   
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
X<-matrix(cbind(x1,x3,x4,x7,x8,x9,x10),ncol=7)   
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
X<-matrix(cbind(x1,x3,x4,x7,x10),ncol=5)   
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                   distribution.model = "std"
)
spec

modelres <- ugarchfit(spec = spec, data = y)
modelres


modelres@fit$matcoef
infocriteria(modelres)



###############Step Vrontos: Comparison with simple Regression####################################
X<-matrix(cbind(x1,x2,x3,x9),ncol=4)   
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(0,2), include.mean = TRUE, archm = FALSE, external.regressors = X),
                   distribution.model = "sstd"
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
res = res/sigmaHat

# Plot the residuals
plot(res, main="Residuals of the GARCH Model")

acf(res, main="ACF of Residuals")
pacf(res, main="PACF of Residuals")

acf(res^2, main="ACF of Squared Residuals")
pacf(res^2, main="PACF of Squared Residuals")



qqnorm(res,main="Normal QQplot of MA(2),GARCH(1,1) Regression")
qqline(res)

# Normality Test
jarque.bera.test(res)
shapiro.test(res) 
print("Y not following a Normal distribution")













#################STEP 6:Further updating ARMA-GARCH (Important Variables)###############
X<-matrix(cbind(x1,x2,x7),ncol=3)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(1,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
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
res = res/sigmaHat

# Plot the residuals
plot(res, main="Residuals of the GARCH Model")

acf(res, main="ACF of Residuals")
pacf(res, main="PACF of Residuals")

acf(res^2, main="ACF of Squared Residuals")
pacf(res^2, main="PACF of Squared Residuals")



# Ljung-Box test on the residuals
Box.test(res, lag = 10, type = "Ljung-Box")

# Ljung-Box test on the residuals
Box.test(res, lag = 11, type = "Ljung-Box")

# Ljung-Box test on the residuals
Box.test(res, lag = 13, type = "Ljung-Box")



# Perform the Ljung-Box test
box_test_result <- Box.test(res, lag = 10, type = "Ljung-Box")

# Extract relevant components
test_statistic <- box_test_result$statistic
p_value <- box_test_result$p.value
degrees_of_freedom <- box_test_result$parameter

# Combine components into a matrix
result_matrix <- matrix(c(test_statistic, p_value, degrees_of_freedom), nrow = 1, byrow = TRUE,
                        dimnames = list(c("Box Test Result"), c("Test Statistic", "P-value", "Degrees of Freedom")))

# Print the matrix
print(result_matrix)


qqnorm(res,main="Normal QQplot of y")
qqline(res)

# Normality Test
jarque.bera.test(res)
shapiro.test(res) 
print("Y not following a Normal distribution")




###########################Prediction(ARIMA prediction + no garch)##################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 8)]
x1_test <- x1[(length(x1) - 7):length(x1)]

x2_train <- x2[1:(length(x2) - 8)]
x2_test <- x2[(length(x2) - 7):length(x2)]

x7_train <- x7[1:(length(x7) - 8)]
x7_test <- x7[(length(x7) - 7):length(x7)]

y_train <- y[1:(length(y) - 8)]
y_test <- y[(length(y) - 7):length(y)]

# Create an empty vector to store predictions
predictions <- numeric(0)


# Iterate through the test set
for (i in 1:8) {
  
  X_train <- cbind(x1_train, x2_train, x7_train)
  colnames(X_train) <- c("x1", "x2", "x7")  
  
  # Fit an ARIMA model with multiple external regressors
  arima_model <- Arima(y_train, order = c(1, 0, 0), xreg = X_train)
  
  X_test_i <- cbind(x1_test[i], x2_test[i], x7_test[i])
  colnames(X_test_i) <- c("x1", "x2", "x7")  
  
  # Forecast the next value using the ARIMA model with multiple external regressors
  arima_forecast <- forecast(arima_model, xreg = X_test_i)
  next_value <- arima_forecast$mean[1]
  
  # Store the combined forecast
  predictions <- c(predictions, next_value)
  
  # Update the training set for the next iteration
  if(i < 8) {  # Prevents out of bounds error on the last iteration
    x1_train <- c(x1_train, x1_test[i])
    x2_train <- c(x2_train, x2_test[i])
    x7_train <- c(x7_train, x7_test[i])
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
write.xlsx(data.frame(Numeric_Variable = forecast), "EUNG_Q_ARIMA_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "EUNG_Q_ARIMA_errors.txt")







##################################Prediction(sGarch)########################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 8)]
x1_test <- x1[(length(x1) - 7):length(x1)]

x2_train <- x2[1:(length(x2) - 8)]
x2_test <- x2[(length(x2) - 7):length(x2)]

x7_train <- x7[1:(length(x7) - 8)]
x7_test <- x7[(length(x7) - 7):length(x7)]

y_train <- y[1:(length(y) - 8)]
y_test <- y[(length(y) - 7):length(y)]


# Create an empty vector to store predictions
predictions <- numeric(0)

# Iterate through the test set
for (i in 1:8) {
  X <- matrix(cbind(x1_train,x2_train,x7_train), ncol = 3) 
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                     mean.model = list(armaOrder = c(1,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                     distribution.model = "sstd"
  )
  modelres <- ugarchfit(spec = spec, data = y_train)
  forecast <- ugarchforecast(modelres, n.ahead = 1, external.forecasts = list(mregfor = matrix(c(x1_test[i], x2_test[i], x7_test[i]), ncol = 3)))
  predictions <- c(predictions, forecast@forecast$seriesFor[1])
  
  
  if(i < 8){  
    x1_train <- c(x1_train, x1_test[i])
    x2_train <- c(x2_train, x2_test[i])
    x7_train <- c(x7_train, x7_test[i])
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
write.xlsx(data.frame(Numeric_Variable = forecast), "EUNG_Q_sGARCH_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "EUNG_Q_sGARCh_errors.txt")














#################Residual errors eGARCH###############
X<-matrix(cbind(x1,x7),ncol=2)
spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder=c(0,1)), 
                   mean.model = list(armaOrder=c(2,2), include.mean = TRUE, archm = FALSE, external.regressors = X),
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
res = res/sigmaHat

# Plot the residuals
plot(res, main="Residuals of the GARCH Model")

acf(res, main="ACF of Residuals")
pacf(res, main="PACF of Residuals")

acf(res^2, main="ACF of Squared Residuals")
pacf(res^2, main="PACF of Squared Residuals")






###########################Prediction(eGarch)##################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 8)]
x1_test <- x1[(length(x1) - 7):length(x1)]

x7_train <- x7[1:(length(x7) - 8)]
x7_test <- x7[(length(x7) - 7):length(x7)]

y_train <- y[1:(length(y) - 8)]
y_test <- y[(length(y) - 7):length(y)]


# Create an empty vector to store predictions
predictions <- numeric(0)

# Iterate through the test set
for (i in 1:8) {
  X <- matrix(cbind(x1_train,x7_train), ncol = 2) 
  spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(0, 1)), 
                     mean.model = list(armaOrder = c(2,2), include.mean = TRUE, archm = FALSE, external.regressors = X),
                     distribution.model = "norm"
  )
  modelres <- ugarchfit(spec = spec, data = y_train)
  forecast <- ugarchforecast(modelres, n.ahead = 1, external.forecasts = list(mregfor = matrix(c(x1_test[i], x7_test[i]), ncol = 2)))
  predictions <- c(predictions, forecast@forecast$seriesFor[1])
  
  
  if(i < 8){  
    x1_train <- c(x1_train, x1_test[i])
    x7_train <- c(x7_train, x7_test[i])
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
write.xlsx(data.frame(Numeric_Variable = forecast), "EUNG_Q_eGARCH_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "EUNG_Q_eGARCh_errors.txt")











#################Residuals gjrGARCH###############
X<-matrix(cbind(x1),ncol=1)
spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(1,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
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
res = res/sigmaHat

# Plot the residuals
plot(res, main="Residuals of the GARCH Model")

acf(res, main="ACF of Residuals")
pacf(res, main="PACF of Residuals")

acf(res^2, main="ACF of Squared Residuals")
pacf(res^2, main="PACF of Squared Residuals")






###########################Prediction(gjrGARCH)##################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 8)]
x1_test <- x1[(length(x1) - 7):length(x1)]

y_train <- y[1:(length(y) - 8)]
y_test <- y[(length(y) - 7):length(y)]


# Create an empty vector to store predictions
predictions <- numeric(0)

# Iterate through the test set
for (i in 1:8) {
  X <- matrix(cbind(x1_train), ncol = 1) 
  spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), submodel="GJRGARCH"), 
                     mean.model = list(armaOrder = c(1,0), include.mean = TRUE, archm = FALSE, external.regressors = X),
                     distribution.model = "norm"
  )
  modelres <- ugarchfit(spec = spec, data = y_train)
  forecast <- ugarchforecast(modelres, n.ahead = 1, external.forecasts = list(mregfor = matrix(c(x1_test[i]), ncol = 1)))
  predictions <- c(predictions, forecast@forecast$seriesFor[1])
  
  
  if(i < 8){  
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
write.xlsx(data.frame(Numeric_Variable = forecast), "EUNG_Q_gjrGARCH_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "EUNG_Q_gjrGARCh_errors.txt")



















#################Residual errors iGARCH###############
X<-matrix(cbind(x1,x7),ncol=2)
spec <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(3,2), include.mean = TRUE, archm = FALSE, external.regressors = X),
                   distribution.model = "ged"
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
res = res/sigmaHat

# Plot the residuals
plot(res, main="Residuals of the GARCH Model")

acf(res, main="ACF of Residuals")
pacf(res, main="PACF of Residuals")

acf(res^2, main="ACF of Squared Residuals")
pacf(res^2, main="PACF of Squared Residuals")



# Ljung-Box test on the residuals
Box.test(res, lag = 13, type = "Ljung-Box")


# Ljung-Box test on the residuals
Box.test(res^2, lag = 10, type = "Ljung-Box")


qqnorm(res,main="Normal QQplot of y")
qqline(res)

# Normality Test
jarque.bera.test(res)
shapiro.test(res) 
print("Y not following a Normal distribution")











###########################Prediction(iGARCH)##################################
# Initialize train and test sets
x1_train <- x1[1:(length(x1) - 8)]
x1_test <- x1[(length(x1) - 7):length(x1)]

x7_train <- x7[1:(length(x5) - 8)]
x7_test <- x7[(length(x5) - 7):length(x7)]

y_train <- y[1:(length(y) - 8)]
y_test <- y[(length(y) - 7):length(y)]


# Create an empty vector to store predictions
predictions <- numeric(0)

# Iterate through the test set
for (i in 1:8) {
  X <- matrix(cbind(x1_train,x7_train), ncol = 2) 
  spec <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), 
                     mean.model = list(armaOrder = c(3,2), include.mean = TRUE, archm = FALSE, external.regressors = X),
                     distribution.model = "ged"
  )
  modelres <- ugarchfit(spec = spec, data = y_train)
  forecast <- ugarchforecast(modelres, n.ahead = 1, external.forecasts = list(mregfor = matrix(c(x1_test[i], x7_test[i]), ncol = 2)))
  predictions <- c(predictions, forecast@forecast$seriesFor[1])
  
  
  if(i < 8){  
    x1_train <- c(x1_train, x1_test[i])
    x7_train <- c(x7_train, x7_test[i])
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
write.xlsx(data.frame(Numeric_Variable = forecast), "EUNG_Q_iGARCH_pred.xlsx", rowNames = FALSE)
cat(paste(mae, mape, mse, rmse, sep = "\n"), file = "EUNG_Q_iGARCh_errors.txt")

