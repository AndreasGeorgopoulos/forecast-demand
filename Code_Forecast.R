library(dplyr)
library(forecast)
library(tseries)

#############################################################################
############################### Preprocessing ###############################
#############################################################################

store_restaurant <- read.csv("store_restaurant.csv")
pos_ordersale <- read.csv("pos_ordersale.csv")
menuitem <- read.csv("menuitem.csv")
menu_items <- read.csv("menu_items.csv")
ingredients <- read.csv("ingredients.csv")
recipes <- read.csv("recipes.csv")
recipe_ingredient_assignments <- read.csv("recipe_ingredient_assignments.csv")
sub_recipes <- read.csv("sub_recipes.csv")
sub_recipe_ingr_assignments <- read.csv("sub_recipe_ingr_assignments.csv")
recipe_sub_recipe_assignments <- read.csv("recipe_sub_recipe_assignments.csv")
portion_uom_types <- read.csv("portion_uom_types.csv")

# Ingredient: lettuce characteristics
ingredients <- merge (ingredients,portion_uom_types)
ingredients[ingredients$IngredientId == 27,] #so lettuce in ounces

# FOR EACH RECIPE FIND TOTAL OUNCE OF LETTUCE

# recipes that include lettuce
recipes_total_lettuce <- recipe_ingredient_assignments[recipe_ingredient_assignments$IngredientId == 27,c(1,3)]

# subrecipes that include lettuce
sub_recipes_lettuce <- sub_recipe_ingr_assignments[sub_recipe_ingr_assignments$IngredientId == 27,]
# all recipes where their sub recipes iclude lettuce
recipes_where_subrecipes_lettuce <- inner_join(sub_recipes_lettuce, recipe_sub_recipe_assignments, by = "SubRecipeId")
# quantity of lettuce in subrecipe included in recipe: factor*quantity
recipes_where_subrecipes_lettuce$Lettuce_Quant <- recipes_where_subrecipes_lettuce$Factor * recipes_where_subrecipes_lettuce$Quantity
# total quantity of lettuce per recipe based on their subrecipes' quant of lettuce (one recipe can have multiple subrecipes)
recipes_subrec_total_lettuce <- aggregate( cbind(Lettuce_Quant) ~ RecipeId, sum, data = recipes_where_subrecipes_lettuce)

# total quantity of lettuce per recipe (either recipe or their subrecipes (or both) that include lettuce)
colnames(recipes_subrec_total_lettuce) <- c("RecipeId","Quantity")
a <- rbind(recipes_total_lettuce, recipes_subrec_total_lettuce)
total_lettuce_per_recipe <- aggregate(cbind(Quantity) ~ RecipeId, sum, data = a)
write.csv(total_lettuce_per_recipe, file ="total_lettuce_per_recipe.csv")


# FOR EACH RESTAURANT: find quantity of lettuce per day:

# find recipe id and lettuce_quantity per menuitem (keep menuitems that include lettuce only)
menuitem <- inner_join(menuitem, menu_items, by = c("Id" = "MenuItemId"))
menuitem <- inner_join(menuitem, total_lettuce_per_recipe, by = "RecipeId")
colnames(menuitem)[20] <- "Lettuce_Quantity"

# compute total amount of lettuce per transaction and menu item
menuitem$Total_Lettuce_Quantity <- menuitem$Quantity.x * menuitem$Lettuce_Quantity

# total lettuce quantity per day for each restaurant
store_4904  <- aggregate(cbind(Total_Lettuce_Quantity) ~ date, sum, data = menuitem[menuitem$StoreNumber == 4904, c(15,21)])
store_12631 <- aggregate(cbind(Total_Lettuce_Quantity) ~ date, sum, data = menuitem[menuitem$StoreNumber == 12631, c(15,21)])
store_20974 <- aggregate(cbind(Total_Lettuce_Quantity) ~ date, sum, data = menuitem[menuitem$StoreNumber == 20974, c(15,21)])
store_46673 <- aggregate(cbind(Total_Lettuce_Quantity) ~ date, sum, data = menuitem[menuitem$StoreNumber == 46673, c(15,21)])

write.csv(store_4904, file ="store_4904.csv")
write.csv(store_12631, file ="store_12631.csv")
write.csv(store_20974, file ="store_20974.csv")
write.csv(store_46673, file ="store_46673.csv")

# remove first 6 date-data from store 20974 (sequential missing data)
store_20974 <- store_20974[-c(1:6),]




#############################################################################
################################# Store 4904 ################################
#############################################################################


# Holt-Winters --------------------------------------------------------------

# Convert the numerical vector to time series object
store_4904_ts <- ts(store_4904[, 2], frequency = 7, start = c(03,13))
# Plot of time-series
plot.ts(store_4904_ts, main="Daily lettuce demand of store 4904", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", col = "darkred", frame.plot = FALSE)

# Seasonal Decomposition of Time Series by Loess stl()
plot(stl(store_4904_ts, s.window = "period"), main="Seasonal Decomposition of Time Series Lettuce Demand of Store 4904", xaxt = "n")  # SO FOR THIS TIME SERIES WE CAN IGNORE trend FACTORS SINCE (GREY BAR VERY LONG (and thus variation of discomponent is very small)

# HoltWinters function: (find optimal a,b,g, by min sum of square errors)
store_4904_HW <- HoltWinters(store_4904_ts, beta = FALSE) 

#ETS (max likelihood approach and optimises a,b,g and also initial states) Exponential smoothing state space model
store_4904_ets <- ets(store_4904_ts, model = "ZZZ")    #(A,N,A) initial states (here no trend so it gives initial level and seasonality)

# Plot in-sample performance
plot(store_4904_HW, main="Holt-Winters - ETS (A,N,A) in-sample performance", xlab="Time Horizon", ylab="Estimated (fitted) values of lettuce demand (ounces)", lty = 1, col = "black", frame.plot = FALSE)            #dark line:original data, red line: our estimation
lines(fitted(store_4904_ets), col = "blue", lty = 2)
legend("bottom", legend=c("Actual","HW", "ETS"), col=c("black", "red", "blue"), box.lty=0, lty=c(1,1,2), cex=0.8)

# in -sample performance measures table
sqrt(store_4904_HW$SSE/(length(store_4904_ts)-2)) #sum of square erros / by number of observation-2 (we used first two for initilisation)
accuracy(store_4904_ets)

# HW with initial states
store_4904_HW2 <- HoltWinters(store_4904_ts, beta = FALSE, optim.start = c(alpha = 0.1, gamma = 0), l.start = 320, s.start = c(5,47,56,36,37,-89,-91))

# RMSE 
sqrt(store_4904_HW2$SSE/(length(store_4904_ts)-2)) 

# Training Set (80%, first 76 days)
training_set_4904_ts   <- ts(store_4904[1:(nrow(store_4904)*0.8),2], frequency = 7)
# Validation Set (20%, last 19 days)
validation_set_4904 <- store_4904[(nrow(store_4904)*0.8+1):nrow(store_4904),2] #  as.character(store_4904[nrow(store_4904)*0.8+1,1])

# HW forecast based on training set
store_4904_HW_training <- HoltWinters(training_set_4904_ts, beta = FALSE) 
store_4904_HW1_forecast <- forecast.HoltWinters(store_4904_HW_training, h = 19) #we forecast for the next 6 months h=6

# ETS forecast based on training set
store_4904_ets_training <- ets(training_set_4904_ts, model = "ANA")    #(A,N,A) initial states (here no trend so it gives initial level and seasonality)
store_4904_ets_forecast <- forecast.ets(store_4904_ets_training, h = 19)

# Plot out-of sample HW
plot(store_4904_HW1_forecast, main = "Forecast from HoltWinters on Training Set", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_4904_HW1_forecast), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# Plot out-of-sample ETS
plot(store_4904_ets_forecast, main = "Forecast from ETS(A,N,A) on Training Set", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_4904_ets_forecast), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# out-of-sample performance (forecast power comparison)
accuracy(store_4904_HW1_forecast, validation_set_4904) #  on 
accuracy(store_4904_ets_forecast, validation_set_4904)

# best model forecast (train on thw whole dataset)
store_4904_ets_f <- forecast.ets(store_4904_ets, h = 14)
# Plot 
plot(store_4904_ets_f, main = "Final Forecast from ETS(A,N,A) for next 14 days", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_4904_ets_f), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# Check for forecasted errors - further imporvement
# plot ACF 
plot(acf(store_4904_ets_f$residuals, lag.max=20), main = "Autocorrelation Function of ETS(A,N,A) Residuals", frame.plot = FALSE) #  autocorrelation on residuals 
# Test Ljung–Box (autocorrelation?)
Box.test(store_4904_ets_f$residuals, lag=20, type="Ljung-Box")
# Plot residuals (constant variance?)
plot.ts(store_4904_ets_f$residuals, main="ETS(A,N,A) in-sample Forecast Errors", xlab="Time Horizon", ylab="Residuals of ETS (A,N,A)", col = "darkred", frame.plot = FALSE)
# Histogram of forecast errors (normally distributed?)
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a  histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="lightgrey", freq=FALSE, breaks=mybins, main = "Distribution of Forecast Errors on the ETS (A,N,A)", xlab = "Foreacst Errors", "Density")
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="darkred", lwd=2)
}
# plot the normal curve:
plotForecastErrors(store_4904_ets_f$residuals) 


#ARIMA 4904

# identification
plot.ts(store_4904_ts)

# test if time series is stationary
adf.test(store_4904_ts)
kpss.test(store_4904_ts)
pp.test(store_4904_ts)
ndiffs(store_4904_ts) #  seasonal differencing
nsdiffs(store_4904_ts, m = 7, test = "ocsb", max.D = 1) #  Osborn-Chui-Smith-Birchenhall (1988) test is used (with null hypothesis that a seasonal unit root exists)

# first difference
store_4904_ts_diff1 <- diff(store_4904_ts, differences = 1, lag = frequency(store_4904_ts))
plot.ts(store_4904_ts_diff1)

# determine p and q
plot(acf(store_4904_ts_diff1, lag.max = 20), main = "Autocorrelation Function of Seasonal Differenced Time-Series", frame.plot = FALSE) 
plot(pacf(store_4904_ts_diff1, lag.max = 20), main = " Partial Autocorrelation Function of Seasonal Differenced Time-Series", frame.plot = FALSE) 

auto.arima(store_4904_ts, trace = TRUE, ic = 'bic', approximation = FALSE) #Best model ARIMA(1,0,1) (0,1,1)[7]

# estimation
store_4904_arima1 <- Arima(store_4904_ts, order = c(1, 0, 6),seasonal = list(order = c(0, 1, 1), period = 7), include.drift = FALSE)
store_4904_arima2 <- Arima(store_4904_ts, order = c(1, 0, 1),seasonal = list(order = c(0, 1, 1), period = 7), include.drift = FALSE) 
store_4904_arima3 <- Arima(store_4904_ts, order = c(1, 0, 3),seasonal = list(order = c(0, 1, 1), period = 7), include.drift = FALSE)

# forecast
store_4904_forecast_arima1 <- forecast.Arima(store_4904_arima1, h = 14)
store_4904_forecast_arima2 <- forecast.Arima(store_4904_arima2, h = 14)
store_4904_forecast_arima3 <- forecast.Arima(store_4904_arima3, h = 14)

# plot in sample
plot.ts(store_4904_ts, main="ARIMA in-sample performance", xlab="Time Horizon", ylab="Estimated (fitted) values of lettuce demand (ounces)", lty = 1, col = "black", frame.plot = FALSE)            #dark line:original data, red line: our estimation
lines((fitted(store_4904_forecast_arima1)), col = "red", lty = 2)
lines(fitted(store_4904_forecast_arima2), col = "blue", lty = 2)
lines(fitted(store_4904_forecast_arima3), col = "darkgoldenrod", lty = 1)
legend("bottom", title = "Model:", legend=c("Actual", "ARIMA(1,0,6)(0,1,1)","ARIMA(1,0,1)(0,1,1)", "ARIMA(1,0,3)(0,1,1)"), col=c("black", "red", "blue", "darkgoldenrod"), box.lty=0, lty=c(1,2,2,1), cex=0.8)

# accuracy 
accuracy(store_4904_forecast_arima1)
accuracy(store_4904_forecast_arima2)
accuracy(store_4904_forecast_arima3)

# Training Set (80%, first 76 days)
training_set_4904_ts   <- ts(store_4904[1:(nrow(store_4904)*0.8),2], frequency = 7)
# Validation Set (20%, last 19 days)
validation_set_4904 <- store_4904[(nrow(store_4904)*0.8+1):nrow(store_4904),2] #  as.character(store_4904[nrow(store_4904)*0.8+1,1])

# train on training set
# forecast based on training set
store_4904_arima1_training <- Arima(training_set_4904_ts, order = c(1, 0, 6),seasonal = list(order = c(0, 1, 1), period = 7), include.drift = FALSE) # BIC = 752
store_4904_arima2_training <- Arima(training_set_4904_ts, order = c(1, 0, 1),seasonal = list(order = c(0, 1, 1), period = 7), include.drift = FALSE) # BIC = 744
store_4904_arima4_training <- Arima(training_set_4904_ts, order = c(1, 0, 3),seasonal = list(order = c(0, 1, 1), period = 7), include.drift = FALSE) # BIC = 749

# forecast
store_4904_forecast_arima1 <- forecast.Arima(store_4904_arima1_training, h = 18)
store_4904_forecast_arima2 <- forecast.Arima(store_4904_arima2_training, h = 18)
store_4904_forecast_arima4 <- forecast.Arima(store_4904_arima4_training, h = 18)

# out-of-sample performance (forecast power comparison)
accuracy(store_4904_forecast_arima1, validation_set_4904) #  so best model (1,0,6) (0,1,1)
accuracy(store_4904_forecast_arima2, validation_set_4904)
accuracy(store_4904_forecast_arima4, validation_set_4904)    

# best model forecast (train on thw whole dataset)
store_4904_arima_best <- Arima(store_4904_ts, order = c(1, 0, 3),seasonal = list(order = c(1, 1, 1), period = 7), include.drift = FALSE)
store_4904_forecast_arima_best <- forecast.Arima(store_4904_arima_best, h = 14)
# plot
plot(store_4904_forecast_arima_best, main = "Final Forecast from ARIMA(1,0,3)(0,1,1)[7] for next 14 days", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  
lines(fitted(store_4904_forecast_arima_best), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)


# verification
# we want to have a random sequence of the residuals!!!!!!!
plot.ts(store_4904_arima_best$residuals)           
acf(store_4904_arima_best$residuals, lag.max = 20) 
Box.test(store_4904_arima_best$residuals, lag = 20, type = "Ljung-Box") 

# Check for forecasted errors - further imporvement
# plot ACF 
plot(acf(store_4904_forecast_arima_best$residuals, lag.max=20), main = "Autocorrelation Function of ARIMA(1,0,3)(0,1,1) Residuals", frame.plot = FALSE) #  autocorrelation on residuals 
# Test Ljung–Box (autocorrelation?)
Box.test(store_4904_forecast_arima_best$residuals, lag=20, type="Ljung-Box")
# Plot residuals (constant variance?)
plot.ts(store_4904_forecast_arima_best$residuals, main="ARIMA(1,0,3)(0,1,1) in-sample Forecast Errors", xlab="Time Horizon", ylab="Residuals of ARIMA(1,0,3)(0,1,1)", col = "darkred", frame.plot = FALSE)
# Histogram of forecast errors (normally distributed?)
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a  histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="lightgrey", freq=FALSE, breaks=mybins, main = "Distribution of Forecast Errors on the ARIMA(1,0,3)(0,1,1)", xlab = "Foreacst Errors", "Density")
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="darkred", lwd=2)
}
# plot the normal curve:
plotForecastErrors(store_4904_forecast_arima_best$residuals) # make a histogram

# final selection ETS(A,N,A)
store_4904_final <- forecast.ets(store_4904_ets, h = 14)
# Plot 
plot(store_4904_final, main = "Forecasted Lettuce Demand of Restaurant 4904", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_4904_final), col = "red") 
legend("bottom", title = "Model: ETS(A,N,A)", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

write.csv(store_4904_final, "forecast_4904.csv")





#############################################################################
################################# Store 20974 ###############################
#############################################################################


# Holt-Winters --------------------------------------------------------------

# Convert the numerical vector to time series object
store_20974_ts <- ts(store_20974[, 2], frequency = 7, start = c(03,13))
# Plot of time-series
plot.ts(store_20974_ts, main="Daily lettuce demand of store 20974", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", col = "darkred", frame.plot = FALSE)

# Seasonal Decomposition of Time Series by Loess stl()
plot(stl(store_20974_ts, s.window = "period"), main="Seasonal Decomposition of Time Series Lettuce Demand of Store 20974", xaxt = "n")  # no seasonality, only trend
# HoltWinters function: (find optimal a,b,g, by min sum of square errors)
store_20974_HW <- HoltWinters(store_20974_ts, beta = FALSE) 

# ETS (max likelihood approach and optimises a,b,g and also initial states) Exponential smoothing state space model
store_20974_ets <- ets(store_20974_ts, model = "ZZZ")    #(A,N,A)

# Plot in-sample performance
plot(store_20974_HW, main="Holt-Winters - ETS (A,N,A) in-sample performance", xlab="Time Horizon", ylab="Estimated (fitted) values of lettuce demand (ounces)", lty = 1, col = "black", frame.plot = FALSE)            #dark line:original data, red line: our estimation
lines(fitted(store_20974_ets), col = "blue", lty = 2)
legend("bottom", title = "Store:", legend=c("Actual","HW", "ETS"), col=c("black", "red", "blue"), box.lty=0, lty=c(1,1,2), cex=0.8)

# in -sample performance measures table
sqrt(store_20974_HW$SSE/(length(store_20974_ts)-2)) #sum of square erros / by number of observation-2 (we used first two for initilisation)
accuracy(store_20974_ets)

# HW with initial states
store_20974_HW2 <- HoltWinters(store_20974_ts, beta = FALSE, optim.start = c(alpha = 0.15, gamma = 0), l.start = 198, s.start = c(21.94,29.8,11.42,16.93,0.34,-63.84,-16.58))
# RMSE 
sqrt(store_20974_HW2$SSE/(length(store_20974_ts)-2)) 


# Training Set (80%, first 70 days)
training_set_20974_ts   <- ts(store_20974[1:(nrow(store_20974)*0.8),2], frequency = 7)
# Validation Set (20%, last 18 days)
validation_set_20974 <- store_20974[(nrow(store_20974)*0.8+1):nrow(store_20974)+1,2] #  as.character(store_20974[nrow(store_20974)*0.8+1,1])

# HW forecast based on training set
store_20974_HW_training <- HoltWinters(training_set_20974_ts, beta = FALSE) 
store_20974_HW1_forecast <- forecast.HoltWinters(store_20974_HW_training, h = 18) # forecast for next 17

# ETS forecast based on training set
store_20974_ets_training <- ets(training_set_20974_ts, model = "ANA")    #(A,N,A) initial states (here no trend so it gives initial level and seasonality)
store_20974_ets_forecast <- forecast.ets(store_20974_ets_training, h = 18)

# Plot out-of sample HW
plot(store_20974_HW1_forecast, main = "Forecast from HoltWinters on Training Set", xlab="Time Horizon (days)", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_20974_HW1_forecast), col = "red") 
legend("bottom", title = "Store:", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# Plot out-of-sample ETS
plot(store_20974_ets_forecast, main = "Forecast from ETS(A,N,A) on Training Set", xlab="Time Horizon (days)", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_20974_ets_forecast), col = "red") 
legend("bottom", title = "Store:", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# out-of-sample performance (forecast power comparison)
accuracy(store_20974_HW1_forecast, validation_set_20974) #  on 
accuracy(store_20974_ets_forecast, validation_set_20974)

# best model forecast (train on thw whole dataset)
store_20974_ets_f <- forecast.ets(store_20974_ets, h = 14)
# Plot 
plot(store_20974_ets_f, main = "Final Forecast from ETS(A,N,A) for next 14 days", xlab="Time Horizon (days)", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_20974_ets_f), col = "red") 
legend("bottom", title = "Store:", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# Check for forecasted errors - further imporvement

# plot ACF 
plot(acf(store_20974_ets_f$residuals, lag.max=20), main = "Autocorrelation Function of ETS(A,N,A) Residuals", frame.plot = FALSE) #  autocorrelation on residuals 
# Test Ljung–Box (autocorrelation?)
Box.test(store_20974_ets_f$residuals, lag=20, type="Ljung-Box")
# Plot residuals (constant variance?)
plot.ts(store_20974_ets_f$residuals, main="ETS(A,N,A) in-sample Forecast Errors", xlab="Time Horizon", ylab="Residuals of ETS (A,N,A)", col = "darkred", frame.plot = FALSE)
# Histogram of forecast errors (normally distributed?)
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a  histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="lightgrey", freq=FALSE, breaks=mybins, main = "Distribution of Forecast Errors on the ETS (A,N,A)", xlab = "Foreacst Errors", "Density")
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="darkred", lwd=2)
}
# plot the normal curve:
plotForecastErrors(store_20974_ets_f$residuals) # make a histogram



# ARIMA 20974

# identification
plot.ts(store_20974_ts)

# first difference
store_20974_ts_diff1 <- diff(store_20974_ts, differences = 1)
store_20974_ts_diff2 <- diff(store_20974_ts_diff1, differences = 1)
plot.ts(store_20974_ts_diff1)
plot.ts(store_20974_ts_diff2)

# test if time series is stationary
adf.test(store_20974_ts)
kpss.test(store_20974_ts_diff1)
pp.test(store_20974_ts_diff1)
ndiffs(store_20974_ts) #  seasonal differencing

nsdiffs(store_20974_ts_diff1, m = 7, test = "ocsb", max.D = 2) #  Osborn-Chui-Smith-Birchenhall (1988) test is used (with null hypothesis that a seasonal unit root exists)

# determine p and q
plot(acf(store_20974_ts, lag.max=20), main = "Autocorrelation Function of Time Series", frame.plot = FALSE)          #  autocorrelation on residuals 
plot(pacf(store_20974_ts, lag.max=20), main = "Partial Autocorrelation Function of Time Series", frame.plot = FALSE) #  ACF sinusoidal + PACF vainshes after lag 1--> so AR part with p<=1  

auto.arima(store_20974_ts, trace = TRUE, ic = 'bic') #Best model FROM AUTO-ARIMA(0,0,0,) (1,0,0)[7]

# estimation
store_20974_arima1 <- Arima(store_20974_ts, order = c(0,0,0),seasonal = list(order = c(1, 0, 0), period = 7), include.drift = FALSE)
store_20974_arima3 <- Arima(store_20974_ts, order = c(0, 0, 1),seasonal = list(order = c(1, 0, 0), period = 7), include.drift = FALSE)
store_20974_arima2 <- Arima(store_20974_ts, order = c(0, 0, 0),seasonal = list(order = c(1, 0, 1), period = 7), include.drift = FALSE)

# forecast
store_20974_forecast_arima1 <- forecast.Arima(store_20974_arima1, h = 14)
store_20974_forecast_arima3 <- forecast.Arima(store_20974_arima3, h = 14)
store_20974_forecast_arima2 <- forecast.Arima(store_20974_arima2, h = 14)

# accuracy 
accuracy(store_20974_forecast_arima1)
accuracy(store_20974_forecast_arima3)
accuracy(store_20974_forecast_arima2)

# Training Set (80%, first 70 days)
training_set_20974_ts   <- ts(store_20974[1:(nrow(store_20974)*0.8),2], frequency = 7)
# Validation Set (20%, last 18 days)
validation_set_20974 <- store_20974[(nrow(store_20974)*0.8+1):nrow(store_20974)+1,2] #  as.character(store_20974[nrow(store_20974)*0.8+1,1])

# train on training set
# forecast based on training set
store_20974_arima1_training <- Arima(training_set_20974_ts, order = c(0,0,0),seasonal = list(order = c(1, 0, 0), period = 7), include.drift = FALSE)
store_20974_arima3_training <- Arima(training_set_20974_ts, order = c(0, 0, 1),seasonal = list(order = c(1, 0, 0), period = 7), include.drift = FALSE)
store_20974_arima2_training <- Arima(training_set_20974_ts, order = c(0, 0, 0),seasonal = list(order = c(1, 0, 1), period = 7), include.drift = FALSE)

# forecast
store_20974_forecast_arima1 <- forecast.Arima(store_20974_arima1_training, h = 18)
store_20974_forecast_arima3 <- forecast.Arima(store_20974_arima3_training, h = 18)
store_20974_forecast_arima2 <- forecast.Arima(store_20974_arima2_training, h = 18)

# out-of-sample performance (forecast power comparison)
accuracy(store_20974_forecast_arima1, validation_set_20974)  # BIC: BIC=759.97 AT IN-SAMPLE
accuracy(store_20974_forecast_arima3, validation_set_20974)   # 
accuracy(store_20974_forecast_arima2, validation_set_20974)   # 

# best model forecast (train on thw whole dataset)
store_20974_arima3 <- Arima(store_20974_ts, order = c(0, 0, 0),seasonal = list(order = c(1,0,0), period = 7), include.drift = FALSE)
store_20974_forecast_arima_best <- forecast.Arima(store_20974_arima3, h = 14)

# Plot 
plot(store_20974_forecast_arima_best, main = "Final Forecast from ARIMA(0,0,0)(1,0,0)[7] for next 14 days", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_20974_forecast_arima_best), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# verification --> check whether the model fit the data
plot.ts(store_20974_arima3$residuals)          
acf(store_20974_arima3$residuals, lag.max = 20) # we want that to be 0, we want no autocorrelation independencies in the residuals (if tehre are we can improve the model more)
# every line between the bound can be considered statistically equal to zero---> (first line is the variance so dn take into account)---->here we can see no autocorrelation
Box.test(store_20974_arima3$residuals, lag = 20, type = "Ljung-Box") 

# Check for forecasted errors - further imporvement

# plot ACF 
plot(acf(store_20974_forecast_arima_best$residuals, lag.max=20), main = "Autocorrelation Function of ARIMA(0,0,0)(1,0,0) Residuals", frame.plot = FALSE) #  autocorrelation on residuals 
# Test Ljung–Box (autocorrelation?)
Box.test(store_20974_forecast_arima_best$residuals, lag=20, type="Ljung-Box")
# Plot residuals (constant variance?)
plot.ts(store_20974_forecast_arima_best$residuals, main="ARIMA(0,0,0)(1,0,0) in-sample Forecast Errors", xlab="Time Horizon", ylab="Residuals of ARIMA(0,0,0)(1,0,0)", col = "darkred", frame.plot = FALSE)
# Histogram of forecast errors (normally distributed?)
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a  histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="lightgrey", freq=FALSE, breaks=mybins, main = "Distribution of Forecast Errors on the ARIMA(0,0,0)(1,0,0)", xlab = "Foreacst Errors", "Density")
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="darkred", lwd=2)
}
# plot the normal curve:
plotForecastErrors(store_20974_forecast_arima_best$residuals) # make a histogram

# final selection ETS(A,N,A)
store_20974_final <- forecast.ets(store_20974_ets, h = 14)
# Plot 
plot(store_20974_final, main = "Forecasted Lettuce Demand of Restaurant 20974", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_20974_ets_f), col = "red") 
legend("bottom", title = "Model: ETS(A,N,A)", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

write.csv(store_20974_final, "forecast_20974.csv")





#############################################################################
################################# Store 12631 ###############################
#############################################################################


# Holt-Winters --------------------------------------------------------------

# Convert the numerical vector to time series object
store_12631_ts <- ts(store_12631[, 2], frequency = 7, start = c(03,05))
# Plot of time-series
plot.ts(store_12631_ts, main="Daily lettuce demand of store 12631", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", col = "darkred", frame.plot = FALSE)

# Seasonal Decomposition of Time Series by Loess stl()
plot(stl(store_12631_ts, s.window = "period"), main="Seasonal Decomposition of Time Series Lettuce Demand of Store 12631", xaxt = "n")  # trend no seasonality
# HoltWinters function: (find optimal a,b,g, by min sum of square errors)
store_12631_HW <- HoltWinters(store_12631_ts, seasonal="multiplicative", beta = FALSE) 

#ETS (max likelihood approach and optimises a,b,g and also initial states) Exponential smoothing state space model
store_12631_ets <- ets(store_12631_ts, model = "ZZZ")    #(M,N,M) initial states (here no trend so it gives initial level and seasonality)

# Plot in-sample performance
plot(store_12631_HW, main="Holt-Winters (multiplicative) - ETS (M,N,M) in-sample performance", xlab="Time Horizon", ylab="Estimated (fitted) values of lettuce demand (ounces)", lty = 1, col = "black", frame.plot = FALSE)            #dark line:original data, red line: our estimation
lines(fitted(store_12631_ets), col = "blue", lty = 2)
legend("bottom", legend=c("Actual","HW", "ETS"), col=c("black", "red", "blue"), box.lty=0, lty=c(1,1,2), cex=0.8)

# in -sample performance measures table
sqrt(store_12631_HW$SSE/(length(store_12631_ts)-2)) #sum of square erros / by number of observation-2 (we used first two for initilisation)
accuracy(store_12631_ets)

# Training Set (80%, first 83 days)
training_set_12631_ts   <- ts(store_12631[1:83,2], frequency = 7)
# Validation Set (20%, last 20 days)
validation_set_12631 <- store_12631[84:nrow(store_12631),2] #  as.character(store_12631[nrow(store_12631)*0.8+1,1])

# HW forecast based on training set
store_12631_HW_training <- HoltWinters(training_set_12631_ts, seasonal="multiplicative", beta = FALSE) 
store_12631_HW_forecast <- forecast.HoltWinters(store_12631_HW_training, h = 20) # forecast for next 20 days

# ETS forecast based on training set
store_12631_ets_training <- ets(training_set_12631_ts, model = "MNM")    
store_12631_ets_forecast <- forecast.ets(store_12631_ets_training, h = 20)

# Plot out-of sample HW
plot(store_12631_HW_forecast, main = "Forecast from HoltWinters(multiplicative) on Training Set", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_12631_HW_forecast), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# Plot out-of-sample ETS
plot(store_12631_ets_forecast, main = "Forecast from ETS(M,N,M) on Training Set", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  
lines(fitted(store_12631_ets_forecast), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# out-of-sample performance (forecast power comparison)
accuracy(store_12631_HW_forecast, validation_set_12631) #  on 
accuracy(store_12631_ets_forecast, validation_set_12631)

# best model forecast (train on thw whole dataset)
store_12631_ets_f <- forecast.ets(store_12631_ets, h = 14)
# Plot 
plot(store_12631_ets_f, main = "Final Forecast from ETS(M,N,M) for next 14 days", xlab="Time Horizon ", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_12631_ets_f), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# Check for forecasted errors - further imporvement

# plot ACF 
plot(acf(store_12631_ets_f$residuals, lag.max=100), main = "Autocorrelation Function of ETS(M,N,M) Residuals", frame.plot = FALSE) #  autocorrelation on residuals 
# Test Ljung–Box (autocorrelation?)
Box.test(store_12631_ets_f$residuals, lag=20, type="Ljung-Box")
# Plot residuals (constant variance?)
plot.ts(store_12631_ets_f$residuals, main="ETS(M,N,M) in-sample Forecast Errors", xlab="Time Horizon", ylab="Residuals of ETS (M,N,M)", col = "darkred", frame.plot = FALSE)
# Histogram of forecast errors (normally distributed?)
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a  histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="lightgrey", freq=FALSE, breaks=mybins, main = "Distribution of Forecast Errors on the ETS (M,N,M)", xlab = "Foreacst Errors", "Density")
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="darkred", lwd=2)
}
plotForecastErrors(store_12631_ets_f$residuals) # make a histogram



# ARIMA --------------------------------------------------------------------

# identification
# not stationary and appears to have multilicative seasonlity--> so we take the logarithm form of the time series
plot.ts(store_12631_ts, main="Daily lettuce demand of initial time-series of store 12631", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", col = "darkred", frame.plot = FALSE)
# log transformation
store_12631_ts_log <- log(store_12631_ts)
plot.ts(store_12631_ts_log, main="Daily lettuce demand of time-series (Logarithm Form) of store 12631", xlab="Time Horizon", ylab="Logarithm of Lettuce quantity (ounces)", col = "darkred", frame.plot = FALSE)

# first difference
store_12631_ts_log_diff <- diff(store_12631_ts_log, differences = 1)
plot.ts(store_12631_ts_log_diff)
plot.ts(store_12631_ts_log_diff, main="First differenced time-series (Logarithm Form) of store 12631", xlab="Time Horizon", ylab="Logarithm of Lettuce quantity (ounces)", col = "darkred", frame.plot = FALSE)

# test if time series is stationary
adf.test(store_12631_ts_log)
kpss.test(store_12631_ts_log)
pp.test(store_12631_ts_log)
ndiffs(store_12631_ts_log) #  seasonal differencing
nsdiffs(store_12631_ts_log, m = 7, test = "ocsb", max.D = 2) #  Osborn-Chui-Smith-Birchenhall (1988) test is used (with null hypothesis that a seasonal unit root exists)
nsdiffs(store_12631_ts_log_diff, m = 7, test = "ocsb", max.D = 2) #  Osborn-Chui-Smith-Birchenhall (1988) test is used (with null hypothesis that a seasonal unit root exists)

# determine p and q
acf(store_12631_ts_log_diff, lag.max = 20) # ACF vanishes after non-seasonal lag 2-->  so a non-seasonal MA(q) part with q<= 2
pacf(store_12631_ts_log_diff, lag.max = 20) # PACF spike at lag 1  ---> so SEASONAL AR(P) part with P<=1     
# so a good start an ARIMA(1,0,2)(0,0,)

plot(acf(store_12631_ts_log_diff, lag.max=20), main = "Autocorrelation Function of Time Series", frame.plot = FALSE) # ACF vanishes after lag 2 (lag 8 by chance 1 out of 20)-->q<=2, ACF 3 spikes (1,2,8)---> seasonal MA() Q<=3, ACF doesn t decay expoenentially (rather sunisodially)--> P=0,
plot(pacf(store_12631_ts_log_diff, lag.max=20), main = "Partial Autocorrelation Function of Time Series", frame.plot = FALSE) #  PACF 5 spikes at lags--> seasonal AR(P), P<=4 (one out of 20 by chance),  p=0 (PACF doesn t underline any simple form)
# so model (0,1,2) (0,0,3)

auto.arima(store_12631_ts, trace = TRUE, ic = 'bic', lambda = 0, approximation = FALSE) #Best model FROM AUTO-ARIMA(0,1,1) (0,0,2)[7] BIC=-64.85
#auto.arima(store_12631_ts_log, trace = TRUE, ic = 'bic', approximation = FALSE) #Best model FROM AUTO-ARIMA(0,1,1) (0,0,2)[7] BIC=-64.85

# estimation (lambda = 0 to take the natural log form)
store_12631_arima1 <- Arima(store_12631_ts, order = c(0,1,1),seasonal = list(order = c(0, 0, 2), period = 7), include.drift = FALSE, lambda = 0) # BIC = -64.85
store_12631_arima2 <- Arima(store_12631_ts, order = c(0, 1, 1),seasonal = list(order = c(0, 0, 3), period = 7), include.drift = FALSE, lambda = 0) # BIC = -67.61
store_12631_arima3 <- Arima(store_12631_ts, order = c(0, 1, 1),seasonal = list(order = c(1, 0, 2), period = 7), include.drift = FALSE, lambda = 0) # BIC = -74.6

# forecast
store_12631_forecast_arima1 <- forecast.Arima(store_12631_arima1, h = 14)
store_12631_forecast_arima2 <- forecast.Arima(store_12631_arima2, h = 14)
store_12631_forecast_arima3 <- forecast.Arima(store_12631_arima3, h = 14)

# accuracy 
accuracy(store_12631_forecast_arima1)
accuracy(store_12631_forecast_arima2)
accuracy(store_12631_forecast_arima3)

#plot in sample
plot.ts(store_12631_ts, main="ARIMA in-sample performance", xlab="Time Horizon", ylab="Estimated (fitted) values of lettuce demand (ounces)", lty = 1, col = "black", frame.plot = FALSE)            #dark line:original data, red line: our estimation
lines((fitted(store_12631_forecast_arima1)), col = "red", lty = 2)
lines(fitted(store_12631_forecast_arima2), col = "blue", lty = 2)
lines(fitted(store_12631_forecast_arima3), col = "darkgoldenrod", lty = 1)
legend("bottom", title = "Model:", legend=c("Actual", "ARIMA(0,1,1)(0,0,2)","ARIMA(0,1,1)(0,0,3)", "ARIMA(0,1,1)(1,0,2)"), col=c("black", "red", "blue", "darkgoldenrod"), box.lty=0, lty=c(1,2,2,1), cex=0.8)

# train on training set
# forecast based on training set
store_12631_arima1_training <- Arima(training_set_12631_ts, order = c(0,1,1),seasonal = list(order = c(0, 0, 2), period = 7), include.drift = FALSE, lambda = 0)
store_12631_arima2_training <- Arima(training_set_12631_ts, order = c(0,1,1),seasonal = list(order = c(0, 0, 3), period = 7), include.drift = FALSE, lambda = 0)
store_12631_arima3_training <- Arima(training_set_12631_ts, order = c(0, 1, 1),seasonal = list(order = c(1, 0, 2), period = 7), include.drift = FALSE, lambda = 0)

# forecast
store_12631_forecast_arima1 <- forecast.Arima(store_12631_arima1_training, h = 18)
store_12631_forecast_arima2 <- forecast.Arima(store_12631_arima2_training, h = 18)
store_12631_forecast_arima3 <- forecast.Arima(store_12631_arima3_training, h = 18)

# correct log transformation, scale up by variance of error
store_12631_forecast_arima1$mean <-exp(store_12631_forecast_arima1$model$sigma2/2) * store_12631_forecast_arima1$mean
store_12631_forecast_arima1$lower <-exp(store_12631_forecast_arima1$model$sigma2/2) * (store_12631_forecast_arima1$lower)
store_12631_forecast_arima1$x <-exp(store_12631_forecast_arima1$model$sigma2/2) * (store_12631_forecast_arima1$x)
store_12631_forecast_arima1$upper <-exp(store_12631_forecast_arima1$model$sigma2/2) * (store_12631_forecast_arima1$upper)
store_12631_forecast_arima2$mean <-exp(store_12631_forecast_arima2$model$sigma2/2) * store_12631_forecast_arima2$mean
store_12631_forecast_arima2$lower <-exp(store_12631_forecast_arima2$model$sigma2/2) * (store_12631_forecast_arima2$lower)
store_12631_forecast_arima2$x <-exp(store_12631_forecast_arima2$model$sigma2/2) * (store_12631_forecast_arima2$x)
store_12631_forecast_arima2$upper <-exp(store_12631_forecast_arima2$model$sigma2/2) * (store_12631_forecast_arima2$upper)
store_12631_forecast_arima3$mean <-exp(store_12631_forecast_arima3$model$sigma2/2) * store_12631_forecast_arima3$mean
store_12631_forecast_arima3$lower <-exp(store_12631_forecast_arima3$model$sigma2/2) * (store_12631_forecast_arima3$lower)
store_12631_forecast_arima3$x <-exp(store_12631_forecast_arima3$model$sigma2/2) * (store_12631_forecast_arima3$x)
store_12631_forecast_arima3$upper <-exp(store_12631_forecast_arima3$model$sigma2/2) * (store_12631_forecast_arima3$upper)

# out-of-sample performance (forecast power comparison)
accuracy(store_12631_forecast_arima1, validation_set_12631) #  on 
accuracy(store_12631_forecast_arima2, validation_set_12631)
accuracy(store_12631_forecast_arima3, validation_set_12631)   #  so best model (0,0,1),(0,1,2) [7]

# best model forecast (train on thw whole dataset)
store_12631_arima3 <- Arima(store_12631_ts, order = c(0, 1, 1),seasonal = list(order = c(1, 0, 2), period = 7), include.drift = FALSE)
store_12631_forecast_arima_best <- forecast.Arima(store_12631_arima3, h = 14)

# Plot 
plot(store_12631_forecast_arima_best, main = "Final Forecast from ARIMA(0,1,1)(1,0,2)[7] for next 14 days", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  
lines(fitted(store_12631_forecast_arima_best), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# verification --> check whether the model fit the data
plot.ts(store_12631_arima3$residuals)          
acf(store_12631_arima3$residuals, lag.max = 20) 
Box.test(store_12631_arima3$residuals, lag = 20, type = "Ljung-Box") 


# Check for forecasted errors - further imporvement
# plot ACF 
plot(acf(store_12631_forecast_arima_best$residuals, lag.max=20), main = "Autocorrelation Function of ARIMA Residuals", frame.plot = FALSE) #  autocorrelation on residuals 
# Test Ljung–Box (autocorrelation?)
Box.test(store_12631_forecast_arima_best$residuals, lag=20, type="Ljung-Box")
# Plot residuals (constant variance?)
plot.ts(store_12631_forecast_arima_best$residuals, main="ARIMA(0,1,1)(1,0,2)[7] in-sample Forecast Errors", xlab="Time Horizon", ylab="Residuals of ARIMA (0,1,1)(1,0,2)[7]", col = "darkred", frame.plot = FALSE)
# Histogram of forecast errors (normally distributed?)
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a  histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="lightgrey", freq=FALSE, breaks=mybins, main = "Distribution of Forecast Errors on the ARIMA(0,1,1)(1,0,2)[7]", xlab = "Foreacst Errors", "Density")
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="darkred", lwd=2)
}
plotForecastErrors(store_12631_forecast_arima_best$residuals) # make a histogram

# final selection ETS(M,N,M)
store_12631_final <- forecast.ets(store_12631_ets, h = 14)
# Plot 
plot(store_12631_final, main = "Forecasted Lettuce Demand of Restaurant 12631", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_20974_ets_f), col = "red") 
legend("bottom", title = "Model: ETS(M,N,M)", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

write.csv(store_12631_final, "forecast_12631.csv")





#############################################################################
################################# Store 46673 ###############################
#############################################################################


# Holt-Winters --------------------------------------------------------------

# Convert the numerical vector to time series object
store_46673_ts <- ts(store_46673[, 2], frequency = 7, start = c(03,05))
# Plot of time-series
plot.ts(store_46673_ts, main="Daily lettuce demand of store 46673", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", col = "darkred", frame.plot = FALSE)

# Seasonal Decomposition of Time Series by Loess stl()
plot(stl(store_46673_ts, s.window = "period"), main="Seasonal Decomposition of Time Series Lettuce Demand of Store 46673", xaxt = "n")  # trend no seasonality
# HoltWinters function: (find optimal a,b,g, by min sum of square errors)
store_46673_HW <- HoltWinters(store_46673_ts, beta = FALSE) 

#ETS (max likelihood approach and optimises a,b,g and also initial states) Exponential smoothing state space model
store_46673_ets <- ets(store_46673_ts, model = "ZZZ")    

# Plot in-sample performance
plot(store_46673_HW, main="Holt-Winters - ETS (A,N,A) in-sample performance", xlab="Time Horizon", ylab="Estimated (fitted) values of lettuce demand (ounces)", lty = 1, col = "black", frame.plot = FALSE)            #dark line:original data, red line: our estimation
lines(fitted(store_46673_ets), col = "blue", lty = 2)
legend("bottom", legend=c("Actual","HW", "ETS"), col=c("black", "red", "blue"), box.lty=0, lty=c(1,1,2), cex=0.8)

# in -sample performance measures table
sqrt(store_46673_HW$SSE/(length(store_12631_ts)-2)) #sum of square erros / by number of observation-2 (we used first two for initilisation)
accuracy(store_46673_ets)

# Training Set (80%, first 83 days)
training_set_46673_ts   <- ts(store_46673[1:83,2], frequency = 7)
# Validation Set (20%, last 20 days)
validation_set_46673 <- store_46673[84:nrow(store_46673),2] #  as.character(store_12631[nrow(store_12631)*0.8+1,1])

# HW forecast based on training set
store_46673_HW_training <- HoltWinters(training_set_46673_ts, beta = FALSE) 
store_46673_HW_forecast <- forecast.HoltWinters(store_46673_HW_training, h = 20) # forecast for next 20 days

# ETS forecast based on training set
store_46673_ets_training <- ets(training_set_46673_ts, model = "ANA")    
store_46673_ets_forecast <- forecast.ets(store_46673_ets_training, h = 20)

# Plot out-of sample HW
plot(store_46673_HW_forecast, main = "Forecast from HoltWinters on Training Set", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_46673_HW_forecast), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# Plot out-of-sample ETS
plot(store_46673_ets_forecast, main = "Forecast from ETS(A,N,A) on Training Set", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_46673_ets_forecast), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# out-of-sample performance (forecast power comparison)
accuracy(store_46673_HW_forecast, validation_set_46673) #  on 
accuracy(store_46673_ets_forecast, validation_set_46673)


# best model forecast (train on thw whole dataset)
store_46673_ets_f <- forecast.ets(store_46673_ets, h = 14)
# Plot 
plot(store_46673_ets_f, main = "Final Forecast from ETS(A,N,A) for next 14 days", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_46673_ets_f), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# Check for forecasted errors - further imporvement

# plot ACF 
plot(acf(store_46673_ets_f$residuals, lag.max=20), main = "Autocorrelation Function of ETS(A,N,A) Residuals", frame.plot = FALSE) #  autocorrelation on residuals 
# Test Ljung–Box (autocorrelation?)
Box.test(store_46673_ets_f$residuals, lag=20, type="Ljung-Box")
# Plot residuals (constant variance?)
plot.ts(store_46673_ets_f$residuals, main="ETS(A,N,A) in-sample Forecast Errors", xlab="Time Horizon", ylab="Residuals of ETS (A,N,A)", col = "darkred", frame.plot = FALSE)
# Histogram of forecast errors (normally distributed?)
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a  histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="lightgrey", freq=FALSE, breaks=mybins, main = "Distribution of Forecast Errors on the ETS (A,N,A)", xlab = "Foreacst Errors", "Density")
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="darkred", lwd=2)
}
plotForecastErrors(store_46673_ets_f$residuals) # make a histogram




# ARIMA ---------------------------------------------------------------------

# identification
plot.ts(store_46673_ts, main="Daily lettuce demand of store 46673", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", col = "darkred", frame.plot = FALSE)

# test if time series is stationary
adf.test(store_46673_ts)
kpss.test(store_46673_ts)
pp.test(store_46673_ts)
ndiffs(store_46673_ts) #  seasonal differencing

nsdiffs(store_46673_ts, m = 7, test = "ocsb", max.D = 2) #  Osborn-Chui-Smith-Birchenhall (1988) test is used (with null hypothesis that a seasonal unit root exists)

# determine p and q
acf(store_46673_ts, lag.max = 200)  # acf no exponential decrase and doesn t vanish after specific lag so no non-seas MA q = 0 , NO SPECIFIC spike in ACF so no seasonal MA Q=0 
pacf(store_46673_ts, lag.max = 200) # ACF decays sinusoidal --> so non-seas AR part with p<=1    , PACF vanishes after lag 6 so non-seas AR Q<=5 
plot(acf(store_46673_ts, lag.max=20), main = "Autocorrelation Function of Time Series", frame.plot = FALSE) #  autocorrelation on residuals 
plot(pacf(store_46673_ts, lag.max=20), main = "Partial Autocorrelation Function of Time Series", frame.plot = FALSE) #  

auto.arima(store_46673_ts, trace = TRUE, ic = 'bic') #Best model FROM AUTO-ARIMA(1,0,0,) (2,0,0)[7]

# estimation
store_46673_arima1 <- Arima(store_46673_ts, order = c(1,0,0),seasonal = list(order = c(2, 0, 0), period = 7), include.drift = FALSE)
store_46673_arima2 <- Arima(store_46673_ts, order = c(1, 0, 0),seasonal = list(order = c(2, 0, 1), period = 7), include.drift = FALSE)
store_46673_arima3 <- Arima(store_46673_ts, order = c(1, 0, 0),seasonal = list(order = c(3, 0, 0), period = 7), include.drift = FALSE)

# forecast
store_46673_forecast_arima1 <- forecast.Arima(store_46673_arima1, h = 14)
store_46673_forecast_arima2 <- forecast.Arima(store_46673_arima2, h = 14)
store_46673_forecast_arima3 <- forecast.Arima(store_46673_arima3, h = 14)

# accuracy 
accuracy(store_46673_forecast_arima1)
accuracy(store_46673_forecast_arima2)
accuracy(store_46673_forecast_arima3)

#plot in sample
plot.ts(store_46673_ts, main="ARIMA in-sample performance", xlab="Time Horizon", ylab="Estimated (fitted) values of lettuce demand (ounces)", lty = 1, col = "black", frame.plot = FALSE)            #dark line:original data, red line: our estimation
lines((fitted(store_46673_forecast_arima1)), col = "red", lty = 2)
lines(fitted(store_46673_forecast_arima2), col = "blue", lty = 2)
lines(fitted(store_46673_forecast_arima3), col = "darkgoldenrod", lty = 1)
legend("bottom", title = "Model:", legend=c("Actual", "ARIMA(1,0,0)(2,0,0)","ARIMA(1,0,0)(2,0,1)", "ARIMA(1,0,0)(3,0,0)"), col=c("black", "red", "blue", "darkgoldenrod"), box.lty=0, lty=c(1,2,2,1), cex=0.8)


# train on training set
# forecast based on training set
store_46673_arima1_training <- Arima(training_set_46673_ts, order = c(1,0,0),seasonal = list(order = c(2, 0, 0), period = 7), include.drift = FALSE)
store_46673_arima2_training <- Arima(training_set_46673_ts, order = c(1, 0, 0),seasonal = list(order = c(2, 0, 1), period = 7), include.drift = FALSE)
store_46673_arima3_training <- Arima(training_set_46673_ts, order = c(1, 0, 0),seasonal = list(order = c(3, 0, 0), period = 7), include.drift = FALSE)

# forecast
store_46673_forecast_arima1 <- forecast.Arima(store_46673_arima1_training, h = 18)
store_46673_forecast_arima2 <- forecast.Arima(store_46673_arima2_training, h = 18)
store_46673_forecast_arima3 <- forecast.Arima(store_46673_arima3_training, h = 18)

# out-of-sample performance (forecast power comparison)
accuracy(store_46673_forecast_arima1, validation_set_46673) 
accuracy(store_46673_forecast_arima2, validation_set_46673)   # so best model (1,0,0),(2,0,1) [7]
accuracy(store_46673_forecast_arima3, validation_set_46673)   


# best model forecast (train on thw whole dataset)
store_46673_arima3 <- Arima(store_46673_ts, order = c(1, 0, 0),seasonal = list(order = c(2, 0, 1), period = 7), include.drift = FALSE)
store_46673_forecast_arima_best <- forecast.Arima(store_46673_arima3, h = 14)

# Plot 
plot(store_46673_forecast_arima_best, main = "Final Forecast from ARIMA(1,0,0)(2,0,1)[7] for next 14 days", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_46673_forecast_arima_best), col = "red") 
legend("bottom", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

# verification --> check whether the model fit the data
plot.ts(store_46673_arima3$residuals)          
acf(store_46673_arima3$residuals, lag.max = 20)     # we want that to be 0, we want no autocorrelation independencies in the residuals (if tehre are we can improve the model more)
Box.test(store_46673_arima3$residuals, lag = 20, type = "Ljung-Box") 


# Check for forecasted errors - further imporvement

# plot ACF 
plot(acf(store_46673_forecast_arima_best$residuals, lag.max=200), main = "Autocorrelation Function of ARIMA(1,0,0)(2,0,1)[7] Residuals", frame.plot = FALSE) #  autocorrelation on residuals 
# Test Ljung–Box (autocorrelation?)
Box.test(store_46673_forecast_arima_best$residuals, lag=20, type="Ljung-Box")
# Plot residuals (constant variance?)
plot.ts(store_46673_forecast_arima_best$residuals, main="ARIMA(1,0,0)(2,0,1)[7] in-sample Forecast Errors", xlab="Time Horizon", ylab="Residuals of ARIMA(1,0,0)(2,0,1)[7]", col = "darkred", frame.plot = FALSE)
# Histogram of forecast errors (normally distributed?)
plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a  histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="lightgrey", freq=FALSE, breaks=mybins, main = "Distribution of Forecast Errors on the ARIMA(1,0,0)(2,0,1)[7]", xlab = "Foreacst Errors", "Density")
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="darkred", lwd=2)
}
plotForecastErrors(store_46673_forecast_arima_best$residuals) # make a histogram


# final selection ETS(A,N,A)
store_46673_final <- forecast.ets(store_46673_ets, h = 14)
# Plot 
plot(store_46673_final, main = "Forecasted Lettuce Demand of Restaurant 46673", xlab="Time Horizon", ylab="Lettuce quantity (ounces)", lty = 1, col = "black", frame.plot = FALSE)  #  HW
lines(fitted(store_46673_final), col = "red") 
legend("bottom", title = "Model: ETS(A,N,A)", legend=c("Actual values","Forecasted values", "Fitted values"), col=c("black", "blue", "red"), box.lty=0, lty=1, cex=0.8)

write.csv(store_46673_final, "forecast_46673.csv")



