# Linear 
rm(list = ls(all=T))
library(readr)
delivery_time <- read_csv("D:/ALL Assignments/3.Simple Linear Regression/delivery_time.csv")
View(delivery_time)
attach(delivery_time)
plot(SortingTime,DeliveryTime)
cor(SortingTime,DeliveryTime)
delivery_time_model <- lm(DeliveryTime~SortingTime)
summary(delivery_time_model)
sqrt(sum(delivery_time_model$residuals^2)/(nrow(delivery_time)))

# Logarithm regression
plot(log(SortingTime),DeliveryTime)
cor(log(SortingTime),DeliveryTime)
delivery_time_model_2 <- lm(DeliveryTime~log(SortingTime))
summary(delivery_time_model_2)
sqrt(sum(delivery_time_model_2$residuals^2)/(nrow(delivery_time)))

#predicted_val <- predict(delivery_time_model_2)
#res = predicted_val - DeliveryTime
#sqrt(sum(res^2)/(nrow(delivery_time)-1))

# Exponential regression
plot(SortingTime,log(DeliveryTime))
cor(SortingTime,log(DeliveryTime))
delivery_time_model_3 <- lm(log(DeliveryTime)~SortingTime)
summary(delivery_time_model_3)

# calculate RMSE 
predict(delivery_time_model_3) # log of deliveryTime
predicted_val  <- exp(predict(delivery_time_model_3)) # Delivery time
res2 <- predicted_val - DeliveryTime 
sqrt(sum(res2^2)/(nrow(delivery_time)))
plot(delivery_time_model_3)


confint(delivery_time_model_3,level = 0.95)

help(nrow)
# Polynomial (Quadratic) Refression
plot(SortingTime + I(SortingTime^2),DeliveryTime)
cor(SortingTime + I(SortingTime^2),DeliveryTime)
delivery_time_model_4 <- lm(log(DeliveryTime)~SortingTime + I(SortingTime^2))
summary(delivery_time_model_4)

# Polynomial (3rd Degree) Refression
plot(SortingTime + I(SortingTime^2) + I(SortingTime^3),DeliveryTime)
cor(SortingTime + I(SortingTime^2) +I(SortingTime^3),DeliveryTime)
delivery_time_model_5 <- lm(log(DeliveryTime)~SortingTime + I(SortingTime^2) + I(SortingTime^3))
summary(delivery_time_model_5)
 