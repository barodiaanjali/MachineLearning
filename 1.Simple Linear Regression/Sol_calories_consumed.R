library(readr)
calories_consumed <- read_csv("D:/ALL Assignments/3.Simple Linear Regression/calories_consumed.csv")
View(calories_consumed)
attach(calories_consumed)

head(calories_consumed)
# EDA
# 1. Scatter diagram: scatterplot
scatter.smooth(x=Cal,
               y=Wgt,
               main="Wgt ~ Cal")


#-----------------------------------------------------------

plot(Cal,Wgt)
cor(Cal,Wgt)
lm(Wgt~Cal)
calories_model <-lm(Wgt~Cal)
summary(calories_model)
confint(calories_model,level = 0.95)
predict(calories_model,data.frame(Cal=1800),interval="confidence")
predict(calories_model,data.frame(Cal=1800),interval = "prediction")
#predict(calories_model, data.frame(1800))
View(calories_model$residuals)
sqrt(sum(calories_model$residuals^2)/(nrow(calories_consumed)-1)) #RMSE

# --------------------------------------
predicted_val <- predict(calories_model)
residual = predicted_val - Wgt
sqrt(sum(residual^2)/(nrow(calories_consumed) -1 ))


