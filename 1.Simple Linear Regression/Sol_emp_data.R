library(readr)
emp_data <- read_csv("D:/ALL Assignments/3.Simple Linear Regression/emp_data.csv")
View(emp_data)
attach(emp_data)
plot(Salary_hike,Churn_out_rate)
cor(Salary_hike,Churn_out_rate)
emp_data_model <- lm(Churn_out_rate~Salary_hike)
summary(emp_data_model)

#log
plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike),Churn_out_rate)
emp_data_model_2 <- lm(Churn_out_rate~log(Salary_hike))
summary(emp_data_model_2)

#exp
plot(Salary_hike,log(Churn_out_rate))
cor(Salary_hike,log(Churn_out_rate))
emp_data_model_3 <- lm(log(Churn_out_rate)~Salary_hike)
summary(emp_data_model_3)
confint(emp_data_model_3,level = 0.95)
