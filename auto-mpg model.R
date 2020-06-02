#contributors: Joel Curtis, Ben Curtis

#load tidyverse
library(tidyverse)

#set the path, and load in the data, downloaded from Kaggle
auto_mpg_path <- "auto-mpg.csv"
auto_mpg <- read.csv(auto_mpg_path)

#filter out missing data in horsepower field
auto_mpg <- auto_mpg %>%
  filter(horsepower != "?")%>%
  mutate(horsepower = as.character(horsepower),#convert horsepower to character and then into numeric
         cylinders = as.factor(cylinders),#convert cylinders to a factor
         origin = as.factor(origin), #convert origin to a factor
         weight = as.double(weight), #convert weight to numeric
         model.year = as.factor(model.year))%>%  #convert model.year to factor
  mutate(horsepower = as.numeric(horsepower)) #this is a work around fixing a data distortion

#set the seed to a random number to create reproducibility
set.seed(100)

#randomly split the data into 70/30 train and test set
auto_mpg$id <- 1:nrow(auto_mpg)
train <- auto_mpg %>%
  sample_frac(0.7)
test <- anti_join(auto_mpg, train, by = 'id')

#model to predict mpg based on vehicle qualities
#create model's all models use multiple linear regression algorithm to train
#model 1
#cylinders + displacement + horsepower + weight + acceleration + model.year
m1 <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration +
           model.year, data = train)#train

p1 <- predict(m1, test) #predict on test set

p1_result <- cbind(test, p1) #bind columns of test and predictions
p1_result$se <- (p1_result$mpg - p1_result$p1)^2 #compute square error

rmse_1 <- sqrt(mean(p1_result$se)) #compute root mean square error

#model 2
#removed acceleration from m1
m2 <- lm(mpg ~ cylinders + displacement + horsepower + weight +
           model.year, data = train)#train

p2 <- predict(m2, test)#predict on test set

p2_result <- cbind(test, p2) #bind columns of test and predictions
p2_result$se <- (p2_result$mpg - p2_result$p2)^2 #compute square error

rmse_2 <- sqrt(mean(p2_result$se)) #compute root mean square error


#model 3
#removed displacement from m2
m3 <- lm(mpg ~ cylinders + horsepower + weight +
           model.year, data = train)#train

p3 <- predict(m3, test) #predict on test set

p3_result <- cbind(test, p3) #bind columns of test and predictions
p3_result$se <- (p3_result$mpg - p3_result$p3)^2 #compute square error

rmse_3 <- sqrt(mean(p3_result$se)) #compute root mean square error

#model 4
m4 <- lm(mpg ~ cylinders + horsepower + weight,
         data = train) #train

p4 <- predict(m4, test) #predict on test set

p4_result <- cbind(test, p4) #bind columns of test and predictions
p4_result$se <- (p4_result$mpg - p4_result$p4)^2 #compute square error

rmse_4 <- sqrt(mean(p4_result$se)) #compute root mean square error

#model 5
m5 <- lm(mpg ~ cylinders + horsepower + weight + acceleration +
           displacement,
         data = train) #train

p5 <- predict(m5, test) #predict on test set

p5_result <- cbind(test, p5) #bind columns of test and predictions
p5_result$se <- (p5_result$mpg - p5_result$p5)^2 #compute square error

rmse_5 <- sqrt(mean(p5_result$se)) #compute root mean square error

#model 6
m6 <- lm(mpg ~ cylinders + horsepower + weight +
           displacement,
         data = train) #train

p6 <- predict(m6, test) #predict on test set

p6_result <- cbind(test, p6) #bind columns of test and predictions
p6_result$se <- (p6_result$mpg - p6_result$p6)^2 #compute square error

rmse_6 <- sqrt(mean(p6_result$se)) #compute root mean square error

#compare models
rmse_list <- c("rmse_1" = rmse_1, "rmse_2" = rmse_2, 
               "rmse_3" = rmse_3, "rmse_4" = rmse_4, 
               "rmse_5" = rmse_5, "rmse_6" = rmse_6)#create vector with all rmse's
rmse_list #view the rmse_list

min_rmse <- names(which.min(rmse_list))# find the name of the minimum rmse
min_rmse_value <- min(rmse_list) #find the value of the minimum rmse

min_rmse #view the min rmse name
min_rmse_value #view the min rmse value

#model 3 has minimum rmse at 3.126
#due to min rmse on test data model 3 should be used

