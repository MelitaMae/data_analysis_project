## Logistic Regression

library(dplyr)
mtcars %>% head()

str(mtcars)

## converse type of am to factor
mtcars$am <- factor(mtcars$am,
                    levels = c(0,1),
                    labels = c("Auto", "Manual"))

class(mtcars$am)
table(mtcars$am)

## split data
set.seed(99)
sample(1:10, 3)
n <- nrow(mtcars)
id <- sample(1:n, size = n*0.8)
train_data <- mtcars[id, ]
test_data <- mtcars [-id, ]

## train model
logic_model <- glm(am ~ mpg, data = train_data, family = "binomial")
p_train <- predict(logic_model, type = "response")
train_data$pred <- if_else(p_train >= 0.5, "Manual", "Auto")
train_data$am == train_data$pred
mean(train_data$am == train_data$pred)  ## คือค่า accuracy

## ตัวแปรมตาม ~ ตัวแปรต้น
## ใส่ type = "response" คือ การทำให้ค่าออกมาเป็นค่า prob มีค่าอยู่ระหว่าง 0-1


## test model
p_test <- predict(logic_model, newdata = test_data, type = "response")
test_data$pred <- if_else(p_test >= 0.5, "Manual", "Auto")
test_data$am == test_data$pred
mean(train_data$am == test_data$pred)





