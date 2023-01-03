## Correlation
cor(mtcars$mpg, mtcars$hp)
cor(mtcars$mpg, mtcars$wt)

plot(mtcars$mpg, mtcars$hp, pch = 16) #pch คือเปลี่ยนจุดเป็นสีดำ
plot(mtcars$wt, mtcars$mpg, pch = 16)
plot(mtcars$wt, mtcars$hp, pch = 16)

cor(mtcars[ , c("mpg", "wt", "hp")])  #ตัวแรกว่าง คือ เลือกทุก row

## dplyr (tidyverse) 
#เหมือน code บรรทัดบน แต่ลองใช้วิธีแบบ dplyr

library(dplyr)
corMat <- mtcars %>%
  select(mpg, wt, hp) %>% 
  cor()
# ผลออกมาเหมือนกัน

## compute correlation (r) and significant test
cor(mtcars$mpg, mtcars$hp)
cor.test(mtcars$hp, mtcars$mpg)

## Linear Regression

## mpg = f(hp)

lmfit <- lm(mpg ~ hp, data = mtcars)

summary(lmfit)

## prediction

lmfit$coefficients
lmfit$coefficients[[1]] + lmfit$coefficients[[2]]*200

#ใน [] เลข1คือตำแหน่งแรก = intercept , เลข2คือตำแหน่งที่สอง = slope (hp)

new_cars <- data.frame(
  hp = c(250, 320, 400, 410, 450)
)

## predict()
new_cars$mpg_pred <- predict(lmfit, newdata = new_cars)
new_cars$hp_pred <- NULL   #ใส่ NULL คือการลบทิ้ง

#argument ที่1 คือ model ที่เราสร้งขึ้นมา ชื่อว่า lmfit เอาไปทำนาย argument ที่2 ที่ชื่อ new_data

summary(mtcars$hp)

#การที่ค่า mpg ออกมาติดล มันไม่เคยเห้นคือ hp = 450 มาก่อน ค่าสูงเกินไป เลยทำนายออกมาไม่ค่อยดีเท่าไหร่

## RMSE Root Mean Square Error
## Multiple Linear Regression
## mpg = f(hp, wt, am)
## mpg = intercept + b0*hp + b1*wt + b2*am

lmfit_v2 <- lm(mpg ~ hp + wt + am, data = mtcars)

coefs <- coef(lmfit_v2)

coefs[[1]] + coefs[[2]]*200 + coefs[[3]]*3.5 + coefs[[4]]*1

## Build Full Model

lmfit_Full <- lm(mpg ~ ., data = mtcars)
lmfit_Full <- lm(mpg ~. - gear, data = mtcars)

# ใส่ . คือ การเอาทุก column
# ถ้าไม่เอา column ไหน ให้ใส่ - ลบชื่อ column นั้น ออกไป

mtcars$predicted <- predict(lmfit_Full)

head(mtcars)

## Train RMSE
squared_error <- (mtcars$mpg - mtcars$predicted) ** 2
(rmse <- sqrt(mean(squared_error))) 

## split data
set.seed(99)    ## ล็อคผลเอาไว้
sample(1:10, 3)
n <- nrow(mtcars)
id <- sample(1:n, size = n*0.8)  # n ในที่นี้ = 32 คือข้อมูลทั้งหมดมี 32ตัว
train_data <- mtcars[id, ]
test_data <- mtcars [-id, ]

## Train Test Model

model1 <- lm(mpg ~ hp + wt, data = train_data)
p_train <- predict(model1)
error_train <- train_data$mpg - p_train
(rmse_train <- sqrt(mean( error_train**2 )))

## Test Model
p_test <- predict(model1, newdata = test_data)
error_test <- test_data$mpg - p_test
(rmse_test <- sqrt(mean(error_test ** 2 )))

## Print Result
cat("RMSE Train:", rmse_train,
    "\nRMSE Test:", rmse_test)

# \n คือ ขึ้นบรรทัดใหม่
# function cat คือ การปริ้นผลลัพธ์ไปใน console ซึ่ง = function paste, print

# ทำงานกับ Train ได้ดี แต่พอทำงานกับ Test แล้วค่า error มีค่าสูงขึ้น เรียกว่า Overfitting

# วิธีแก้ overfitting คือ ใส่ตัวแปรเข้าไปเพิ่ม เพื่อช่วยทำนายให้มันแม่นยำขึ้น





