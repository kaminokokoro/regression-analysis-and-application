rm(list=ls())
library(MASS)
library(ISLR)

data(cars)
View(cars)

names(cars)
dim(cars)
summary(cars)

set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(cars), replace = TRUE, prob=c(0.65, 0.35))

train <- cars[sample, ]
test <- cars[!sample, ]

dim(train)
dim(test)

attach(train)

lm <- lm(dist ~ speed)
lm
# intercept    speed
# -16.609      3.843

# Y = -16.609 + 3.843 * X

# Khoảng tin cậy cho các hệ số
confint(lm, level = 0.95)

# Y = a_0 + a_1 * X
# H0: a_0 = 0, H1: a_0 != 0
summary(lm)
# Vì p_value < 0.05, bác bỏ H0
# Với myn 5%, có cơ sở để nói a_0 khác 0

# H0: a_1 = 0; a_1 != 0
# Vì p_value < 0.05, bác bỏ H0
# Với myn 5%, có cơ sở để nói a_1 khác 0
detach(train)

# Sử dụng test data
attach(test)
Y_hat = -18.924 + 4.049 * speed

#  Tính TSS = sum((y - mean(y))^2)  xét trên y
#  Tính RSS = sum((y - Y_hat)^2)
TSS = sum((dist - mean(dist))^2)
TSS
RSS = sum((dist - Y_hat)^2)
RSS

# TSS = RSS + 

R2 = 1 - RSS/TSS
R = sqrt(R2)
R2
R

# Với R = 0.7479092, dist và speed có sự tương quan mạnh
lm.fit <- lm(dist ~ speed,data=test)
summary(lm.fit)

