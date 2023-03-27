# buoi5
rm(list = ls())
library(ISLR)
set.seed(1)

?sample

train=sample(392,196)
#sinh ra 196 giá trị từ 1 đến 392

dim(Auto)
attach(Auto)
train 
lm.fit = lm(mpg ~ horsepower ,data=Auto ,subset=train)
MSE=mean((mpg -predict (lm.fit ,Auto))[-train ]^2)

lm.fit2=lm(mpg~poly(horsepower ,2),data=Auto , subset=train)
mean((mpg -predict (lm.fit2 ,Auto ))[- train]^2)


#LOOCV
glm.fit= glm(mpg~ horsepower)
coef(glm.fit)

glfit = lm(mpg~horsepower, data = Auto)
coef(glfit)
# E((y - y_h)^2) tính trên tập dữ liệu test = Auto\Auto[train]
# xét mô hình hồi quy tuyến tính Y = a + bx (trang 192 - 204)


library(boot)
glm.fit = glm(mpg~horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta #MSE trước và sau khi hiệu chỉnh

cv.error=rep(0,5)
for (i in 1:5) {
  glm.fit=glm(mpg~poly(horsepower,i))
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

