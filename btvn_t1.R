#pthq_btvn
#câu1
rm(list=ls())
library(readr)
data=read.csv("C:/Users/ADMIN/Desktop/anh tam thoi/Australian Institute of Sport.csv")

names(data)
summary(data)

# Đường thẳng hồi quy BMI theo Wt
attach(data)
lm.fit= lm(BMI ~ Wt)
lm.fit

summary(lm.fit)

# Phần dư có phân bố chuẩn với giá trị trung bình bằng 0 không?
e=lm.fit$residuals
e
shapiro.test(e)

# Đặt giả thuyết H0: e tuân theo phân phối chuẩn, H1: e k tuân theo pp chuẩn
# p-value < 0.05 nên bác bỏ H0
# Với myn 5%, có cơ sở để nói e không tuân theo pp chuẩn

# tìm khoảng tin cậy 90% cho các hệ số hồi quy
confint(lm.fit, level = 0.9)


# Các hệ số hồi quy có thực sự khác 0 không?
summary(lm.fit)
# Y = alpha + beta*X
# H0: hệ số hồi quy = 0; H1: Hệ số hồi quy != 0
# Vì p-value < 0.05 nên bác bỏ H0
# KL với myn 5%, có cơ sở để nó alpha khác 5%

# Giả sử tồn tại hệ số hồi quy bằng 0; ước lượng hệ số mới
lm(BMI~Wt + 0)
# khoảng tin cậy cho các hệ số
confint(lm(BMI~Wt + 0))


# Câu 2
# Sinh ngẫu nhiên 1000 giá trị của biến X từ phân phối chuẩn N(158; 25) và 1000 giá trị
# của biến Y từ phân phối chuẩn N(59; 9).
set.seed(1)
X=rnorm(1000,158,25)
Y=rnorm(1000,59,9)

#Viết phương trình đường thẳng hồi quy tuyến tính của X theo Y.
lm.fit= lm(X~Y)
summary(lm.fit)

#Phần dư có phân bố chuẩn với giá trị trung bình bằng 0 không?
e=lm.fit$residuals
shapiro.test(e)
#đặt giả thuyết H0: e tuân theo phân phối chuẩn ; H1: e không tuân theo phân phối chuẩn 
#p-value>0.05 nên chấp nhận H0 
# Với myn 5%, có cơ sở để nói e  tuân theo pp chuẩn
# H0: e = 0; H1: e != 0
t.test(e, mu=0)
# Vì p-value > 0.05 nên k đủ cơ sở để chấp nhận H0

#Với Y = 57.5, đưa ra dự đoán về giá trị của X và khoảng tin cậy 95% cho giá trị trung bình của X.
predict(lm.fit,data.frame(Y=c(57.5)),interval = "confidence",level = 0.95)

