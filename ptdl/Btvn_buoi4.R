#btvn_buoi4
rm(list=ls())

library(MASS)
library(dataset)
attach(Boston)
View(Boston)

#câu a
only=lm(medv ~ 1)
only
all=lm(medv ~ ., data=Boston )
forward=step(only,scope = formula(all),direction = "forward",trace = 0)
forward$anova
forward$coefficients

backward=step(all,scope = formula(only),direction = "backward", trace = 0)
backward$anova
backward$coefficients
#câu b
# Khi phân tích “forward”,biến medv đc biểu diễn theo hai biến lstat và rm
b=lm(medv ~ lstat +rm)
summary(b)
# Kiểm định phần dư
# H0: phần dư = 0; H1: phần dư != 0
# Vì các giá trị Pr(>|T|) > 0.05, chấp nhận H0
# có cơ sở để nói phần dư = 0 với myn 5%

# Viết lại mô hình với phần dư = 0
b = lm(medv ~ lstat + rm + 0, data =Boston)
summary(b)

# Kiểm định hệ số hồi quy
# H0: tồn tại hệ số hồi quy = 0; H1: tồn tại hệ số hồi quy != 0
# Vì tất cả p value < 0.05, bác bỏ H0
# có thể nói hệ số hồi quy khác 0 với myn 5%

#câu c
summary(backward)
# vì các Pr(>|t|) <0.05 nên các hệ số hồi quy khác 0 với mức ý nghĩa 5%
shapiro.test(backward$residuals)
# đặt giả thiết H0: phần dư của backward tuân theo phân phối chuẩn và H1: ko tuân theo phân phối chuản
# p-value <0.05 nên bác bỏ H0 và ta kết luận phần dư ko tuân theo quy luật phân phối chuẩn với myn 5%
t.test(backward$residuals,mu=0)
# đặt giả thiết H0: e=0 và H1: e!=0
# vì p-value >0.05 nên chấp nhận H0
# giá trị trung bình của phần dư backward bằng 0 với mức ý nghĩa 5%

