rm(list=ls())
library(MASS)

library(dataset)
View(Boston)

only = lm(medv ~ 1, data = Boston)
all = lm(medv ~ ., data = Boston)

forward = step(only, scope = formula(all), direction = 'forward', trace = 0)
forward$anova
forward$coefficients
summary(forward)

# nếu biểu diễn medv theo 2 biến thì biểu diễn theo lstat và rm
re = lm(medv ~ lstat + rm, data = Boston)
summary(re)

# Kiểm định phần dư
# H0: phần dư = 0; H1: phần dư != 0
# Vì các giá trị Pr(>|T|) > 0.05, chấp nhận H0
# có cơ sở để nói phần dư = 0 với myn 5%

# Viết lại mô hình với phần dư = 0
re = lm(medv ~ lstat + rm + 0, data =Boston)
summary(re)

# Kiểm định hệ số hồi quy
# H0: tồn tại hệ số hồi quy = 0; H1: tồn tại hệ số hồi quy != 0
# Vì tất cả p value < 0.05, bác bỏ H0
# có thể nói hệ số hồi quy khác 0 với myn 5%


backward = step(all, scope = formula(only), direction = 'backward', trace = 0)
backward$anova
summary(backward)
backward$coefficients

# Kiểm tra xem phần dư có phân theo phân phối chuẩn hay không
# H0: Residual tuân theo pp chuẩn, H1: Residual không tuân theo pp chuẩn
shapiro.test(backward$residuals)
# Vì p value < 0.05, bác bỏ H0
# có cơ sở để nói Residual không tuân theo phân phối chuẩn với myn 5%

# Kiểm định xem giá trung bình có bằng 0 ?
# H0: mean trung bình phần dư = 0, H1: mean phần dư != 0
wilcox.test(backward$residuals, mu = 0)
# Vì p value > 0.05, chấp nhận H0
# Có cơ sở để nói mean phần dư = 0

# Kiểm định hệ số hồi quy
# H0: tồn tại hệ số hồi quy = 0; H1 : tồn tại hệ só hồi quy != 0
summary(backward)
# Vì các Pr(>|t|) < 0.05, có cơ sở để nói các hệ số hồi quy khác 0 với myn 5%

