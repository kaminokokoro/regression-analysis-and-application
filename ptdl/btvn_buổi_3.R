rm(list=ls())
data("cars")
set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(cars), replace = TRUE, prob=c(0.65, 0.35))
sample

training <- cars[sample, ]
testing <- cars[!sample, ]

attach(training)
lm = lm(dist ~ speed, data = training)
lm
#(Intercept)        speed  
#-16.609        3.843  
# dist= -16.609 + 3.843*speed

# khoảng tin cậy cho các hệ số
confint(lm.fit, level = 0.95)
#                2.5 %    97.5 %
# (Intercept) -31.191826 -2.026813
# speed         2.928665  4.757896

# Y = -16.609 + 3.843*X
# H0: a_o=0 ; H1: a_0!=0
summary(lm)
# vì p_value <0.05 nên bác bỏ H0
# với myn 5% , có cơ sở nói a_0 khác 0

detach(training)
# sử dụng test data
attach(testing)
lm=lm(dist ~ speed)
summary(lm)
# dist= -18.924 + 4.049*speed

#  Tính TSS = sum((y - mean(y))^2)  xét trên y
#  Tính RSS = sum(e^2) - sum((y - a_0_h - a_1_h * x)^2)

TSS=sum((dist-mean(dist))**2)
TSS
# TSS=15863.24
RSS=sum((dist-(-18.924 + 4.049*speed))**2)
RSS
#RSS=6947.259

R2=1-(RSS/TSS)
R2
#R2=0.5620529
#Multiple R-squared:  0.5621
R=sqrt(R2)
R
#R=0.7497019
# với R=0.7497019, ta kết luận giữa dist và speed có tương quan mạnh