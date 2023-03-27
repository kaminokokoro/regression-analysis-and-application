rm(list=ls())
# mô hình hội quy tuyến tính bội
install.packages(datasets) # cài đặt gói lệnh
library(datasets)
attach(mtcars)
only = lm(mpg ~ cyl, data = mtcars) # nếu cyl + 0 thì chỉ có cyl thui, bỏ hệ số tự do
only
# nếu chỉ có hệ số tự do: only = lm(mpg~1, data=mtcars)
only = lm(mpg ~ cyl + 0, data = mtcars)
only

two = lm(mpg ~ cyl + disp, data = mtcars) # theo 2 bien
two$coefficients
# pt hqtt: mpg = 34.66 - 1.588*cyl - 0.02*disp

# mpg theo tat ca cac bien
all = lm(mpg~., data = mtcars) # mpg theo 10 bien con lai
all

summary(all)

# mpg = a0 + a1 * cyl + a2 * disp + ...
# h0: a1 = 0 vs H1: a1 != 0
# do p-value = 0.9161 > 0.05 nen chap nhan H0

summary(two) # theo 2 bien
# dua vao p-value xac dinh bac ho hay chap nhan H0

two = lm(mpg ~ cyl + wt, data = mtcars)
summary(two)
# mpg = a0 + a1 * cyl + a2 * wt
# phan du e ~ N(0, sigma^2(e))

shapiro.test(two$residuals) # kiem tra pp chuan cua phan du
# H0: phan du tuan theo phan phoi chuan, H1: phan du ko tuan theo pp chuan
# do p-value = 0.0631 > 0.05 -> 
# co co so de noi rang phan du tuan theo pp chuan
# Neu phan du k tuan theo pp chuan, de kiem dinh gia tri trung binh cua phan du co bang 0
# hay ko thi dung wilcox.test()
# Neu phan du tuan theo pp chuan thi dung t.test()
t.test(two$residuals) # kiem dinh xem gia tri trung binh cua phan du co bang 0 
# khong khi biet phan du tuan theo phan phoi chuan


# Chỉ số AIC dùng hàm step để tìm mô hình tốt nhất, tương ứng với AIC tốt nhất
library(stats)

# forward: mô hình đi từ y=a0 + e đến y = a0 + x1.a1 +.... + e
# backward ngươc lại

# object: hàm đơn giản nhất/ phức tạp nhất
# scope: hàm lm phức tạp nhất/ đơn giản nhất
# direction: forward/ backward
# trace: đưa vào giá trị càng lớn thì kết quả càng chi tiết

only = lm(mpg ~ 1, data=mtcars)
all = lm(mpg ~ ., data=mtcars) # mpg theo 10 biến còn lại

forward = step(only, scope = formula(all), direction = 'forward', trace=0)
forward$anova

# Mô hình thu được sau bước này mà:
# Sau 4 bước, do tính toán AIC không giảm nên dừng ở 4 biến
# Tham số cụ thể:
forward$coefficients

summary(forward)

backward = step(all, scope = formula(only), direction = 'backward', trace = 0)
backward$anova
summary(backward)
