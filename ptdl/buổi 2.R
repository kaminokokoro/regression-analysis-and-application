# TSS so sánh sự sai khác tính trên gt của y(mức độ chênh lệch của y vs gt 
# trung bình của y)
# RSS sum(y - a0)
# 
# 1. Chia bộ dữ liệu làm 2 phần training data và test data
# Chọn ngẫu nhiên một phần của bộ dữ liệu làm training data (thường theo 1 
# tỉ lệ cho trước)
# Phần còn lại dùng làm test data

# 2. Trên training data
# Phân tích hồi quy tuyến tính Y = a_0 + a_1 * X + e
# Đưa ra ktc beta cho a_0 và a_1
# Đánh giá hệ số của mô hình

# 3. Trên test data
#  Tính TSS = sum((y - mean(y))^2)  xét trên y
#  Tính RSS = sum(e^2) - sum((y - a_0_h - a_1_h * x)^2)
# Xét trên phần dư
# Tính R2 = 1 - RSS/TSS
#   R là hệ số tương quan mẫu đo mức độ phụ thuộc tuyến tính của Y và X

# RSE độ thiếu chính xác của mô hình

data(iris)
# make this example reproducible
set.seed(1)
# use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE, prob=c(0.7, 0.3))
sample
table(sample)        
View(iris)
train <- iris[sample, ]
test <- iris[!sample, ]
dim(test)                                                                    

# Bộ dữ liệu iris gồm 150 quan sát đã được chia thành 2 bộ dữ liệu con train gồm 106 quan sát và 44 quan sát cho test

names(iris)
# Viết mô hình hồi quy tuyến tính của Sepal.Length theo Petal.Length
attach(iris)
lm.fit <- lm(Sepal.Length ~ Petal.Length)
lm.fit
# Intercept : 4.3066
# Petal.Length : 0.4089

summary(lm.fit)

lm.fit <- lm(Sepal.Length ~ Petal.Length,data = train)
lm.fit
confint(lm.fit)
