# 請將 fish.csv 載入環境，存入 fishdata 變數中
fishdata <- read.csv('./Exercise 4/fish.csv', fileEncoding='UTF-8-BOM')

# 接下來我們要使用手刻的最小方差法，來回歸 fishdata 中的 weights 屬性
# 我們要以 Weight 以外的屬性來回歸 Weight
X <- model.matrix(~ Length1 + Length2 + Length3 + Height + Width, data=fishdata)

# 我們以 Weight 的值放入 y 之中，作為迴歸的目標
y <- fishdata$Weight

beta.hat <- solve(t(X) %*% X) %*% t(X)
y.hat <- X %*% beta.hat

#' 這邊我們可以計算 y.hat 和 y 的 correlation, R^2 與 Root Mean Squared Error
#' cor(y.hat, y)
#' sum((y - y.hat) ^ 2)
#' sum((y - mean(y)) ^ 2)

my.corr <- cor(y.hat, y)
my.r2 <- my.corr ^ 2
my.rmse <- sqrt(mean((y - y.hat) ^ 2))

#' 在R 裡面跑迴歸分析，可以簡單用`lm`這個函數來進行線性迴歸：
g <- lm(Weight ~ Length1 + Length2 + Length3 + Height + Width, fishdata)

#' g 這個物件就會包含我們剛剛算過得答案
#' g$coef就會是beta.hat
#' g$fitted.value就會是X %*% beta.hat 也就是 lm 函式建模後所計算的 y.hat
#' summary(g)則會顯示各個參數的t 檢定，以及整個模型的R-squared
#' 這邊我們可以計算由 lm 函式所計算的y.hat 和 y 的 correlation, 
#' R^2 與 Root Mean Squared Error
my.corr_2 <- cor(g$fitted.values, y)
my.r2_2 <- my.corr_2 ^ 2
my.rmse_2 <- sqrt(mean((y - g$fitted.values) ^ 2))

# answer = c(my_corr, my.r2, my.rmse, my.corr_2, my.r2_2, my.rmse_2)
answer = c(my.corr, my.r2, my.rmse, my.corr_2, my.r2_2, my.rmse_2)
print(answer)
print(beta.hat)
print(summary(g))

