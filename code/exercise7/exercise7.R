# 1101 資三合 資料科學實務 期中上機測驗
# 滿分 150p 最高採計至 99p

# 準備作答所需資料，勿改動，請直接執行
answers <- list()
data(CO2)
set.seed(1102)
index <- sample(1:nrow(CO2), floor(0.8*nrow(CO2)))
CO2.sample1 = CO2[index,]
CO2.sample2 = CO2[-index,]

set.seed(2021)
models <- lapply(1:20, function(i) {
	ii <- sample(1:nrow(CO2), floor(0.8*nrow(CO2)))
	dd = CO2[ii,]
	lm(uptake~Type+Treatment+conc, dd)
})
# 資料準備結束

# 1. (10p) 試計算 CO2 資料表中 Type 為 Quebec 其平均的 uptake 值，並存入 answers[[1]]
answers[[1]] <- mean(CO2$uptake[CO2$Type=='Quebec'])

# 2. (10p) 試計算 CO2 資料表中 Type 為 Mississippi 其平均的 uptake 值，並存入 answers[[2]]
answers[[2]] <- mean(CO2$uptake[CO2$Type=='Mississippi'])

# 3. (10p) 試將平均較高的 Type 填入 answer[[3]]，請以程式碼作答，不可直接填寫答案
a <- mean(CO2$uptake[CO2$Type=='Quebec'])
b <- mean(CO2$uptake[CO2$Type=='Mississippi'])

Q = 'Quebec'
M = 'Mississippi'

l <- list()
l[[a]] <- Q
l[[b]] <- M

h = max(a, b)
answers[[3]] <- l[h]

# 4. (15p) 試計算 CO2 資料表中 在不同的 Type 與 Treatment 的組合下，uptake 的平均值
# 並將其組織為 data.frame，存入 answers[[4]]
ans4_1 <- data.frame(
	Type = 'Quebec',
	Treatment = 'nonchilled',
	uptake.avg = mean(CO2$uptake[CO2$Type=='Quebec' & CO2$Treatment=='nonchilled'])
)
ans4_2 <- data.frame(
	Type = 'Quebec',
	Treatment = 'chilled',
	uptake.avg = mean(CO2$uptake[CO2$Type=='Quebec' & CO2$Treatment=='chilled'])
)
ans4_3 <- data.frame(
	Type = 'Mississippi',
	Treatment = 'nonchilled',
	uptake.avg = mean(CO2$uptake[CO2$Type=='Mississippi' & CO2$Treatment=='nonchilled'])
)
ans4_4 <- data.frame(
	Type = 'Mississippi',
	Treatment = 'chilled',
	uptake.avg = mean(CO2$uptake[CO2$Type=='Mississippi' & CO2$Treatment=='chilled'])
)
ans4_t1 = rbind(ans4_1, ans4_2)
ans4_t2 = rbind(ans4_3, ans4_4)
ans4 = rbind(ans4_t1, ans4_t2)
answers[[4]] <- ans4

# 5. (10p) 試計算 CO2 資料表中，conc 與 uptake 的關聯性
answers[[5]] <- cor(CO2$conc, CO2$uptake)

# 6. (10p) 單純以 CO2 資料表中的 conc 建立線性模型預測 uptake，將此模型的 R2 存入 answer[[6]]
# 7. (15p) RMSE 存入 answers[[7]]
# 提示：可以從模型中的 fitted.values 屬性取得模型對於 uptake 的估計值，再著手計算 R2，或者將模型輸入 summary 函式取得其 R2 值，獲取方式請見 summary 函式的 help
m = lm(uptake ~ conc, CO2)
y = CO2$uptake
answers[[6]] <- cor(m$fitted.values, y) ^ 2
answers[[7]] <- sqrt(mean((y - m$fitted.values) ^ 2))

# 8. (10p) 以 CO2 資料表中的 uptake 與 plant 以外的屬性建立線性模型預測 uptake，計算此模型的 R2 相較於單純只用 conc 建立的模型進步多少，將 R2 的差存入 answers[[8]]
m2 <- lm(uptake ~ Type + Treatment + conc, CO2)
y = CO2$uptake
deltaM = cor(m2$fitted.values, y) ^ 2 - cor(m$fitted.values, y) ^ 2
answers[[8]] <- deltaM

# 9. (10p) CO2.sample1 為 CO2 資料表的抽樣，
# 試以 CO2_sample1 資料表中的 uptake 與 plant  以外的屬性建立線性模型預測 uptake，
# 將建立的模型存入 model1，並將此模型的 R2 存入 answers[[9]]
model1 <- lm(uptake ~ Type + Treatment + conc, CO2.sample1)
y = CO2.sample1$uptake
answers[[9]] <- cor(model1$fitted.values, y) ^ 2 

# 10. (10p) CO2.sample2 為 CO2 資料表中除去 CO2.sample1 的資料，
# 以 predict 函式在 CO2.sample2 上預測 uptake 的估計值 uptake_hat，
# 計算 uptake_hat 與 CO2.sample2 中的 uptake 之 R2 值存入 answers[[10]]
uptake_hat = predict(model1, CO2.sample2)
answers[[10]] <- cor(CO2.sample2$uptake, uptake_hat) ^ 2

# 11. (20p) models 是一個 list，
# 裡面放了多個以 CO2 的抽樣資料表訓練的模型，
# 請計算 (或取得) 這些模型平均的 R2 並存入 answers[[11]]
l2 = models[[1]]$fitted.values
names(l2) <- NULL
answers[[11]] <- cor(l2, models[[1]]$model$uptake) ^ 2

# 12. (20p) testdata.csv 內登載了五位同學某課程小考與作業成績的檔案，
# 請以正確的編碼將其載入 R 環境，並且將成績資料檔以 data.frame 型式存入 answers[[12]]
csv <- read.csv('testdata.csv', fileEncoding='UTF-16LE', skip=2)
csv <- head(csv, -1)
answers[[12]] <- csv


# 以下為答案之基本檢查，可以視為是提示
stopifnot(answers[[3]] %in% c("Quebec","Mississippi"))
stopifnot(nrow(answers[[4]]) == 4)
stopifnot(class(answers[[12]])=="data.frame")
stopifnot(nrow(answers[[12]]) == 4 & ncol(answers[[12]]) == 4)

# print(answers[[3]] %in% c("Quebec","Mississippi"))
# print(nrow(answers[[4]]) == 4)
# print(class(answers[[12]])=="data.frame")
# print(nrow(answers[[12]]) == 4 & ncol(answers[[12]]) == 4)

# 印出答案
print(answers)

