year1 <- 87:91
# 社會服務業自民國87至民國91年的年度用電量（度）
power1 <- c(6097059332, 6425887925, 6982579022, 7323992602.53436, 7954239517) 
# 製造業自民國87至民國91年的年度用電量（度）
power2 <- c(59090445718, 61981666330, 67378329131, 66127460204.6482, 69696372914.6949) 

# 用於指定填答用變數，勿改動
for(i in 1:6) assign(paste0("answer", i), NA)
# 結束

# 請選出各年度 (`year1`)中，社會服務業的的用電量超過 `7e9` 的年份。
# (`7e9`是R 的科學符號，代表`7 * 10^9`)
# 並將結果存於 answer1 中
answer1 = year1[power1 > 7e9]

# 請計算「社會服務業從民國87年到91年的平均用電量」
# 並將結果存於 answer2 中
answer2 = mean(power1)

# 請計算「社會服務業從民國87年到91年用電量的標準差」
# 並將結果存於 answer3 中
answer3 = sd(power1)

# 在統計中，我們常常會計算一筆數據的「標準分數」，也就是將數據減去平均數後除以標準差。
# 請同學計算「社會服務業從民國87年到91年用電量的標準分數」
# 並將結果存於 answer4 中
answer4 = (power1 - mean(power1)) / sd(power1)
# answer4 = scale(power1)

# 同樣的道理，請同學算出「製造業自民國87年至民國91年用電量的平均數、標準差和標準分數」
# 並將結果存於 answer5 中
answer5 = c(
    mean(power2),
    sd(power2),
    scale(power2)
)

# 最後，請同學找出年度中，「社會服務業的用電量」，超過「製造業的10%用電量」的年份
# 並將結果存於 answer6 中
answer6 = year1[power1 > (power2 * 0.1)]

####### 列印答案
cat("The answer is:")
print(c(answer1, answer2, answer3, answer4, answer5, answer6))