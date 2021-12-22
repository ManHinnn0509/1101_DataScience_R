# 請依照指示完成資料處理
# 這是從 <http://data.gov.tw/node/7769> 下載的海盜通報資料，
# 請使用課堂所學的技巧，將此文件中的資訊整理成為表格。
# 首先，先將該檔案載入到R 之中。
pirate_info <- readLines(file("./pirate-info-2015-09_2.txt", encoding = "utf-8"))

# 這份資料的格式，可以用`：`分割出資料的欄位與內容，
# 請同學利用`strsplit`將`pirate_info`進行切割，
# 並將結果儲存到`pirate_info_key_value`之中。
pirate_info_key_value <- strsplit(pirate_info, '：')

# 請利用 lapply 或 sapply 將欄位存入 pirate_info_key 中
pirate_info_key <- sapply(pirate_info_key_value, '[[', 1)

# 請利用 lapply 或 sapply 將內容存入 pirate_info_value 中
pirate_info_value <- sapply(pirate_info_key_value, '[', 2)

# 請辨認欄位名稱，並且正確分割 pirate_info_value 的內容取出下列內容
# 取出事件發生的日期，以字串形式儲存
temp = (grepl('日期', pirate_info_key, fixed=T))
event.date = pirate_info_value[temp == T]

# 取出事件發生的地點，以字串形式儲存
temp2 = pirate_info_key == '地點'
event.location <- pirate_info_value[temp2 == T]

# 取出事件發生之緯度座標屬於北緯或南緯，北緯請存 "N", 南緯請存 "S"
temp3 = pirate_info_key == '經緯度'
coords = pirate_info_value[temp3 == T]
temp4 = grepl('北緯', coords, fixed=TRUE)

temp4[temp4 == T] <- 'N'
temp4[temp4 == F] <- 'S'

lat.type <- temp4

# 取出事件發生緯度座標的度數，以數字型式儲存
lat.degree <- as.numeric(substring(coords, 3, 4))

# 取出事件發生緯度座標的分數，以數字型式儲存
lat.minute <- as.numeric(substring(coords, 6, 7))

# 取出事件發生之經度座標屬於東經或西經，東經請存 "E", 西經請存 "W"
temp5 = substring(coords, 10, 11) == '東經'
temp5[temp5 == T] <- 'E'
temp5[temp5 == F] <- 'W'
lng.type <- temp5

# 取出事件發生經度座標的度數，以數字型式儲存
lng.degree <- as.numeric(substring(coords, 12, 14))

# 取出事件發生經度座標的分數，以數字型式儲存
lng.minute <- as.numeric(substring(coords, 16, 17))

# 請以下列程式碼將上述資訊存入 data.frame 中
pirate.df <- data.frame(
  date = event.date,
  location = event.location,
  lat.type = lat.type,
  lat.degree = lat.degree,
  lat.minute = lat.minute,
  lng.type = lng.type,
  lng.degree = lng.degree,
  lng.minute = lng.minute
)

# 以下為格式檢查，上傳前請確認通過所有下列檢查條件
# 即程式能夠正確執行結束，不出現錯誤
# stopifnot(nrow(pirate.df) == 11)
# stopifnot(sum(pirate.df$lat.degree) == 43)
# stopifnot(sum(pirate.df$lat.minute) == 251)
# stopifnot(sum(pirate.df$lng.degree) == 1151)
# stopifnot(sum(pirate.df$lng.minute) == 339)

print(nrow(pirate.df) == 11)
print(sum(pirate.df$lat.degree) == 43)
print(sum(pirate.df$lat.minute) == 251)
print(sum(pirate.df$lng.degree) == 1151)
print(sum(pirate.df$lng.minute) == 339)

