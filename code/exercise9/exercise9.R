#' 請用學到的方法讀取 GDP.txt 的資料、
#' 整理資料，並把最後的結果存到變數`gdp`。
#' 提示： GDP.txt 中的第一欄數據是年/季、第二欄數據是該季的GDP(百萬)。
#' 整理結果應該要有兩欄的數據，第一欄是年份，第二欄是我國每年的GDP，
#' 具體細節請參考最後的`stopifnot`的檢查事項。
#' 提示：拿掉數據中間的逗號，請用：`gsub(pattern = ",", replacement = "", x = <你的字串向量>)`

gdp <- read.table('GDP.txt', sep=",", skip=4)
gdp <- head(gdp, 132)

backupGDP <- gdp

years <- sapply(strsplit(gdp$V1, "Q"), `[`, 1)
values <- as.numeric(gsub(pattern=",", replacement="", x=gdp$V2))

formatGDP <- function(years, gdpValues) {
	
	if (length(years) != length(gdpValues)) {
		return(NA)
	}
	
	l <- list()
	len <- length(years)
	
	for (i in 1:len) {
		year = years[i]
		value = gdpValues[i] * 1000000
		
		if (!(year %in% names(l))) {
			l[[year]] <- 0
		}
		
		l[[year]] <- l[[year]] + value
	}
	
	K = names(l)
	V = c()
	
	for (i in 1:length(l)) {
		k = names(l)[i]
		v = l[[k]]
		V <- c(V, v)
	}
	
	return(
		data.frame(
			"year"=K,
			"gdp"=V
		)
	)
}

gdp <- formatGDP(years, values)

stopifnot(is.data.frame(gdp))
stopifnot(colnames(gdp) == c("year", "gdp"))
stopifnot(class(gdp$year) == "character")
stopifnot(class(gdp$gdp) == "numeric")
stopifnot(nrow(gdp) == 33)
stopifnot(range(gdp$year) == c("1981", "2013"))
stopifnot(range(gdp$gdp) == c(1810829,14564242) * 1000000)

# --------------------------------------------------------------------------------

#' 請同學利用之前所學，從cl_info_other.csv的欄位 
#' data_dt的資料中萃取出資料的「年和月」，並存到欄位 year_month 中。
#' 並且最後只留下 year_month 與 mortgage_bal兩個欄位。
#' 這裡的data_dt 是收集資料的時間點，
#' mortgage_bal 則是房貸餘額。

cl_info <- read.csv('cl_info_other.csv', fileEncoding="utf-8")
date <- sapply(strsplit(csv$data_dt, " "), `[`, 1)

year_month <- sapply(substr(date, 1, 7), `[`, 1)
cl_info$year_month <- year_month

keep = c("year_month", "mortgage_bal")
cl_info = subset(cl_info, select=keep)

stopifnot(class(cl_info$year_month)[1] == "character")
stopifnot(ncol(cl_info) == 2)
stopifnot(!is.null(cl_info$mortgage_bal))

#' 請算出各年度每個月份全國的 mortgage_bal 總和，
#' 並且把結果放在 mortgage_total_bal 欄位中。
#' 結果依照月份由小到大做排序。
#' 最後將結果整理至 `cl_info_year` 變數中
#' 結果應該要有兩欄的數據，第一欄是年份，第二欄是每年房貸餘額的值(請以每年的一月份資料為準)。
#' 具體細節請參考最後的`stopifnot`檢查事項。
#' 
#' ??
#' "結果依照月份由小到大做排序。" ?
#' "第一欄是年份"                 ?
#' 
#' ??
#' "請算出各年度每個月份全國的 mortgage_bal 總和，並且把結果放在 mortgage_total_bal 欄位中。" ?
#' "第二欄是每年房貸餘額的值(請以每年的一月份資料為準)。" ?
#' 
#' What....

calc <- function(df) {
	
	# years = sapply(substr(df$year_month, 1, 4), `[`, 1)
	years = df$year_month
	
	values = df$mortgage_bal
	l = list()
	
	for (i in 1:length(years)) {
		year = years[i]
		month = substr(year, 6, 7)
		if (all(month == "01")) {
			value = values[i]
			year = substr(year, 1, 4)
			
			if (!(year %in% names(l))) {
				l[[year]] <- 0
			}
			
			l[[year]] <- l[[year]] + value
		}
	}
	
	K = names(l)
	V = c()
	
	for (i in 1:length(l)) {
		k = names(l)[i]
		v = l[[k]]
		V <- c(V, v)
	}
	
	return(
		data.frame(
			"year"=K,
			"mortgage_total_bal"=V
		)
	)
}

cl_info_year <- calc(cl_info)
# sorted = temp[order(cl_info_year$mortgage_total_bal, decreasing = T),]

stopifnot(is.data.frame(cl_info_year))
stopifnot(colnames(cl_info_year) == c("year", "mortgage_total_bal"))
stopifnot(class(cl_info_year$year) == "character")
stopifnot(class(cl_info_year$mortgage_total_bal) == "numeric")
stopifnot(nrow(cl_info_year) == 9)
stopifnot(range(cl_info_year$year) == c("2006", "2014"))
stopifnot(range(cl_info_year$mortgage_total_bal) == c(3.79632e+12, 5.726784e+12))


# --------------------------------------------------------------------------------

#' 最後，請同學用這門課程所學的技術整合`gdp`與`cl_info`的資料，
#' 並計算出房貸餘額與gdp的比率（mortgage_total_bal / gdp）。
#' 請將結果輸出到一個data.frame，第一欄是年份，第二欄則是房貸餘額的GDP佔有比率。
#' 細節請參考`stopifnot`的檢查。
#' 

calcRatio <- function(gdp, cl_info_year) {
	offset = 25
	len = length(cl_info_year$year)
	
	l = list()
	
	for (i in 1:len) {
		yr = cl_info_year$year[i]
		
		gdpValue = gdp[[2]][i + offset]
		mtbValue = cl_info_year$mortgage_total_bal[i]
		
		ratio = mtbValue / gdpValue
		
		if (!is.na(ratio)) {
			if (!(yr %in% names(l))) {
				l[[yr]] <- 0
			}
			
			l[[yr]] <- l[[yr]] + ratio
		}
	}
	
	K = names(l)
	V = c()
	
	for (i in 1:length(l)) {
		k = names(l)[i]
		v = l[[k]]
		V <- c(V, v)
	}
	
	return(
		data.frame(
			"year"=K,
			"index"=V
		)
	)
}

mortgage2gdp <- calcRatio(gdp, cl_info_year)

stopifnot(is.data.frame(mortgage2gdp))
stopifnot(nrow(mortgage2gdp) == 8)
stopifnot(colnames(mortgage2gdp) == c("year", "index"))
stopifnot(class(mortgage2gdp$year) == "character")
stopifnot(class(mortgage2gdp$index) == "numeric")
stopifnot(min(mortgage2gdp$index) > 0.3)
stopifnot(max(mortgage2gdp$index) < 0.4)

# --------------------------------------------------------------------------------

#' 請繪製各年度房貸餘額佔比 GDP 的長條圖
#' 將其輸出為 PNG 格式，與程式碼一同上傳

mergeDF = merge(x=gdp, y=cl_info_year, by="year")

# Not sure what to plot
# But I found something like this:
# https://stackoverflow.com/questions/44724580/add-percentage-labels-to-a-stacked-barplot

ratioMTB = mergeDF$mortgage_total_bal / mergeDF$gdp
ratioDF = data.frame(
	year = mergeDF$year,
	gdp = 1 - ratioMTB,
	mtb = ratioMTB
)

library(ggplot2)
library(reshape2)

meltDF = melt(ratioDF)
ggplot(meltDF, aes(year, value, fill=variable)) + 
	geom_bar(stat="identity", position="fill", color='black', width=0.9) +
	scale_y_continuous(labels = scales::percent) +
	geom_text(
		aes(label = paste0(round(value * 100, 2), "%")),
		position = position_stack(vjust = 0.5), size = 3
	)




#' 請思考使用當年度一月的房貸餘額與當年全部的 GDP 比較計算是否合理
#' 若是，請在下方以註解提出你的理由
#' 若否，請提出你的計算方法，並且進行將結果算出來

#'是
#'因為以我所知，房貸餘額每月只會上升一點點