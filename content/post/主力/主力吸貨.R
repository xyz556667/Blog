rm(list=ls());gc()
# 使用函數包
library(dplyr)
library(magrittr)
library(quantmod)

load("/主力/adjStockPriceData.RData")
colnames(adjStockPriceData)<-c("code","name","date","open","high","low","close","volume","mktvalue")
code_list<-unique(adjStockPriceData$code)


#------------5日均量要9成要大於500張------------------
vol_require<-500
require_ratio<-0.9

code_Big_Vol<-adjStockPriceData %>% #filter(code=="1101   ")
  group_by(code) %>%
  dplyr::filter(n()>=252) %>%
  mutate(
    volMA5=SMA(volume,5),
    is_volbig500=ifelse(volMA5>vol_require,1,0)
  ) %>% 
  group_by(code) %>%
  summarise(volbig500_Ratio=mean(is_volbig500,na.rm=T)) %>%
  dplyr::filter(volbig500_Ratio>=require_ratio) %>% pull(code)

adjStockPriceData<-adjStockPriceData %>% 
  dplyr::filter(code %in% code_Big_Vol )
#------------5日均量要9成要大於500張------------------

# 找出主力吸貨股票進場位置
standardKdays <- 90   # 標準K棒平均日數
adjStockPriceData <- adjStockPriceData %>%  
  group_by(code) %>%
  dplyr::filter(n()>=standardKdays) %>%        # 股票交易日數需大於標準K棒平均日數                                      
  # 整理判斷條件所需資料
  mutate(
    open=as.numeric(open),
    MA5=SMA(close,5),              # 5日移動平均線
    MA20=SMA(close,20),            # 20日移動平均線
    MA60=SMA(close,60),            # 60日移動平均線
    kbar=abs(open/close-1),        # K棒絕對值大小 
    standardKbar=rollmean(x=kbar, k=standardKdays,fill=NA, align = "right"), # 計算標準K棒值長度
    lagkbar1=dplyr::lag(kbar,1),          # 前一天K棒大小
    lagkbar2=dplyr::lag(kbar,2),          # 前二天K棒大小
    lagOpen1=dplyr::lag(open,1),          # 前一天開盤價
    lagOpen2=dplyr::lag(open,2),          # 前二天開盤價
    lagClose1=dplyr::lag(close,1),        # 前一天收盤價
    lagClose2=dplyr::lag(close,2),        # 前二天收盤價
    leadOpen1=lead(open,1),
    Buy_date=lead(date,1)
  )

# 整理進場位置
tradeTargetTable <- adjStockPriceData %>%
  dplyr::filter(lagClose1<lagOpen1,              # 第t-1天的收盤價<第t-1天的開盤價(黑K)
         close>open,                      # 第t天的收盤價>第t天的開盤價(紅K)
         close>MA5,
         close>lagOpen1,
         lagClose1>open,
         kbar>(standardKbar*1.5),          # 第t天的K棒為標準K棒的1.5倍以上
         lagkbar1<(standardKbar*0.4)) %>%  # 第t-1天的K棒為標準K棒的0.4倍以下
  select(code, signalDate=date,inDate=Buy_date, inPrice=leadOpen1) 

# 整理出場位置
outSiteTable <- adjStockPriceData %>%
  mutate(outSite=ifelse((close<MA20)&(dplyr::lag(close,1)>dplyr::lag(MA20,1)),1,0)) %>%  # 出場位置為收盤價跌破MA20
  dplyr::filter(outSite==1,
                lagClose1>lagOpen1,              # 第t-1天的收盤價<第t-1天的開盤價(黑K)
                close<open,                      # 第t天的收盤價>第t天的開盤價(紅K)
                close<lagOpen1,
                lagClose1<open,
                kbar>(standardKbar*1.5),          # 第t天的K棒為標準K棒的1.5倍以上
                lagkbar1<(standardKbar*0.4)) %>%
  select(code, outDate=date, outPrice=close)%>%
  group_by()



# 整理交易明細表
tradeDetailTable <- NULL
for(ix in 1:nrow(tradeTargetTable)){
  
  inDate <- tradeTargetTable$inDate[ix]   # 進場日期
  stockCode <- tradeTargetTable$code[ix]  # 股票代碼
  outData <- outSiteTable %>%             # 該支股票代碼資料進場日之後的最近出場日
    dplyr::filter(code==stockCode, outDate>inDate) %>%
    dplyr::filter(row_number()==1) %>%
    select(outDate, outPrice)
  if(nrow(outData)>0){
    tradeDetailTable <- bind_rows(tradeDetailTable, bind_cols(tradeTargetTable[ix,], outData))
  }
}
# 計算考慮交易成本後之報酬率
buyCostR <- 0.001425
sellCostR <- 0.004425
tradeDetailTable$ret <- (tradeDetailTable$outPrice*(1-sellCostR))/(tradeDetailTable$inPrice*(1+buyCostR))-1
# 計算持有期間日數
# 日期轉換函數
DateConvert <- function(x){
  return(as.Date(paste(substring(x,1,4),substring(x,5,6),substring(x,7,8),sep="-",format="%Y%m%d")))
}
tradeDetailTable$holdDays <- as.numeric(DateConvert(tradeDetailTable$outDate)-DateConvert(tradeDetailTable$inDate))

# 計算相關績效指標
# 平均報酬率
meanRet <- mean(tradeDetailTable$ret)
# 報酬率標準差
sdRet <- sd(tradeDetailTable$ret)
# 交易次數
tradeNums <- nrow(tradeDetailTable)
# 勝率
winRatio <- sum(as.numeric(tradeDetailTable$ret>0))/tradeNums
# 最大報酬率
maxRet <- max(tradeDetailTable$ret)
# 最小報酬率
minRet <- min(tradeDetailTable$ret)
# 平均持有期間日數
avgHoldDays <- mean(tradeDetailTable$holdDays)

Annual_Ret=meanRet*250/avgHoldDays
# 繪製績效分配圖
hist(tradeDetailTable$ret, xlab="報酬率", ylab="次數", main="策略報酬率分配圖", col="deepskyblue1")

# 繪圖函數
PlotGraph <- function(plotSample){
  
  # 繪製交易的股票代碼
  plotCode <- tradeDetailTable$code[plotSample]
  inDate <- tradeDetailTable$inDate[plotSample]
  outDate <- tradeDetailTable$outDate[plotSample]
  
  # 整理該股票的股價資料
  stockData <- adjStockPriceData[which(adjStockPriceData$code==plotCode),] %>%
    rename(tradeVolume=volume)
  stockData <- stockData[,c("date","open","high","low","close","tradeVolume","MA5","MA20","MA60")] # 取出繪圖所需資料(開高收低成交量)
  
  # 繪圖起始日
  matchSite <- which(stockData$date==inDate)-10
  plotStartDate <- stockData$date[ifelse(matchSite<1, 1, matchSite)]  # 此處用ifelse避免資料超出邊界
  
  # 繪圖結束日
  matchSite <- which(stockData$date==outDate)+10
  plotEndDate <- stockData$date[ifelse(matchSite>nrow(stockData), nrow(stockData), matchSite)] # 此處用ifelse避免資料超出邊界
  
  # 要繪製的股價資料範圍
  plotData <- stockData[which((stockData$date>=plotStartDate)&(stockData$date<=plotEndDate)),]
  
  # 加入進場位置資訊
  plotData$inSite <- rep(NA, nrow(plotData))
  plotData$inSite[which(plotData$date==inDate)] <- plotData$close[which(plotData$date==inDate)]
  
  # 加入出場位置資訊
  plotData$outSite <- rep(NA, nrow(plotData))
  plotData$outSite[which(plotData$date==outDate)] <- plotData$close[which(plotData$date==outDate)]
  
  # 將plotData資料由data.frame格式轉為xts，符合繪圖資料格式要求
  plotData <- xts(plotData[,-1], order.by= as.Date(ISOdate(year=substring(plotData$date,1,4),
                                                           month=substring(plotData$date,5,6),
                                                           day=substring(plotData$date,7,8)),format="%Y%m%d")) 
  # 繪製技術分析圖形
  # 設定K棒顏色
  myTheme <- chart_theme()
  myTheme$col$dn.col <- c("chartreuse3")  # 跌K棒顏色
  myTheme$col$up.col <- c("firebrick3")   # 漲K棒顏色
  
  # 繪製主圖形
  chart_Series(plotData[,1:5], name=paste0(plotCode," 技術分析圖"), theme=myTheme)
  # 加入成交量
  add_Vo()
  # 加入5日移動平均線
  add_TA(plotData$MA5, on=1, type="l", col="blue", lwd=1.5)
  # 加入20日移動平均線
  add_TA(plotData$MA20, on=1, type="l", col="orange", lwd=1.5)
  # 加入60日移動平均線
  add_TA(plotData$MA60, on=1, type="l", col="green", lwd=1.5)
  # 標註進場位置
  add_TA(plotData$inSite, on=1, type="p", col="red", pch=2, cex=5, lwd=3)
  # 標註出場位置
  add_TA(plotData$outSite, on=1, type="p", col="green", pch=6, cex=5, lwd=3)  
}

tradeDetailTable <- tradeDetailTable %>% arrange(ret)

# 繪製交易技術分析圖形 plotSample為交易明細表內的列數
for ( ix in c(1:3,(nrow(tradeDetailTable)-2):nrow(tradeDetailTable))) {
  PlotGraph(plotSample=ix) %>% print()
}
which(tradeDetailTable$ret>0.3)

# 輸出結果
cat(paste0("*********策略回測績效*********\n",
           #"回測期間: ",backtestStartDate, " 至 ",backtestEndDate,"\n",
           "平均報酬率: ",round(meanRet*100,2)," %\n",
           "交易次數: ",tradeNums," 次\n",
           "勝率: ",round(winRatio*100,2)," %\n",
           "報酬率標準差: ",round(sdRet*100,2)," %\n",
           "最大報酬率: ",round(maxRet*100,2)," %\n",
           "最小報酬率: ",round(minRet*100,2)," %\n",
           "平均持有日數: ",round(avgHoldDays,2),"天"))



