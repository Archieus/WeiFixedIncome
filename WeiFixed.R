library(quantmod)
library(PerformanceAnalytics)
library(Quandl)

#alphavantage API = Y474
Sys.setenv(TZ = "EST5EDT")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")

#### Read CSV for Symbols List ###
symblist <- read.csv("wei.csv", header = FALSE)
symbol <- t(symblist)

####Download ADJUSTED PRICE DATA from AlphaVantage
###outputsize=c(full,compact) full= 20 years of data, compact = 100 datapoints

#### CREATE COMPONENTS FOR API CALL ####
apikey <- "&outputsize=full&apikey=Y474&datatype=csv"
URLbase <- "http://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol="

cu <-NULL
ru <-NULL

### Loop to download dats for all symbols in list as it's own object ###

for(i in 1:length(symbol)){
  cu[i] <- paste0(URLbase, symbol[i])
  ru[i] <- paste0(cu[i],apikey)
  assign(paste0(symbol[i], ".c"), read.csv(ru[i]))
}

getSymbols("DGS30", src = "FRED")

USTBond <- Quandl("CHRIS/CME_US1", type = "xts", start_date = "2002-07-26")
UST10Yr <- Quandl("CHRIS/CME_TY1", type = "xts", start_date = "2002-07-26")

USTreas <- na.omit(cbind(USTBond[,6], UST10Yr[,6]))
names(USTreas) <- c("USTBonds", "US10Yr")
USTReturn <- na.omit(Return.calculate(USTreas, "log"))

###Convert CSV to XTS ####
MUB <- xts(MUB.c[,-1], order.by = as.POSIXct(MUB.c$timestamp, format = "%Y-%m-%d"))
JNK <- xts(JNK.c[,-1], order.by = as.POSIXct(JNK.c$timestamp, format = "%Y-%m-%d"))
LQD <- xts(LQD.c[,-1], order.by = as.POSIXct(LQD.c$timestamp, format = "%Y-%m-%d"))
PFF <- xts(PFF.c[,-1], order.by = as.POSIXct(PFF.c$timestamp, format = "%Y-%m-%d"))

AdjClose <- na.omit(cbind(MUB$adjusted_close, JNK$adjusted_close, LQD$adjusted_close, PFF$adjusted_close))
names(AdjClose) <- symbol

MUBRet <- na.omit(Return.calculate(MUB$adjusted_close, "log"))
JNKRet <- na.omit(Return.calculate(JNK$adjusted_close, "log"))
LQDRet <- na.omit(Return.calculate(LQD$adjusted_close, "log"))
PFFRet <- na.omit(Return.calculate(PFF$adjusted_close, "log"))

MUB.dd <- table.Drawdowns(MUBRet, 10, 4)
JNK.dd <- table.Drawdowns(JNKRet, 10, 4)
LQD.dd <- table.Drawdowns(LQDRet, 10, 4)
PFF.dd <- table.Drawdowns(PFFRet, 10, 4)

USTBD.dd <- table.Drawdowns(USTReturn$USTBonds, 15, 4)
UST10.dd <- table.Drawdowns(USTReturn$US10Yr, 15, 4)

# write.csv(MUB.dd, file = "MUBdd.csv")
# write.csv(JNK.dd, file = "JNKdd.csv")
# write.csv(LQD.dd, file = "LQDdd.csv")
# write.csv(PFF.dd, file = "PFFdd.csv")
# write.csv(USTBD.dd, file = "USTBDdd.csv")
# write.csv(UST10.dd, file = "UST10dd.csv")

DGS30Max <- na.omit(runMax(DGS30[!is.na(DGS30)]))
DGS30Min <- na.omit(runMin(DGS30[!is.na(DGS30)]))

chart_Series(MUB$adjusted_close, subset = '2017:/', line.type = "s", pch = 16,
             TA = c("add_TA(DGS30Max, on = NA)", "add_TA(DGS30[!is.na(DGS30)], on = 2)", "add_TA(DGS30Max, on = 2)"))
layout(1:2)
chart_Series(MUB$adjusted_close, subset = '2017:/', name = "MUB")
chart_Series(DGS30, subset = '2017:/', TA = c("add_TA(DGS30Max, on = 1)", "add_TA(DGS30Min, on = 1)"))

####Create an Object List of all Data####
#objNames <- ls(pattern="*c$")    # object names
#objList <- lapply(objNames, get)  # list of objects
#names(objList) <- objNames        # assign names to list
