
install.packages('quantmod')
install.packages('dtw')


library('dtw')
library('quantmod')

dist.MOdist <- function(x) { MOdist(t(x)) }
dist.DTW <- function(x) { dtw(x[1,], x[2,])$distance }





# EXAMPLE

## A noisy sine wave as query
idx<-seq(0,6.28,len=100);
query<-sin(idx)+runif(100)/10;

## A cosine is for template; sin and cos are offset by 25 samples
template<-cos(idx)

alignment<-dtw(query,template,keep=TRUE);

## Display the warping curve, i.e. the alignment curve
plot(alignment,type="threeway")

tickers <- c('SPY','BAC', 'JPM')
data <- getSymbols(tickers, src = 'yahoo', from = '2010-01-01', auto.assign = T)  


plot(dtw(query,template,keep=TRUE,step=rabinerJuangStepPattern(6,"c")), type="twoway",offset=-2)
dtw(query,template, keep=TRUE, step=rabinerJuangStepPattern(6,"c"))$distance
dtw(query,template, keep=TRUE)$distance

dtw(BAC$BAC.Adjusted,SPY$SPY.Adjusted, keep=TRUE)$distance
dtw(BAC,SPY, keep=TRUE)$distance

plot(dtw(BAC,SPY,keep=TRUE,step=rabinerJuangStepPattern(6,"c")), type="twoway",offset=-2)

plot(rabinerJuangStepPattern(6,"c"))
rabinerJuangStepPattern(6,"c")

BAC.month <- window(BAC, start=Sys.Date() - 30)
SPY.month <- window(SPY, start=Sys.Date() - 30)

plot(dtw(BAC.month,SPY.month,keep=TRUE,step=rabinerJuangStepPattern(6,"c")), type="twoway",offset=-2)


zscore <- function(v,the_mean,the_sd) {
  (v - the_mean) / the_sd
}

zscore_stock <- function(stock_name, limit = 30) {
  stock <- get(stock_name)
  column <- paste(stock_name, ".Adjusted", sep="")
  stock.limit <- window(stock, start=Sys.Date() - limit)
  meanb <- mean(stock.limit[,column])
  sdb <- apply(stock.limit[,column], 2, sd)
  z <- apply(stock.limit[,column], 2, zscore, meanb,sdb)
  z
}

daylimit <- 30

zBAC <- zscore_stock('BAC', limit = daylimit)
zSPY <- zscore_stock('SPY', limit = daylimit)
zJPM <- zscore_stock('JPM', limit = daylimit)

dtw(zBAC,zSPY, keep=TRUE)$distance
dtw(zBAC,zJPM, keep=TRUE)$distance
plot(dtw(zBAC,zSPY,keep=TRUE,step=rabinerJuangStepPattern(6,"c")), type="twoway",offset=-2)
plot(dtw(zBAC,zJPM,keep=TRUE,step=rabinerJuangStepPattern(6,"c")), type="twoway",offset=-2)

for(i in 1:length(tickers)) {
  ticker <- tickers[i]
  filename <- paste(ticker,".zoo",sep="")
  write.zoo(as.zoo(get(ticker)), file = filename)
}
