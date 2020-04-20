library(DT)
library(data.table)
library(tidyverse)
QubtcallData= fread(input = "/Users/iriszhao/Documents/hackathon/btc_usd.csv", verbose = FALSE)
QuredditData= fread(input = "/Users/iriszhao/Documents/hackathon/reddit.csv", verbose = FALSE)
weight=1
Time=10
Lag=10


QubtcData=QubtcallData[1:100000,]
dim(QubtcData)
View(head(QubtcData))
tail(QubtcData)
head(QuredditData)
tail(QuredditData)




QuredditData=QuredditData[order(QuredditData$created_utc),]
NewredditData=QuredditData[1:10000,];
NewredditData$Date=as.POSIXct(NewredditData$created_utc,tz='GMT',origin="1970-01-01")
a=round(as.numeric(NewredditData$Date)/Time)*Time
NewredditData$miniDate=as.POSIXct(a,tz='GMT',origin="1970-01-01")
# View(NewredditData)
Groupbytime=function(data,time=Time){
  a=as.numeric(as.POSIXct(data$datetime,tz='GMT',origin="1970-01-01") )
  a=round(a/time)*time
  a=as.POSIXct(a,tz='GMT',origin="1970-01-01")
  data$miniDate=a
  return(data)
}
QubtcData=Groupbytime(QubtcData)
QubtcData=data.table(QubtcData)
Trade=QubtcData[, .(miniPrice = mean(price, na.rm = TRUE)), by = miniDate]


library(SentimentAnalysis)
NewredditData$body<-iconv(NewredditData$body,"WINDOWS-1252","UTF-8")
NewredditData$sentiment=analyzeSentiment(NewredditData$body)$SentimentQDAP
#-----------------------------------------------------
NewredditData$senti=NewredditData$sentiment*weight*NewredditData$score+NewredditData$sentiment
NewredditData=data.table(NewredditData)
Comments=NewredditData[, .(miniSenti = mean(senti, na.rm = TRUE)), by = miniDate]
Comments[is.na(miniSenti),]=0
Traindata=merge(Comments,Trade)
Traindata$lag= Traindata$miniPrice[Lag+1:nrow(Traindata)]
Traindata=Traindata[1:(nrow(Traindata)-Lag),]
#----------------------------------------------------------------------------------------------------
# Model,corelated
cor(Traindata$miniSenti,Traindata$lag)


#-----------------------
ggplot(Traindata, aes(miniDate, c(miniSenti,miniPrice))) +
  geom_line()

View(Comments)
View(QubtcData)
View(head(QubtcData,100))
data=QubtcData
a=QubtcData$datetime
names(QubtcData)
# View(head(NewredditData))
# tail(NewredditData$Date)
# install.packages("SentimentAnalysis")
library(SentimentAnalysis)
NewredditData$body<-iconv(NewredditData$body,"WINDOWS-1252","UTF-8")
NewredditData$sentiment=analyzeSentiment(NewredditData$body)
# names(NewredditData$sentiment)
NewredditData$senti=NewredditData$sentiment$SentimentQDAP
SentiData=NewredditData
#score and up relationship with sentiment

#average every minis
SentiData=SentiData[order(SentiData$Date),]
Timesenti=function(data,time=3600,weight=1){
  i=1
  j=2
  data$multisenti=data$senti*weight*data$score+data$senti
  Newdata=numeric()
  while(j<=nrow(data)){
    while(as.numeric(data$created_utc[j]-data$created_utc[i])<=time&j<=nrow(data)&i<=nrow(data)){
      j=j+1
    } 
    
    Newdata=append(Newdata,mean(data$multisenti[i:j],na.rm =T))
    i=j+1
    j=i+1
  }
  return(Newdata)
}







senti=Timesenti(SentiData)
# hist(scale(SentiData$senti))
plotSentiment(senti)
names(SentiData)
names(QubtcData)
ggplot(SentiData[1:100,], aes(Date, senti)) +
  geom_line()
  
  ggplot(QubtcData[1:10,], aes(datetime, price))+geom_line()

  QubtcData$price
plot()

?mean()
summary(data$created_utc)

as.numeric(SentiData$created_utc[nrow(SentiData)]-SentiData$created_utc[1])/60

mean(data$SentimentQDAP[5:10])
data$senti
SentiData=NewredditData

a=head(NewredditData)
a$sentiment=analyzeSentiment(a$body)
View(a)
