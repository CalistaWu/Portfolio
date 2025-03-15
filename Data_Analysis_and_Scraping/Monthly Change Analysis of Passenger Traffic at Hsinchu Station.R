library(jsonlite)
library(tidyverse)
library(magrittr)

data.file <- "data.json"
data.url <- "https://ods.railway.gov.tw/tra-ods-web/ods/download/dataResource/8ae4cabf6973990e0169947ed32454b9"
download.file( url= data.url,destfile = data.file )

data <- fromJSON(data.file)
Hsinchu_dt <- filter( data, staCode =="1210" )

Hsinchu_dt <- Hsinchu_dt %>%
  mutate(trnOpDate = as.Date(as.character(trnOpDate), format = "%Y%m%d")) %>%
  filter(format(trnOpDate, "%d") == "01")

df <- data.frame(
  Month = character(),
  Incoming_changes = numeric(),
  Outgoing_changes = numeric()
)
n <- as.numeric(format(Hsinchu_dt$trnOpDate, "%m"))
n <- tail(n, -1)

Month <- c()
Incoming_changes<- c()
Outgoing_changes<- c()


# 逐月計算變化並存入新的資料框
for ( i in n) {
  Month <- append(Month,as.character(i))
  Incoming_changes <- append(Incoming_changes,as.numeric(Hsinchu_dt$gateInComingCnt[i]) - as.numeric(Hsinchu_dt$gateInComingCnt[i-1]))
  Outgoing_changes <- append(Outgoing_changes,as.numeric(Hsinchu_dt$gateOutGoingCnt[i]) - as.numeric(Hsinchu_dt$gateOutGoingCnt[i-1]))
  }


df <- data.frame(
  Month = Month,
  Incoming_changes = Incoming_changes,
  Outgoing_changes = Outgoing_changes
)

print(df)
