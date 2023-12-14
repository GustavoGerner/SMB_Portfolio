#Portfolio simulation

#Packages

library(dplyr)
library(readxl)
library(xts)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(sandwich)
library(lmtest)

##############################Dealing with data#########################################
#Data (modified from Economatica) - all with free market rate (DI) already subtracted:

returns <- read_excel("retornos.xlsx",na="-")
volume <-read_excel("Cópia de volume.xlsx",na = "-")
mkt_cap <-read_excel("Cópia de valor.xlsx",na = "-")
BTM <-read_excel("BTMRatio.xlsx",na = "-")
MKT <- read_excel("MKT.xls") 
#Row/Column Names:

tick_names <- colnames(returns)

K     <- ncol(returns)

returns[,1] <- as.Date(as.POSIXct(returns[[1]]))

names(returns)[1]<-"Date"

# From chr to double:

returns[,2:K] <- as.data.frame(lapply(returns[,2:K],
                                      function(x) as.numeric(as.character(x))))

#From Wide to Long:

returns <- gather(returns, ticker, ret, tick_names[2]:tick_names[K])

volume <- gather(volume, ticker, vol, tick_names[2]:tick_names[K])

names(volume)[1]<-"Date"

mkt_cap <- gather(mkt_cap, ticker, mkt_cap, tick_names[2]:tick_names[K])

names(mkt_cap)[1]<-"Date"

BTM <- gather(BTM, ticker, BTM, tick_names[2]:tick_names[K])

names(BTM)[1]<-"Date"

#Joining Datasets:
df <- merge(returns, volume, by=c("Date","ticker"))
df <- merge(df, mkt_cap, by=c("Date","ticker"))


#Calculating monthly market rate (Bovespa)

MKT$mkt<-MKT$Rm_minus_Rf+1
MKT<-MKT%>%
  select(year,month,mkt)%>%
  mutate(year_month = year*100+month )%>%
  group_by(year_month)%>%mutate(mkt = prod(mkt)-1)

MKT<-unique(MKT)

#Selecting the 1st hundred tickers by the year's volume 
#(to only deal with sufficiently liquid assets)

filter = df %>%
  select(Date, ticker, vol)%>%
  mutate(year = year(df$Date)) %>%
  group_by(year, ticker) %>%
  mutate(volume_average = mean(vol/1000000, na.rm = TRUE)) %>%
  filter(row_number()==n()) %>%
  select(year, ticker, volume_average) %>%
  drop_na(volume_average) %>%
  group_by(year) %>%
  arrange(year, desc(volume_average)) %>%
  filter(row_number()<=100) 

#Filtering Tickers:
df<- df %>% mutate(year=year(Date),month=month(Date))

df_filtered<- filter %>% left_join(df, by=c("year", "ticker"))

df_filtered<-df_filtered%>%mutate(year = year(as.Date(Date, c("%Y-%m-%d"))))%>%
  mutate(month =month(as.Date(Date, c("%Y-%m-%d"))) )




##########################Portfolios####################################################

#Portfolios based on book to market (BTM), weighted by market capitalization
#We will simulate the return of the 1st and 4th quartiles (by BTM) 
#At the time i didn't thought about using ntile(), i think it can simplify the code a lot

dates<-unique(df_filtered$Date)

#Selecting stocks in the 1st quartile:

portfolio_1<- df_filtered %>%
  select(Date, ticker, mkt_cap)%>%
  group_by(Date, ticker) %>%
  filter(row_number()==n()) %>%
  select(Date, ticker, mkt_cap) %>%
  group_by(Date) %>%
  arrange(Date, desc(mkt_cap)) %>%
  filter(row_number()<=25) %>%
  select(Date, ticker) 

portfolio_1<-portfolio_1%>%left_join(df_filtered, by=c("Date","ticker"))

#Calculating return Weighted by capitalization (and equal weights):

portfolio_1<-portfolio_1%>%group_by(Date) %>%
mutate(r_w_1 = mean(ret*mkt_cap, na.rm = TRUE)/sum(mkt_cap,na.rm = T),r_e_1 = mean(ret,na.rm = TRUE))%>%
  select(Date,year,month,r_w_1,r_e_1)

portfolio_1<-unique(portfolio_1)

#Same process but for the 4th quartile:

portfolio_4<- df_filtered %>%
  select(Date, ticker, mkt_cap)%>%
  group_by(Date, ticker) %>%
  filter(row_number()==n()) %>%
  select(Date, ticker, mkt_cap) %>%
  group_by(Date) %>%
  arrange(Date, desc(mkt_cap)) %>%
  filter(row_number()>=76) %>%
  select(Date, ticker) %>%
  select(Date, ticker)

portfolio_4<-portfolio_4%>%left_join(df_filtered, by=c("Date","ticker"))

#Calculating return Weighted by capitalization (and equal weights):

portfolio_4<-portfolio_4%>%group_by(Date) %>%
  mutate(r_w_4 = mean(ret*mkt_cap, na.rm = TRUE)/sum(mkt_cap,na.rm = T),r_e_4 = mean(ret,na.rm = TRUE))%>%
  select(Date,year,month,r_w_4,r_e_4)

portfolio_4<-unique(portfolio_4)

#########################Risk factor - SMB###########################################

#Now we can finally create a proxy for the risk factor associated to "firm size" 
#by simulating the return of a strategy where you are long in the portfolio
#with highest BTM stocks and short on the portfolio associated with the lowest SMB stocks

portfolio_SMB <-merge(portfolio_1,portfolio_4,by=c("Date","year","month"))

portfolio_SMB <-portfolio_SMB%>%mutate(r_w_smb=r_w_4-r_w_1,r_e_smb=r_e_1-r_e_4)

####################################MKT on BTM#############################
#Now we have a measure on a possible factor related to book to market ratio
#Other usual factors can be obtained in an analogous way with this code
#An interesting exercise is to check how much this (proxy for the) factor explains 
#total market return:
portfolio_SMB<-merge(portfolio_SMB,MKT,by=c("month","year"))

portfolio_SMB<-xts(portfolio_SMB,order.by =as.yearmon(portfolio_SMB$Date)) #Time series

LS_capm<-lm(as.numeric(r_w_smb)~as.numeric(mkt), data = portfolio_SMB)

coeftest(LS_capm,vcov. = NeweyWest(LS_capm))

#As expected, smaller firms are riskier and, thus, have on average higher returns.
#However, this simple estimation does not show a significant effect
#One fact we should point out is that those discarded firms because they were too
#illiquid are generally smaller. Therefore the "small" firms selected in portfolio 4
#are not too different from those "big" firms selected in portfolio 1. 

