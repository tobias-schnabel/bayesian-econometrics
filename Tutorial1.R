# Tutorial 1
rm(list = ls(all = TRUE)) ###CLEAR ALL

##1##
#get data
rawdata = read.csv('NLInflation.csv')

#make TS
require(lubridate)
#create date object
rawdata$dateobj = lubridate::parse_date_time(rawdata$Month, "ym")
#create xts
tsdat = xts(rawdata$AnnualCPIchange, order.by=rawdata$dateobj)
names(tsdat) = "AnnualCPIchange"

##2##
#plot
plot(tsdat)

#re-extract to regular df
data = data.frame(date=index(tsdat), coredata(tsdat))

#sumstats
print(mean(tsdat$AnnualCPIchange))
print(median(tsdat$AnnualCPIchange))
print(min(tsdat$AnnualCPIchange))
print(max(tsdat$AnnualCPIchange))
print(sd(tsdat$AnnualCPIchange))

##3##
#regression model
model1 = lm(AnnualCPIchange ~ date, data = data)

##4##
summary(model1)

##5##
#CPI change requires a prior, months are fixed and known in advance with 0 uncertainty

##6##
#flat prior would make sense, inflation is unpredictable

