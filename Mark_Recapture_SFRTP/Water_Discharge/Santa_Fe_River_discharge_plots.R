################################################################################
#
# this code reads daily water guage data from a USGS site, computes the weekly
# composite mean discharge, the weekly climatology over the entire time frame,
# and anomolies based on the the time span of climatoloty.  Requires the
# waterData package.  Visit the following pages for site numbers and metadata
#
# sites:       http://maps.waterdata.usgs.gov/mapper/
# stat codes:  http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table
# code codes:  http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes     (choose physical)
#
# Fort White Santa Fe River  gauge 02322500
#
################################################################################
#install.packages("waterData")
#install.packages("hydroTSM")
#install.packages("zoo")
#SETUP
rm(list=ls());
library("waterData")
library("hydroTSM")
library("zoo")
library("lubridate")
library("tidyverse")
library("dplyr")
library("Hmisc")



#station to analyze
#Santa Fe Fort White
station = '02322500'   

################################################################################ 
#
#                     READ AND PREPARE DATA
#
################################################################################ 
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series - want to download whole period of record
#change start date to first day of data for your station
#sdate is start data
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1986-10-01") 
#dis is the discharge for the period of record

#get some date components
dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
dis$month   = as.numeric(strftime(dis$dates,format="%m")) 


#years of interest
#discharge for turtles 2007 to 2018 (2 years before and after turtle data)
#disT is discharge for the period of turtle data
disT <- subset(dis, dis$dates < "2020-01-01" & dis$dates > "2006-12-31")

#get yearly mean, sd, and var and annual sum of discharge
#discharge
disT.yr  = aggregate(val~year,data=disT,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x,na.rm=T)))
disT.yr  = do.call('data.frame',disT.yr)
names(disT.yr)[2:5] = c('avg','sd','var','sum')                      
disT.yr$CV  = (disT.yr$sd/disT.yr$avg)*100





###plots
par(mfrow=c(1,1))
par(mfrow=c(3,1), mar= c(5.1, 4.7,4.1,2.1))

plot(disT.yr$year,disT.yr$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, 
     ylim=c(0,2000), xlim=c(2007,2019),cex.lab=1.5, cex.axis=1.5, main = "Average", cex.main = 1.5)
lines(lowess(disT.yr$year,disT.yr$avg),col='red', lwd=4)
abline(h=mean(disT.yr$avg), col="blue", lwd=4, lty=2)
mtext("A", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)

plot(disT.yr$year,disT.yr$var,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, 
     ylim=c(0,3500000), xlim=c(2007,2019),cex.lab=1.5, cex.axis=1.5, main = "Variance", cex.main = 1.5)
lines(lowess(disT.yr$year,disT.yr$var),col='red', lwd=4)
abline(h=mean(disT.yr$var), col="blue", lwd=4, lty=2)
mtext("B", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)

plot(disT.yr$year,disT.yr$CV,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, 
     ylim=c(0,125), xlim=c(2007,2019),cex.lab=1.5, cex.axis=1.5, main = "Coefficient of Variation", cex.main = 1.5)
lines(lowess(disT.yr$year,disT.yr$CV),col='red', lwd=4)
abline(h=mean(disT.yr$CV), col="blue", lwd=4, lty=2)
mtext("C", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)




#######################################################################################################################

# create some boxplots
#get site name to use in plot titles and such
stinfo  = siteInfo(station)
stname = stinfo$staname

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1986-10-01")

str(dis)
summary(dis)

#code are standards from USGS 00060=discharge, 00065=gage height, 00010= Temp
#stat USGS standards 00001 max, 00003 mean daily
#see examples https://owi.usgs.gov/R/dataRetrieval.html#7

#what are the names of our columns?
names(dis)

#use dplyr to rename
#the pattern is new_name= old_name

dis<-rename(dis, discharge = val)

names(dis)

#now let's removed any erroneous or estimated values or missing

#what values are there
unique(dis$qualcode)

#let's remove NA or only keep "A"

#remove NA 
dis <- filter(dis, qualcode != "NA")
#!= is "not equal to" so filter what is not equal to NA

#only keep A
dis <- filter(dis, qualcode == "A")
#== is "equal to" something

#just check
unique(dis$qualcode)

####################
#ok use ggplot to make a plot
####################

names(dis)

#make a dot plot of discharge by year


ggplot(dis, aes(dates, discharge)) +
  geom_point() +
  ggtitle(stname) +
  xlab("Year") + ylab("Discharge (Cubic Feet per Second)")


#now let's create a new variable called "year" and extract year from date
#if we use str(dis) we see date is recognized as a date, so just use lubridate

dis$year<-year(dis$dates)

#using label=TRUE gives us the month as a word
dis$month <-month(dis$dates,label=TRUE)

##need to convert from numeric to character month
head(dis)

#ok make a dot plot by year
ggplot(dis, aes(month, discharge)) +
  geom_point() +
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)") +
  facet_wrap(~year)+
  
  theme_set(theme_gray(base_size = 18))

#add bootstrap mean and 95% CI to plot

ggplot(dis, aes(month, discharge)) +
  geom_point() +
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)") +
  facet_wrap(~year) +
  stat_summary(fun.data = "mean_cl_boot",colour = "red", size = 0.5)


#############

#June of 2012
dis_jun_12<-filter(dis,year==2012) %>% 
  filter(month=="Jun")
max_jun_12<-aggregate(discharge~month,dis_jun_12, max)
mean_jun_12<-aggregate(discharge~month,dis_jun_12, mean)

#July of 2012
dis_jul_12<-filter(dis,year==2012) %>% 
  filter(month=="Jul")
max_jul_12<-aggregate(discharge~month,dis_jul_12, max)
mean_jul_12<-aggregate(discharge~month,dis_jul_12, mean)

#September of 2017
dis_sep_17<-filter(dis,year==2017) %>% 
  filter(month=="Sep")
max_sep_17<-aggregate(discharge~month,dis_sep_17, max)
mean_sep_17<-aggregate(discharge~month,dis_sep_17, mean)

dis_oct_17<-filter(dis,year==2017) %>% 
  filter(month=="Oct")
max_oct_17<-aggregate(discharge~month,dis_oct_17, max)
mean_oct_17<-aggregate(discharge~month,dis_oct_17, mean)

### mean by month per year
mpy<-dis %>% group_by(year,month) 
mpy<-summarise(mpy,mean_discharge=mean(discharge))
mpy

# Creating box plots

#all data by month
ggplot(dis, aes(month, discharge)) +
  geom_boxplot()+
  ggtitle(stname) +
  xlab("Date") +
  ylab("Discharge (Cubic Feet per Second)")+
  ylim(0,17500)

##drop outliers and add mean

#all data with mean
mean_annual <- aggregate(discharge ~ month, dis, mean) 


ggplot(mpy, aes(month, mean_discharge))+ 
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size= 3, shape=20, color="red", fill="red", aes(color="Longterm mean")) +
  xlab("Month") +
  ylab("Discharge (Cubic Feet per Second)") +
  labs(color="")+
  theme_classic()+
  geom_point(data=mean_jun_12, aes(x=month,y=discharge, color="2012"),size=3)+
  geom_point(data=mean_jul_12, aes(x=month,y=discharge, color="2012"),size=3)+
  geom_point(data=mean_sep_17, aes(x=month,y=discharge, color="2017"),size=3)+ 
  geom_point(data=mean_oct_17, aes(x=month,y=discharge, color="2017"),size=3)+ 
  scale_color_manual(values = c("#00AFBB", "#E7B800", "green","purple"))
