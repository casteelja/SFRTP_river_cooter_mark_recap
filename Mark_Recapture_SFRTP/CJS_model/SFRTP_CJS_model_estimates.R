# calculating survival using the Cormack Jolly-Seber model

turtle<-read.csv("SFRTP-Pc_mr_rum_only.csv", header = TRUE)
head(turtle)

#create detection history  #creating a matrix of the columns of data
dethistNG <- as.matrix(turtle[2:ncol(turtle)])

#create input file  #colapses into the capture history
sink('turtle.inp') #save detection history as MARK input file
for (i in 1:nrow(dethistNG)) {
  cat(paste(dethistNG[i,],collapse=''),1,';\n')  #there is a one because we have one row for each turtle
}
sink()

#convert to .inp format
library(RMark)
dta=convert.inp('turtle')  #  convert data for RMARK

#remove all rows were cap hist is all 0s
ind<-which(dta$ch == "00000000")




##create process data and design data
#process data
pd=process.data(dta,model="CJS", begin.time = 2009)

#design data
dd=make.design.data(pd)

#fit dot model
m=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
       model="CJS",model.parameters=list(Phi=list(formula=~1),p=list(formula=~1))) 

m$results$real
m$results$derived


#fit dot time model
m2=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="CJS", model.parameters=list(Phi=list(formula=~1),p=list(formula=~time))) 
m2$results$real
m2$results$beta



#for the model where we set the years that are not working to the average survival values ( I think it was 0.84?)
m3<- mark(pd, dd, retry = 10, model = "CJS", 
           model.parameters=list(Phi=list(formula=~time, fixed=list(time=c(2011,2015), value = 0.837)), 
                                 p=list(formula=~1)))
m3$results$real
m3$results$beta



#for the model where we set the value of 2011 = 2010 and 2015=2014 (or some combination like this)
dd$Phi$yearsame <- dd$Phi$time #this copies over the values from the time covariate
dd$Phi$yearsame[dd$Phi$yearsame == 2011] <- 2010
dd$Phi$yearsame[dd$Phi$yearsame == 2015] <- 2014
#this will change all 2011 to 2010 (so they get one estimated value for both years) and then the same for 2015 to 2014

m4=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="CJS", model.parameters=list(Phi=list(formula=~time),p=list(formula=~1))) 
m4$results$real
m4$results$beta
m4$output
summary_ch(pd)


collect.models()






# Deriving abundance estimates from the CJS capture probability

x09<-turtle$X2009==1
summary(x09)
N09<-86/0.35
L09<-86/0.31
U09<-86/0.38
summary(turtle$X2010==1)
N10<-201/0.35
L10<-201/0.31
U10<-201/0.38
summary(turtle$X2011==1)
N11<-196/0.35
L11<-196/0.31
U11<-196/0.38
summary(turtle$X2012==1)
N12<-94/0.35
L12<-94/0.31
U12<-94/0.38
summary(turtle$X2015==1)
N15<-89/0.35
L15<-89/0.31
U15<-89/0.38
summary(turtle$X2016==1)
N16<-103/0.35
L16<-103/0.31
U16<-103/0.38
summary(turtle$X2017==1)
N17<-72/0.35
L17<-72/0.31
U17<-72/0.38
summary(turtle$X2019==1)
N19<-130/0.35
L19<-130/0.31
U19<-130/0.38
