# estimating abundance using the Huggin's model

#### 2010 ####
turtle<-read.csv("SFRTP-Pc_markrecap2010.csv")
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
ind<-which(dta$ch == "000")

dta2<-dta[-ind,]  #deleating all the rows with all 0s
nrow(dta2)

##create process data and design data
#process data
pd=process.data(dta2,model="Huggins", begin.time = 2010)

#design data
dd=make.design.data(pd)

# set p=c
m=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="Huggins",model.parameters=list(p=list(formula=~1,share=TRUE))) 

m$results$real
m$results$derived




#### 2011 ####

turtle<-read.csv("SFRTP-Pc_markrecap2011.csv")
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
ind<-which(dta$ch == "000")

dta2<-dta[-ind,]  #deleating all the rows with all 0s
nrow(dta2)

##create process data and design data
#process data
pd=process.data(dta2,model="Huggins", begin.time = 2019)

#design data
dd=make.design.data(pd)

#set p=c
m2=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="Huggins",model.parameters=list(p=list(formula=~1,share=TRUE))) 

m2$results$real
m2$results$derived




#### 2019 ####

turtle<-read.csv("SFRTP-Pc_markrecap2019.csv")
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
ind<-which(dta$ch == "000")

dta2<-dta[-ind,]  #deleating all the rows with all 0s
nrow(dta2)

##create process data and design data
#process data
pd=process.data(dta2,model="Huggins", begin.time = 2019)

#design data
dd=make.design.data(pd)

#fit dot model
m=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
       model="Huggins",model.parameters=list(p=list(formula=~1,share=TRUE))) 

m$results$real
m$results$derived