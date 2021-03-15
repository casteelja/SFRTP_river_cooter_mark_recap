# abundance and survival estiamtes using the robust design model
# using data Rum Island data only for the first model
# using data from all sites for the second model and incorporating effort

#### Rum Island only ####

turtle<-read.csv("robust_rum.csv", header = TRUE)
head(turtle)


#create detection history  #creating a matrix of the columns of data
dethistNG <- as.matrix(turtle[2:ncol(turtle)])


#create input file  #colapses into the capture history
sink('turtle_robust4.inp') #save detection history as MARK input file


for (i in 1:nrow(dethistNG)) {
  cat(paste(dethistNG[i,],collapse=''),1,';\n')  #there is a one because we have one row for each turtle
}
sink()



#convert to .inp format
library(RMark)
dta=convert.inp('turtle_robust4')  #  convert data for RMARK



ind<-which(dta$ch == "0.0000000.0.0.0.000")
dta2<-dta[-ind,]  #deleating all the rows with all 0s
nrow(dta2)


##create process data and design data

# add effort (use area likely but maybe distance of sites )  add effort to the p=c

#process data

pd=process.data(dta,model="RDHuggins",time.intervals=c(0,1,0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,0),begin.time=2009)

#design data
dd=make.design.data(pd)

#fit dot model


m1=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="RDHuggins",model.parameters=list(S=list(formula=~1),
                                                p=list(formula=~1,share=TRUE),
                                                GammaDoublePrime=list(formula=~1,share=TRUE))) 

m1$results$real
m1$results$derived
m1$results$beta



m2=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="RDHuggins",model.parameters=list(S=list(formula=~time),
                                                p=list(formula=~1,share=TRUE),
                                                GammaDoublePrime=list(formula=~1,share=TRUE))) 

m2$results$real
m2$results$derived
m2$results$beta



#for the model where we set the years that are not working to the average survival values 
m3 <- mark(pd, dd, retry = 10, 
           model = "RDHuggins", model.parameters=list(S=list(formula=~time, 
                                                             fixed=list(time=c(2013), value = 0.839)), 
                                                      p=list(formula=~1,share=TRUE),
                                                      GammaDoublePrime=list(formula=~1,share=TRUE)))
m3$results$real
m3$results$derived
m3$results$beta



collect.models()


########################################################################################################################


#### All Sites ####
turtle<-read.csv("SFRTP-Pc_robust_effort_all.csv", header = TRUE)
head(turtle)

effort.table<-read.csv("robust_effort_table.csv",header=TRUE)
head(effort.table)

#create detection history  #creating a matrix of the columns of data
dethistNG <- as.matrix(turtle[2:ncol(turtle)])

effort<- as.matrix(effort.table[2:ncol(effort.table)])

#create input file  #colapses into the capture history
sink('turtle_robust4.inp') #save detection history as MARK input file


for (i in 1:nrow(dethistNG)) {
  cat(paste(dethistNG[i,],collapse=''),1,effort[i,1],effort[i,2],effort[i,3],effort[i,4],effort[i,5],
      effort[i,6],effort[i,7],effort[i,8],';\n')  #there is a one because we have one row for each turtle
}
sink()



#convert to .inp format
library(RMark)
dta=convert.inp('turtle_robust4',covariates=c('effort1','effort2','effort3','effort4','effort5','effort6','effort7','effort8'))  #  convert data for RMARK



ind<-which(dta$ch == "0.0000000.0.0.0.000")
dta2<-dta[-ind,]  #deleating all the rows with all 0s
nrow(dta2)


##create process data and design data

# add effort (use area likely but maybe distance of sites )  add effort to the p=c

#process data

pd=process.data(dta2,model="RDHuggins",time.intervals=c(0,1,0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,0))

#design data
dd=make.design.data(pd)

#fit dot model
m1=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="RDHuggins",model.parameters=list(S=list(formula=~1),
                                                p=list(formula=~1,share=TRUE),
                                                GammaDoublePrime=list(formula=~1,share=TRUE))) 

m1$results$real
m1$results$derived
m1$results$beta



#fit dot model
m2=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="RDHuggins",model.parameters=list(S=list(formula=~1),
                                                p=list(formula=~time,share=TRUE),
                                                GammaDoublePrime=list(formula=~1,share=TRUE))) 

m2$results$real
m2$results$derived




m3=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
       model="RDHuggins",model.parameters=list(S=list(formula=~1),
                                               p=list(formula=~effort,share=TRUE),
                                               GammaDoublePrime=list(formula=~1,share=TRUE))) 

m3$results$real
m3$results$derived





m4=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="RDHuggins",model.parameters=list(S=list(formula=~time),
                                                p=list(formula=~effort,share=TRUE),
                                                GammaDoublePrime=list(formula=~1,share=TRUE))) 

m4$results$real
m4$results$derived
m4$results$beta




m5=mark(pd,dd,retry = 10,  #retry with 10 different start to give a better probability of the model working
        model="RDHuggins",model.parameters=list(S=list(formula=~time),
                                                p=list(formula=~1,share=TRUE),
                                                GammaDoublePrime=list(formula=~1,share=TRUE))) 

m5$results$real
m5$results$derived




collect.models()
