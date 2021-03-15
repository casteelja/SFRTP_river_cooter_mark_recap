## Abundance and survival plots for SFRTP river cooters in the Santa Fe River, FL

#### Bayesian and Chapman estimate ####

N<-data.frame(N=c(715,632,849,661,308,299,     690,552,799,540,806,682))   # N estimate
U<-c(1200,815,1422,916,1447,1457,     935,680,1161,691,983,792)            # Upper CIs
L<-c(522,527,579,531,171,189,         445,423,438,388,629,573)             # Lower CIs
yr<-c(2010,2010,2011,2011,2019,2019,      2010,2010,2011,2011,2019,2019)   # year
M<-c("B1","B2","B1","B2","B1","B2","C1","C2","C1","C2","C1","C2")          # Model   B1 & B2 = Bayes     C1 & C2 = Chapman

Fig3<-data.frame(N = N, yr = yr, L = L, U = U, M = M)

ggplot(Fig3, aes(x= yr, y= N, color=M))+
  geom_point(size=2, position = position_dodge2(width=0.25))+
  geom_errorbar(aes(ymin=L,ymax=U), position="dodge2", width=0.25, stat= "identity")+
  labs(x="Year", y="Abundance", title="", color="Method")+
  theme(legend.position="right",panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,size=1, linetype="solid"), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#E69F00","#F0E442","#0072B2","#56B4E9"))+
  scale_x_continuous(breaks=c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))



#### Robust Design Rum only and All sites ####

RD<-c(402, 391, 381,440,823,907,643,560,      375,370,361,410,388,449,313,240)                 # N estimate
yrRD<-c(2009,2010,2011,2012,2015,2016,2017,2019,    2009,2010,2011,2012,2015,2016,2017,2019)   # year
lclRD<-c(321,342,333,353,693,766,536,502,         307,333,324,338,319,373,254,212)             # Lower 95% CIs
uclRD<-c(512,458,447,556,987,1083,780,637,       462,419,409,502,477,546,394,277)              # Upper 95% CIs
M<-c("All Sites","All Sites","All Sites","All Sites","All Sites","All Sites","All Sites","All Sites",           # site(s)
     "Rum Island","Rum Island","Rum Island","Rum Island","Rum Island","Rum Island","Rum Island","Rum Island")

Fig6 <- data.frame(RD = RD, yrRD = yrRD, lclRD = lclRD, uclRD = uclRD, M = M)

ggplot(Fig6, aes(x= yrRD, y= RD, color=M))+
  geom_point(size=2, position = position_dodge2(width=0.25))+
  geom_errorbar(aes(ymin=lclRD,ymax=uclRD), position="dodge2", width=0.25, stat= "identity")+
  labs(x="Year", y="Abundance", title="", color="Method")+
  theme(legend.position="right",panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,size=1, linetype="solid"), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#E69F00","#0072B2"))+
  scale_x_continuous(breaks=c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))



#### Huggins low, Huggins high, CJS, RD, all rum island only ####

# Derived CJS abundance estimates
N09<-86/0.35
L09<-86/0.31
U09<-86/0.38
N10<-201/0.35
L10<-201/0.31
U10<-201/0.38
N11<-196/0.35
L11<-196/0.31
U11<-196/0.38
N12<-94/0.35
L12<-94/0.31
U12<-94/0.38
N15<-89/0.35
L15<-89/0.31
U15<-89/0.38
N16<-103/0.35
L16<-103/0.31
U16<-103/0.38
N17<-72/0.35
L17<-72/0.31
U17<-72/0.38
N19<-130/0.35
L19<-130/0.31
U19<-130/0.38




N<-c(328,515,374,466,363,569,821,1287,924,1449,645,1012,554,
     373,369,360,408,386,447,313,239,
     N09,N10,N11,N12,N15,N16,N17,N19)
yr<-c(2009,2009,2010,2011,2012,2012,2015,2015,2016,2016,2017,2017,2019,
      2009,2010,2011,2012,2015,2016,2017,2019,
      2009,2010,2011,2012,2015,2016,2017,2019)
ucl<-c(383,683,460,611,424,755,959,1708,1079,1922,754,1342,636,
       461,417,407,500,475,544,392,276,
       U09,U10,U11,U12,U15,U16,U17,U19)
lcl<-c(283,392,316,317,313,433,708,980,797,1103,557,771,493,
       306,332,324,337,318,372,253,211,
       L09,L10,L11,L12,L15,L16,L17,L19)
M<-c("Huggins H","Huggins L","Huggins","Huggins","Huggins H","Huggins L","Huggins H","Huggins L","Huggins H","Huggins L","Huggins H","Huggins L","Huggins",
     "Robust Design","Robust Design","Robust Design","Robust Design","Robust Design","Robust Design","Robust Design","Robust Design",
     "CJS","CJS","CJS","CJS","CJS","CJS","CJS","CJS")

Fig4<-data.frame(N = N, yr = yr, lcl = lcl, ucl = ucl, M = M)

ggplot(Fig4, aes(x= yr, y= N, color=M))+
  geom_point(size=2, position = position_dodge2(width=0.25))+
  geom_errorbar(aes(ymin=lcl,ymax=ucl), position="dodge2", width=0.25, stat= "identity")+
  labs(x="Year", y="Abundance", title="", color="Model")+
  theme(legend.position="right",panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,size=1, linetype="solid"), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#E69F00","#F0E442","#0072B2","#56B4E9","#000000"))+
  scale_x_continuous(breaks=c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))



#### CJS and Robust Design survival estimates ####

surv<-c(0.959,0.951,0.837,0.647,0.955,0.831,0.837,     # survival estimate
        0.878,0.880,0.917,0.796,0.839,0.753,0.762)
L<-c(0.479,0.675,0.806,0.540,0.362,0.623,0.806,        # lower 95% CIs
     0.720,0.761,0.602,0.601,0.810,0.560,0.555)
U<-c(0.998,0.994,0.863,0.741,0.999,0.936,0.863,        # upper 95% CIs
     0.953,0.944,0.988,0.910,0.864,0.879,0.891)
yr<-c(2009,2010,2011,2012,2015,2016,2017)              # Year
M<-c("CJS","CJS","CJS","CJS","CJS","CJS","CJS","Robust Design","Robust Design","Robust Design","Robust Design","Robust Design","Robust Design","Robust Design")

Fig5<-data.frame(surv = surv, yr = yr, L = L, U = U, M = M)

ggplot(Fig5, aes(x= yr, y= surv, color=M))+
  geom_point(size=2, position = position_dodge2(width=0.25))+
  geom_errorbar(aes(ymin=L,ymax=U), position="dodge2", width=0.25, stat= "identity")+
  labs(x="Year", y="Survival Rate", title="", color="Model")+
  ylim(0,1)+
  theme(legend.position="right",panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,size=1, linetype="solid"), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#009E73","#E69F00"))+
  scale_x_continuous(breaks=c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

