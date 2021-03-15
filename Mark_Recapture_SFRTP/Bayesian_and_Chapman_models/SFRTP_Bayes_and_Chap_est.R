###########
# Sequential Bayes

bayesSequential <- function(nsamp, marked, caught, recaps){
  
  results = data.frame(matrix(ncol = 11, nrow = nsamp))
  x <- c('occasion', 'marked', 'caught', 'recaps', 'chapman', 'varchap', 'LCIchap', 'UCIchap', 'N', 'LL', "UL")
  colnames(results) <- x
  
  for(i in 1:nsamp){
    marked = marked + (caught[i] - recaps[i]) #total number marked 
    N = seq(marked+10, 1500, length.out = 1000)
    
    #calculates chapman estimate
    chap = ((marked+1)*(caught[i]+1))/(recaps[i]+1) - 1
    
    #calculates chapman estimate variance
    varchap = ((marked+1)*(caught[i]+1)*(marked-recaps[i])*(caught[i]-recaps[i]))/(((recaps[i]+1)^2)*(recaps[i]+2))
    
    #calculates chapman estimate confidence intervals
    LCIchap = chap-1.965*sqrt(varchap[i])
    UCIchap = chap+1.965*sqrt(varchap[i])
    
    #calculate posterior distribution
    pdf_N <- rep(NA, length(N)) #vector to store probability function
    for (j in 1:length(N)){ #for each possible value
      pdf_N[j] <- (marked/N[j])^recaps[i] * (1-marked/N[j])^(caught[i]-recaps[i]) #likelihood for each value of N
    }
    post_N <- pdf_N/sum(pdf_N) #re-scale by p(x)
    
    #plot posterior probability for each possible value of N
    plot(post_N ~ N, xlim=c(0,max(N)), ylim=c(0,0.1))
    
    #scale by max of post_N
    z<-post_N/max(post_N)
    
    #find abundance with confidence intervals
    #add them to data frame
    turtle_N= N[which.max(z)]
    #turtle_N_LL = N[which(density(z,n=1000)$x >= 0.025)[1]]
    #turtle_N_UL = N[which(density(z,n=1000)$x >= 0.975)[1]-1]
    turtle_N_LL=N[which(density(z, n = 1000)$x < 0.026 & density(z, n = 1000)$x > 0.024)]
    ifelse(length(turtle_N_LL) > 1, turtle_N_LL <- mean(turtle_N_LL), turtle_N_LL <- turtle_N_LL)
    turtle_N_UL=N[which(density(z, n = 1000)$x > 0.974 & density(z, n = 1000)$x < 0.976)]
    ifelse(length(turtle_N_UL) > 1, turtle_N_UL <- mean(turtle_N_UL), turtle_N_UL <- turtle_N_UL)
    
    results$occasion[i] <- i
    results$marked[i] <- marked
    results$caught[i] <- caught[i]
    results$recaps[i] <- recaps[i]
    results$chapman[i] <- chap
    results$varchap[i] <- varchap
    results$LCIchap[i] <- LCIchap[i]
    results$UCIchap[i] <- UCIchap[i]
    results$N[i] <- turtle_N
    results$LL[i] <- turtle_N_LL
    results$UL[i] <- turtle_N_UL
  }
  return(results)
}



## Rum Island Site Only

#2010
nsamp2010 = 3 - 1 #number of sampling occasions minus 1
marked2010 = 62 #number marked and released on 1st occasions (sampling occasion 1)
caught2010 = c(97, 97) #number of captures (1st occasion always 0)
recaps2010 = c(19, 35) #number of recaps (1st occasion always 0)

results2010 <- bayesSequential(nsamp2010, marked2010, caught2010, recaps2010)


#2011
nsamp2011 = 3 - 1  #number of total sampling occasions minus 1
marked2011 = 97
caught2011 = c(67,70)
recaps2011 = c(12,25)

results2011 <- bayesSequential(nsamp2011, marked2011, caught2011, recaps2011)


#2019 
nsamp2019RI = 3 - 1 #3 sampling occasions minus 1
marked2019RI = 73
caught2019RI = c(54,45)
recaps2019RI = c(19,20)

results2019RI <- bayesSequential(nsamp2019RI, marked2019RI, caught2019RI, recaps2019RI)

