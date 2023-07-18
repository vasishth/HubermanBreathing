library("ggpubr")

filename = BWPilot_CombinedData_20210308
slopes<- data.frame(matrix (ncol = 11, nrow = 120))

for (i in 1:length(unique(filename$`Subject ID`))){
  print(i)
  id <- unique(filename$`Subject ID`)[i]
  print (id)
  b <- filename$`Subject ID` == as.numeric(id) 
  a <- filename[b,] # data for each individuaal subject
  c<- subset(a, Round ==1) #round 1
  
  c[c=='.'] <- NA
  
  if (nrow(c) == 0){
    next()
  }
  
  if (sum(is.na(c$RHR))==length(c$RHR))
    next()
  
  #  if (sum(!is.na(c$RHR))<2) 
   #   next()
   
  if (sum(!is.na(c$`Pre PANAS Positive Affect`))<2) 
      next()
  
  z<- c[,c(3,9,12,15, 24,41,42,44,46,47,48)] # extract hrv, rhr, respiratory rate, sleep scores
  z<- unique.data.frame(z)
  
  #reg1 <- lm(c$HRV ~ c$`Days from  Round1 Day1`,data=c, na.action = na.omit)
  slope <- lm(formula = z$HRV ~ z$`Days from  Round1 Day1`)
  slopeHRV<-slope[['coefficients']][['z$`Days from  Round1 Day1`']]
  slope <- lm(formula = z$RHR ~ z$`Days from  Round1 Day1`)
  slopeRHR<- slope[['coefficients']][['z$`Days from  Round1 Day1`']]
  slope <- lm(formula = z$`Respiration Rate` ~ z$`Days from  Round1 Day1`)
  slopeRR <- slope[['coefficients']][['z$`Days from  Round1 Day1`']]
  # slope <- lm(formula = z$`Sleep Score` ~ z$`Days from  Round1 Day1`)
  # slopesleepscore <- slope[['coefficients']][['z$`Days from  Round1 Day1`']]
  # slope <- lm(formula = z$`Hours of Sleep` ~ z$`Days from  Round1 Day1`)
  # slopehoursofsleep <- slope[['coefficients']][['z$`Days from  Round1 Day1`']]
  # slope <- lm(formula = z$`Sleep Efficiency` ~ z$`Days from  Round1 Day1`)
  # slopesleepefficiency <- slope[['coefficients']][['z$`Days from  Round1 Day1`']]
  # #slope <- lm(formula = z$`Pre STAI State Anxiety` ~ z$`Days from  Round1 Day1`)
  # #slopeprestate <- slope[['coefficients']][['z$`Days from  Round1 Day1`']]
 slope <- lm(formula = c$`Pre PANAS Positive Affect`~ z$`Days from  Round1 Day1`)
slopeprepanaspos<- slope[['coefficients']][['z$`Days from  Round1 Day1`']]
  
  
  z$deltapanaspos<- as.numeric(z$`Post PANAS Positive Affect`)- as.numeric(z$`Pre PANAS Positive Affect`)
    slope <- lm(formula = z$deltapanaspos~ as.numeric(z$`Days from  Round1 Day1`))
    slopedeltapanaspos <- slope[['coefficients']][2]
  # 
  
  
  #plot(z$`Days from  Round1 Day1`, z$RR, main = id, xlab = "Days from Round 1", ylab = 'RHR' , pch = 20, cex = 4)
#  abline(lm(z$RHR ~ z$`Days from  Round1 Day1`))

 # cor.test(as.numeric(z$`Days from  Round1 Day1`), as.numeric(z$RHR), method="pearson")
  
  
  
 # plot(z$`Days from  Round1 Day1`, z$`Respiration Rate`, main = id, xlab = "Days from Round 1", ylab = 'Respiration Rate' ,pch = 20, cex = 4, bty = 'n')
#  abline(lm(z$`Respiration Rate` ~ z$`Days from  Round1 Day1`))
 # cor.test(as.numeric(z$`Days from  Round1 Day1`), as.numeric(z$`Respiration Rate`), method="pearson")
  
  #reg1 <- lm(z$RHR ~ z$`Days from  Round1 Day1`,data=z) 
  
  slopes [i,1] <- id
  slopes [i,2] <- slopeHRV
  slopes [i,3] <- slopeRHR
  slopes [i,4] <- slopeRR
 slopes [i,5] <- slopedeltapanaspos
  slopes [i,6] <- c$`Round 1 Exercise`[1]
 # slopes [i,7] <- c$Age[1]
 # slopes [i,8] <- c$Gender[1]
 # slopes [i,7] <- slopesleepscore
#  slopes [i,8] <- slopesleepefficiency
 # slopes [i,9] <- slopehoursofsleep
#  slopes[i,10]<-slopeprestate
  slopes [i,11]<- slopeprepanaspos
  
  colnames(slopes)<- c("subjectID", "slopeHRV", "slopeRHR", "slopeRR","slopedeltapanaspos","Group", "SleepScore", "SleepEfficiency", "HoursOfSleep", "slopeprestate", "slopeprepanaspos")
  
}

#remove any empty columns

slopes <- slopes[,colSums(is.na(slopes))<nrow(slopes)]
slopes_rhrhrvRR_excludinglessthan8datapoints<- slopes # we are not excluding data. 

#statistical tests
#non-parametric

slopes_rhrhrvRR_excludinglessthan8datapoints<- slopes_rhrhrvRR_excludinglessthan8datapoints[complete.cases(slopes_rhrhrvRR_excludinglessthan8datapoints),]
kruskal.test(slopeRR~Group, data = slopes_rhrhrvRR_excludinglessthan8datapoints)
pairwise.wilcox.test(slopes_rhrhrvRR_excludinglessthan8datapoints$slopeRR, slopes_rhrhrvRR_excludinglessthan8datapoints$Group, paired = FALSE)

    
  for (i in 1:nrow(slopes_rhrhrvRR_excludinglessthan8datapoints)){
  if (slopes_rhrhrvRR_excludinglessthan8datapoints$Group[i] =="Mindful Meditation"){ slopes_rhrhrvRR_excludinglessthan8datapoints$Type[i]<- 'Mindfulness'}
  # else if (c$`Round 1 Exercise` == 'Box Breathing'){ weekly$Type[i]<- 'Box Breathing'}
  else {slopes_rhrhrvRR_excludinglessthan8datapoints$Type[i]<- 'Breathing'}
  }
  slopes_rhrhrvRR_excludinglessthan8datapoints$Type<- factor(slopes_rhrhrvRR_excludinglessthan8datapoints$Type, levels = c("Mindfulness", "Breathing"))
  
  kruskal.test(slopeRR~Type, data = slopes_rhrhrvRR_excludinglessthan8datapoints)
  
  rrm <- subset(slopes_rhrhrvRR_excludinglessthan8datapoints, Type == "Mindfulness")
  rrb <- subset(slopes_rhrhrvRR_excludinglessthan8datapoints, Type == "Breathing")
  rrsb <- subset(slopes_rhrhrvRR_excludinglessthan8datapoints, Group == "Slow Breathing")
  rrbb <- subset(slopes_rhrhrvRR_excludinglessthan8datapoints, Group == "Box Breathing")
  rrso <- subset(slopes_rhrhrvRR_excludinglessthan8datapoints, Group == "Super Oxygenation")
  
  deltapanasposm <- subset(slopes_rhrhrvRR_excludinglessthan8datapoints, Type == "Mindfulness")
  deltapanasposb <- subset(slopes_rhrhrvRR_excludinglessthan8datapoints, Type == "Breathing")
  
# t test
  
  t.test(rrm$slopeRR, y =rrb$slopeRR,
         alternative = c ("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, var.equal = FALSE,
         conf.level = 0.95)
  signal<- slopes_rhrhrvRR_excludinglessthan8datapoints
  signal <- signal [complete.cases(signal),]
  signal$Type<- factor(signal$Type, levels = c('Mindfulness', 'Breathing'))
  signal$Group<- factor(signal$Group, levels = c('Mindful Meditation', 'Slow Breathing', 'Box Breathing', 'Super Oxygenation'))
  
  e <- ggplot(signal, aes(x = Type, y = slopeRR))+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = 'red'), size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  e <- ggplot(signal, aes(x = Group, y = slopeRR))+geom_boxplot(alpha = 0.3)
  t.test(rrb$HoursOfSleep)
  t.test(rrb$SleepEfficiency)
  
  t.test(deltapanasposm$slopePANASPos, y =deltapanasposb$slopePANASPos,
         alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, var.equal = FALSE,
         conf.level = 0.95)
  
  pairwise.wilcox.test(slopes_rhrhrvRR_excludinglessthan8datapoints$slopePANASPos, slopes_rhrhrvRR_excludinglessthan8datapoints$Type, paired = FALSE)
  e <- ggplot(slopes_rhrhrvRR_excludinglessthan8datapoints, aes(x = Group, y = slopeHRV))+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = Group), size = 2, position = position_jitterdodge(),show.legend = T) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  ## add in dayscompleted
  

  for (i in 1:nrow(slopes_rhrhrvRR_excludinglessthan8datapoints)){
    id<- slopes_rhrhrvRR_excludinglessthan8datapoints$subjectID[i]
  dayscompleted <- dayscompleted_cumulative[dayscompleted_cumulative$subjectID==id,3]
  slopes_rhrhrvRR_excludinglessthan8datapoints$dayscompleted[i]<- dayscompleted
  }
  ## calculate mean for slow breathing (fig 4
  write_csv(slopes, 'slopes_rhrhrvRR_excludinglessthan8datapoints.csv')
  
