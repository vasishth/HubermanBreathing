# Promis T score analysis
install.packages('tidyverse')
library(tidyverse)

promist <- data.frame(matrix(ncol=5, nrow = 120 ))
promist2 <- data.frame(matrix(ncol =4, nrow = 120 ))

#colnames(promist) <- c("subjectID", "Pre Promis", "Post Promis", "Group", "prepromis10", "prepromis119", "prepromis18", "prepromis25", "prepromis27", "prepromis30", "prepromis6", "prepromis7")

colnames(promist) <- c("subjectID", "Pre Promis", "Post Promis", "Group", "Type")

#colnames(promist2) <- c("subjectID", "Pre Promis", "Post Promis", "Group")

#filename = BWPilot_CombinedData_20210308#
filename = BWPilot_CombinedData_20210803


for (i in 1:length(unique(filename$`Subject ID`))){
  print(i)
  id <- unique(filename$`Subject ID`)[i]
  print (id)
  b <- filename$`Subject ID` == as.numeric(id) 
  a <- filename[b,] # data for each individuaal subject
  #ROUND 1
  #ind<- a$`RSP Day` == 1 & a$Round ==1
  #preind <- which(ind, arr.ind = TRUE, useNames = TRUE)
  #if (length(preind) != 0){
  #round1pre <- a[1:preind,]}
  
  round1pre<- subset(a, `Days from  Round1 Day1`<1)
  
  #ind<- a$`RSP Day` ==28 & a$Round == 1
  ind<- a$`RSP Day` ==1 & a$Round == 2
  
  postind <- which(ind, arr.ind = TRUE, useNames = TRUE)
  if (length(postind)==0)next()
  round1post <- a[(postind-5):postind,]
  #round1post<- subset(a, `Days from  Round1 Day1`> 1 &`Days from  Round1 Day1`< 31 )
  prepromisind <-round1pre$`Sleep T Score` != "."
  prepromis <- as.numeric(round1pre$`Sleep T Score`[prepromisind])
  
  # prepromis10 <- as.numeric(round1pre$sleep10[prepromisind])
  # prepromis119 <- as.numeric(round1pre$sleep119[prepromisind])
  # prepromis18 <- as.numeric(round1pre$sleep18[prepromisind])
  # prepromis25 <- as.numeric(round1pre$sleep25[prepromisind])
  # prepromis27 <- as.numeric(round1pre$sleep27[prepromisind])
  # prepromis30 <- as.numeric(round1pre$sleep30[prepromisind])
  # prepromis6 <- as.numeric(round1pre$sleep6[prepromisind])
  # prepromis7 <- as.numeric(round1pre$sleep7[prepromisind])
  
  
  postpromisind <-round1post$`Sleep T Score` != "."
  postpromis <- as.numeric(round1post$`Sleep T Score`[postpromisind])
  # 
  # postpromis10 <- as.numeric(round1post$sleep10[postpromisind])
  # postpromis119 <- as.numeric(round1post$sleep119[postpromisind])
  # postpromis18 <- as.numeric(round1post$sleep18[postpromisind])
  # postpromis25 <- as.numeric(round1post$sleep25[postpromisind])
  # postpromis27 <- as.numeric(round1post$sleep27[postpromisind])
  # postpromis30 <- as.numeric(round1post$sleep30[postpromisind])
  # postpromis6 <- as.numeric(round1post$sleep6[postpromisind])
  # postpromis7 <- as.numeric(round1post$sleep7[postpromisind])
  # 

  
  promist$subjectID [i] <- id
  
   if (length(prepromis)>0) {
     promist$`Pre Promis` [i] <- prepromis[length(prepromis)]
  #   promist$prepromis10 [i]<- prepromis10[length(prepromis10)]
  #   promist$prepromis119 [i]<- prepromis119[length(prepromis119)]
  #   promist$prepromis18 [i]<- prepromis18[length(prepromis18)]
  #   promist$prepromis25 [i]<- prepromis25[length(prepromis25)]
  #   promist$prepromis27 [i]<- prepromis27[length(prepromis27)]
  #   promist$prepromis30 [i]<- prepromis30[length(prepromis30)]
  #   promist$prepromis6 [i]<- prepromis6[length(prepromis6)]
  #   promist$prepromis7 [i]<- prepromis7[length(prepromis7)]
  #   
   }
  
  if (length(postpromis)>0) {
    promist$`Post Promis` [i] <- postpromis[length(postpromis)]
    # promist$postpromis10 [i]<- postpromis10[length(postpromis10)]
    # promist$postpromis119 [i]<- postpromis119[length(postpromis119)]
    # promist$postpromis18 [i]<- postpromis18[length(postpromis18)]
    # promist$postpromis25 [i]<- postpromis25[length(postpromis25)]
    # promist$postpromis27 [i]<- postpromis27[length(postpromis27)]
    # promist$postpromis30 [i]<- postpromis30[length(postpromis30)]
    # promist$postpromis6 [i]<- postpromis6[length(postpromis6)]
    # promist$postpromis7 [i]<- postpromis7[length(postpromis7)]
  }
  promist$Group [i] <-  a$`Round 1 Exercise`[1]
  if (promist$Group[i] == 'Mindful Meditation'){ promist$Type[i]<- 'Mindfulness'}
  else promist$Type[i]<- 'Breathwork'

}
effect_promist <- as.numeric(promist$`Post Promis`) - as.numeric(promist$`Pre Promis`)

# effect_promis10 <- as.numeric(promist$postpromis10) - as.numeric(promist$prepromis10)
# effect_promis119 <- as.numeric(promist$postpromis119) - as.numeric(promist$prepromis119)
# effect_promis18 <- as.numeric(promist$postpromis18) - as.numeric(promist$prepromis18)
# effect_promis25 <- as.numeric(promist$postpromis25) - as.numeric(promist$prepromis25)
# effect_promis27 <- as.numeric(promist$postpromis27) - as.numeric(promist$prepromis27)
# effect_promis30 <- as.numeric(promist$postpromis30) - as.numeric(promist$prepromis30)
# effect_promis6 <- as.numeric(promist$postpromis6) - as.numeric(promist$prepromis6)
# effect_promis7 <- as.numeric(promist$postpromis7) - as.numeric(promist$prepromis7)



promist['delta promis t']<- effect_promist

# promist['delta promis10']<- effect_promis10
# promist['delta promis119']<- effect_promis119
# promist['delta promis18']<- effect_promis18
# promist['delta promis25']<- effect_promis25
# promist['delta promis27']<- effect_promis27
# promist['delta promis30']<- effect_promis30
# promist['delta promis6']<- effect_promis6
# promist['delta promis7']<- effect_promis7
# 
# 



b <- promist$Group == "Mindful Meditation"
promist_medi <- promist[b,]

b <- promist$Group == "Box Breathing"
promist_box <- promist[b,]

b <- promist$Group == "Super Oxygenation"
promist_superox <- promist[b,]

b <- promist$Group == "Slow Breathing"
promist_slow <- promist[b,]


effect_superox <- as.numeric(promist_superOx$`Post Promis`)- as.numeric(promist_SuperOx$`Pre Promis`)
effect_medi <- as.numeric(promist_medi$`Post Promis`)- as.numeric(promist_medi$`Pre Promis`)
effect_box <- as.numeric(promist_box$`Post Promis`)- as.numeric(promist_box$`Pre Promis`)
effect_slow<- as.numeric(promist_slow$`Post Promis`) - as.numeric(promist_slow$`Pre Promis`)




wilcox.test(promist_superox$`Post Promis`,promist_superox$`Pre Promis`, paired = TRUE)
wilcox.test(promist_slow$`Post Promis`,promist_slow$`Pre Promis`, paired = TRUE)
wilcox.test(promist_box$`Post Promis`,promist_box$`Pre Promis`, paired = TRUE)
wilcox.test(promist_medi$`Post Promis`,promist_medi$`Pre Promis`, paired = TRUE)

wilcox.test(promist_medi$postpromis27, promist_medi$prepromis27, paired = TRUE)


pairwise.wilcox.test(promist$`delta promis t`, promist$Group,
                     p.adjust.method = "BH")
pairwise.wilcox.test(promist$`delta promis t`, promist$Type,
                     p.adjust.method = "BH")

promist$Group<- factor(promist$Group, levels = c('Mindful Meditation', 'Slow Breathing', 'Box Breathing', 'Super Oxygenation'))
promist<-promist[complete.cases(promist),] 

e <- ggplot(promist, aes(x = Group, y = `delta promis10`))+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = 'red'), size = 3, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ylim(-4,4)




pairwise.wilcox.test(promiswithcumdays$deltaPromisT, promiswithcumdays$Group,paired = F,
                     p.adjust.method = "BH")





write.csv(promist, 'prepostpromisr1.csv', row.names = FALSE)

plot.new()
for (i in 1:nrow(promist_slow)){
  if (is.na(promist_slow$`Pre Promis`[i])) next
  #plot(c(promist_slow$`Pre Promis`[i],promist_slow$`Post Promis`[i]), type= 'o',cex=1.5, ylab = 'Promis Sleep (t-score)', xlab = 'Slow Breathing', xlim= c(0.5,2.5),ylim = c(35,75),  xaxt='n',frame.plot = FALSE, axes = FALSE)
  plot(c(promist_slow$`Pre Promis`[i],promist_slow$`Post Promis`[i]), type= 'o',cex=1.5, ylab = 'Promis Sleep (t-score)', xlab = 'Slow Breathing', xlim= c(0.5,2.5),ylim = c(35,80), frame.plot = FALSE,  xaxt='n',  axes = FALSE)
  
  par(new=TRUE)
  
  }

axis(1, at=c(1,2), labels=c('Before', 'After'))

promist<- promist[complete.cases(promist),]


promist$Group<- factor(promist$Group, levels = c('Mindful Meditation', 'Slow Breathing', 'Box Breathing', 'Super Oxygenation'))


#STATS


#pairwise plot for slow breathing


plot.new()
for (i in 1:nrow(promist_slow)){
  if (is.na(promis_slow$`Pre Trait`[i])|is.na(promis_slow$`Post Trait`[i])) next
  par(new=T)
  plot(c(trait_Slow$`Pre Trait`[i],trait_Slow$`Post Trait`[i]), type= 'o', ylab = 'Trait Anxiety', xlab = 'Slow Breathing', xlim= c(0,3), ylim = c(20,70),xaxt='n')
}

axis(1, at=c(1,2), labels=c('Before', 'After'))


plot.new()
for (i in 1:nrow(promist_slow)){
  if (is.na(promist_slow$prepromis27 [i])|is.na(promist_slow$postpromis27[i])) next
  par(new=T)
  plot(c(promist_slow$prepromis27[i],promist_slow$postpromis27[i]), type= 'o', ylab = 'Sleep 27', xlab = 'Slow Breathing', xlim=c(0,3),ylim=c(0,6),xaxt='n')
}

axis(1, at=c(1,2), labels=c('Before', 'After'))


##box plot for round1 and round 2
promist$round <- 'Round 1'


## promis by type
promist<- prepostpromisr1
promisbreath <- subset(promist, Type =='Breathwork')
promismed<- subset(promist, Type =='Mindfulness')
t.test(promismed$`delta promis t`, y =promisbreath$`delta promis t`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)


t.test(promismed$`Pre Promis`, y =promismed$`Post Promis`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(promisbreath$`Pre Promis`, y =promisbreath$`Post Promis`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)
kruskal.test(`delta promis t`~Type, data = promist)

#ttest by group
promist<- promist[complete.cases(promist),]
for (i in 1:nrow(promist)){
if (promist$Group[i] == 'Mindful Meditation'){promist$Type[i]<- 'Mindfulness'}
else promist$Type[i]<- 'Breathwork'
}

promisbreath <- subset(promist, Type =='Breathwork')
promismed<- subset(promist, Type =='Mindfulness')
promist_slow<- subset(promist, Group =='Slow Breathing')


t.test(promismed$`delta promis t`, y =promisbreath$`delta promis t`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)


t.test(promist_slow$`Pre Promis`, y =promist_slow$`Post Promis`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)


t.test(promismed$`Pre Promis`, y =promismed$`Post Promis`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(promisbreath$`Pre Promis`, y =promisbreath$`Post Promis`,
       alternative = c("greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

promist$Type<- factor(promist$Type, levels = c("Mindfulness", "Breathwork"))
e <- ggplot(promist, aes(x = Type, y = promist$`delta promis t`))+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33"), size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

promisbreath <- subset(promist, Group =='Box Breathing')
promismed<- subset(promist, Group =='Super Oxygenation')

mean(promisbreath$`delta promis t`)/sd(promisbreath$`Pre Promis`)
mean(promismed$`delta promis t`)/sd(promismed$`Pre Promis`)

for (i in 1:nrow(promiswithcumdays)){
if (promiswithcumdays$Group[i] == 'Mindful Meditation'){ promiswithcumdays$Type[i]<- 'Mindfulness'}
else promiswithcumdays$Type[i]<- 'Breathwork'
}


promist$Type<- factor(promist$Type, levels = c("Mindfulness", "Breathwork"))
e <- ggplot(promist, aes(x = Type, y =`delta promis t`))+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33"), size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

t.test(promisbreath$`delta promis t`)

