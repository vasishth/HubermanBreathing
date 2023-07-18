acutestate <- data.frame(matrix(ncol = 6,nrow=120))

acutepanaspos <- data.frame(matrix(ncol = 6,nrow=120))

acutepanasneg <- data.frame(matrix(ncol = 6,nrow=120))

minperday<-  data.frame(matrix(ncol = 6,nrow=120))


filename = BWPilot_CombinedData_20210308
for (i in 1:length(unique(filename$`Subject ID`))){
  print(i)
  id <- unique(filename$`Subject ID`)[i]
  print (id)
  b <- filename$`Subject ID` == as.numeric(id) 
  a <- filename[b,] # data for each individual subject
  c<- subset(a, Round ==1) #round 1
  for (j in 1:nrow(c)){
    if (c$`Pre PANAS Positive Affect`[j]!="." && c$`Post PANAS Positive Affect`[j]!=".")
    c$BW_done[j] = 1
    else 
      c$BW_done[j] = 0
  }

  
  dayscompleted<- sum (c$BW_done)
  prestate<- mean(as.numeric(c$`Pre STAI State Anxiety`), na.rm = TRUE)
  poststate<- mean(as.numeric(c$`Post STAI State Anxiety`), na.rm = TRUE)
  prepanaspos<- mean(as.numeric(c$`Pre PANAS Positive Affect`), na.rm = TRUE)
  postpanaspos<- mean(as.numeric(c$`Post PANAS Positive Affect`), na.rm = TRUE)
  prepanasneg<- mean(as.numeric(c$`Pre PANAS Negative Affect`), na.rm = TRUE)
  postpanasneg<- mean(as.numeric(c$`Post PANAS Negative Affect`), na.rm = TRUE)
  
 
  
  c[c=='.'] <- NA
  # if (nrow(d) >0){
  #   d[d=='.'] <- NA
  # }
  
  if (nrow(c) == 0){
    next()
  }
  

  
  colnames (acutestate) <-c("subjectID", "pre state", "post state", "group", "Type", "dayscompleted")
  colnames (acutepanaspos) <-c("subjectID", "pre panas pos", "post panas post", "group","Type", "dayscompleted")
  colnames (acutepanasneg) <-c("subjectID", "pre panas neg", "post panas neg", "group","Type","dayscompleted")
#  colnames (minperday)<- c("subjectID", "mintimer","mintimestamp", "group","Type","dayscompleted")
  
  acutestate$subjectID[i]<-id
  acutestate$`pre state`[i] <- prestate
  acutestate$`post state`[i] <- poststate
  acutestate$group[i]<- c$`Round 1 Exercise`[1]
  
  acutepanaspos$subjectID[i]<-id
  acutepanaspos$`pre panas pos`[i] <- prepanaspos
  acutepanaspos$`post panas post`[i] <- postpanaspos
  acutepanaspos$group[i]<- c$`Round 1 Exercise`[1]


  
  acutepanasneg$subjectID [i]<-id
  acutepanasneg$`pre panas neg`[i] <- prepanasneg
  acutepanasneg$`post panas neg`[i] <- postpanasneg
  acutepanasneg$group[i]<- c$`Round 1 Exercise`[1]
  
  minperday$subjectID [i]<-id
  minperday$mintimer[i] <- mintimer
  minperday$mintimestamp[i]<- mintimestamp
  minperday$group[i]<- c$`Round 1 Exercise`[1]
  

  if (c$`Round 1 Exercise`[1] == 'Mindful Meditation'){acutestate$Type[i]<- 'Mindfulness'}
  else {acutestate$Type[i]<- 'Breathwork'}
  if (c$`Round 1 Exercise`[1] == 'Mindful Meditation'){acutepanaspos$Type[i]<- 'Mindfulness'}
  else {acutepanaspos$Type[i]<- 'Breathwork'}
  if (c$`Round 1 Exercise`[1] == 'Mindful Meditation'){acutepanasneg$Type[i]<- 'Mindfulness'}
  else {acutepanasneg$Type[i]<- 'Breathwork'}
  if (c$`Round 1 Exercise`[1] == 'Mindful Meditation'){minperday$Type[i]<- 'Mindfulness'}
  else {minperday$Type[i]<- 'Breathwork'}
  
  acutestate$dayscompleted[i] <- dayscompleted
  acutepanaspos$dayscompleted[i] <- dayscompleted
  acutepanasneg$dayscompleted[i] <- dayscompleted
  minperday$dayscompleted[i]<- dayscompleted
  
}

acutepanaspos$round  <- 'Round 1'
acutepanasneg$round <- 'Round 1'
acutestate$round <- 'Round 1'





###
install.packages('tidyverse')
library ('tidyverse')
acutestate$group<- factor(acutestate$group, levels = c("Mindful Meditation", "Slow Breathing", "Box Breathing", "Super Oxygenation"))
acutestate$effect_size <- acutestate$`post state`-acutestate$`pre state`
acutestate <- acutestate[complete.cases(acutestate),]

## subtract the subjects who have dropped #44096 (Slow Breathing) #44162 (Super Oxygenation)

a<- acutestate$subjectID == 44096 |  acutestate$subjectID == 44162
a<- which(a, arr.ind = TRUE, useNames = TRUE)

a<- dayscompleted$subjectID == 44096 |  dayscompleted$subjectID == 44162
a<- which(a, arr.ind = TRUE, useNames = TRUE)
dayscompleted<-dayscompleted[-a,]
dayscompleted <- dayscompleted [complete.cases(dayscompleted),]

dayscompleted$group<- factor(dayscompleted$group, levels = c ("Mindful Meditation", "Slow Breathing", "Box Breathing", "Super Oxygenation"))

e <- ggplot(dayscompleted, aes(x = group, y = `total bw days`))+ylab('Total BW days')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())



acutestate<- acutestate[-a,]


e <- ggplot(acutestate, aes(x = group, y = effect_size))+ylab('State Anxiety')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
kruskal.test(effect_size ~group, data = acutestate)
pairwise.wilcox.test(acutestate$effect_size, acutestate$group, paired = FALSE)

acutestate$Type<- factor(acutestate$Type, levels = c ("Mindfulness", "Breathwork"))

e <- ggplot(acutestate, aes(x = Type, y = effect_size))+ylab('State Anxiety')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
kruskal.test(effect_size ~Type, data = acutestate)
pairwise.wilcox.test(acutestate$effect_size, acutestate$Type, paired = FALSE)

state_mm<-acutestate[ which (acutestate$group == "Mindful Meditation"),]
mean (state_mm$effect_size)
sd (state_mm$effect_size)
effectsize <- mean (state_mm$effect_size)/sd(state_mm$`pre state`)

state_breath<-acutestate[ which (acutestate$Type == "Breathwork"),]
mean (state_breath$effect_size)
sd (state_breath$effect_size)
effectsize <- mean (state_breath$effect_size)/sd(state_breath$`pre state`)

state_sb<-acutestate[ which (acutestate$group == "Slow Breathing"),]
mean (state_sb$effect_size)
sd (state_sb$effect_size)


state_bb<-acutestate[ which (acutestate$group == "Box Breathing"),]
mean (state_bb$effect_size)
sd (state_bb$effect_size)


state_so<-acutestate[ which (acutestate$group == "Super Oxygenation"),]
mean (state_so$effect_size)
sd (state_so$effect_size)

##panas pos

acutepanaspos$group<- factor(acutepanaspos$group, levels = c("Mindful Meditation", "Slow Breathing", "Box Breathing", "Super Oxygenation"))
acutepanaspos$effect_size <- acutepanaspos$`post panas post`-acutepanaspos$`pre panas pos`
acutepanaspos <- acutepanaspos[complete.cases(acutepanaspos),]

a<- acutepanaspos$subjectID == 44096 |  acutepanaspos$subjectID == 44162
a<- which(a, arr.ind = TRUE, useNames = TRUE)
acutepanaspos<- acutepanaspos [-a,]

e <- ggplot(acutepanaspos, aes(x = group, y = effect_size))+ylab('Panas Pos')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
kruskal.test(effect_size ~group, data = acutepanaspos)
pairwise.wilcox.test(acutepanaspos$effect_size, acutepanaspos$group, paired = FALSE)

acutepanaspos$Type <- factor(acutepanaspos$Type, levels =c("Mindfulness", "Breathwork"))
e <- ggplot(acutepanaspos, aes(x = Type, y = effect_size))+ylab('Panas Pos')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
kruskal.test(effect_size ~Type, data = acutepanaspos)
pairwise.wilcox.test(acutepanaspos$effect_size, acutepanaspos$Type, paired = FALSE)


panaspos_mm<-acutepanaspos[ which (acutepanaspos$group == "Mindful Meditation"),]
mean (panaspos_mm$effect_size)
sd(panaspos_mm$effect_size)
effectsize <- mean (panaspos_mm$effect_size)/sd(panaspos_mm$`pre panas pos`)

panaspos_breath<-acutepanaspos[which (acutepanaspos$Type == "Breathwork"),]
mean (panaspos_breath$effect_size)
sd(panaspos_breath$effect_size)
mean (panaspos_breath$effect_size)/sd(panaspos_breath$`pre panas pos`)


panaspos_sb<-acutepanaspos[ which (acutepanaspos$group == "Slow Breathing"),]
mean (panaspos_sb$effect_size)
sd (panaspos_sb$effect_size)


panaspos_bb<-acutepanaspos[ which (acutepanaspos$group == "Box Breathing"),]
mean (panaspos_bb$effect_size)
sd (panaspos_bb$effect_size)



panaspos_so<-acutepanaspos[ which (acutepanaspos$group == "Super Oxygenation"),]
mean (panaspos_so$effect_size)
sd (panaspos_so$effect_size)






##panas neg

acutepanasneg$group<- factor(acutepanasneg$group, levels = c("Mindful Meditation", "Slow Breathing", "Box Breathing", "Super Oxygenation"))
acutepanasneg$effect_size <- acutepanasneg$`post panas neg`- acutepanasneg$`pre panas neg`
acutepanasneg <- acutepanasneg[complete.cases(acutepanasneg),]

a<- acutepanasneg$subjectID == 44096 |  acutepanasneg$subjectID == 44162
a<- which(a, arr.ind = TRUE, useNames = TRUE)

acutepanasneg<- acutepanasneg [-a,]


e <- ggplot(acutepanasneg, aes(x = group, y = effect_size))+ylab('Panas Neg')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
kruskal.test(effect_size ~group, data = acutepanasneg)
pairwise.wilcox.test(acutepanasneg$effect_size, acutepanasneg$group, paired = FALSE)

acutepanasneg$Type<- factor(acutepanasneg$Type, levels = c("Mindfulness", "Breathwork"))
e <- ggplot(acutepanasneg, aes(x = Type, y = effect_size))+ylab('Panas Neg')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
kruskal.test(effect_size ~Type, data = acutepanasneg)
pairwise.wilcox.test(acutepanasneg$effect_size, acutepanasneg$Type, paired = FALSE)


pairwise.wilcox.test(acutepanasneg$effect_size, acutepanasneg$group, paired = FALSE)


##minutes per day practive (min timer)

minperday$group<- factor(minperday$group, levels = c("Mindful Meditation", "Slow Breathing", "Box Breathing", "Super Oxygenation"))
minperday <- minperday[complete.cases(minperday),]

a<- minperday$subjectID == 44096 |  minperday$subjectID == 44162
a<- which(a, arr.ind = TRUE, useNames = TRUE)

minperday<- minperday[-a,]


e <- ggplot(minperday, aes(x = group, y = mintimer))+ylab('Minutes of Intervention Per Day')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())#+ylim(0,25)
kruskal.test(mintimer ~group, data =minperday)
pairwise.wilcox.test(minperday$mintimer, minperday$group, paired = FALSE)
e <- ggplot(minperday, aes(x = group, y = dayscompleted))+ylab('Minutes of Intervention Per Day')+geom_boxplot(alpha = 0.3)+ theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())#+ylim(0,25)


minperday$Type<- factor(minperday$Type, levels = c("Mindfulness", "Breathwork"))
e <- ggplot(minperday, aes(x = Type, y = mintimer))+ylab('Minutes of Intervention Per Day')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())#+ylim (0,50)
kruskal.test(mintimer ~Type, data = minperday)
pairwise.wilcox.test(minperday$mintimer, minperday$Type, paired = FALSE)


##minutes per day practive (timestamp difference)

minperday$group<- factor(minperday$group, levels = c("Mindful Meditation", "Slow Breathing", "Box Breathing", "Super Oxygenation"))
minperday <- minperday[complete.cases(minperday),]

a<- minperday$subjectID == 44096 |  minperday$subjectID == 44162
a<- which(a, arr.ind = TRUE, useNames = TRUE)

minperday<- minperday[-a,]


e <- ggplot(minperday, aes(x = group, y = mintimestamp))+ylab('Minutes of Intervention Per Day(timestamp)')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+ylim(0,50)
kruskal.test(mintimestamp ~group, data =minperday)
pairwise.wilcox.test(minperday$mintimestamp, minperday$group, paired = FALSE)

minperday$Type<- factor(mintimestamp$Type, levels = c("Mindfulness", "Breathwork"))
e <- ggplot(minperday, aes(x = Type, y = mintimestamp))+ylab('Minutes of Intervention Per Day')+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "gray33") ,size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())#+ylim (0,50)
kruskal.test(mintimestamp ~Type, data = minperday)
pairwise.wilcox.test(minperday$mintimestamp, minperday$Type, paired = FALSE)



#effectsizes

panasneg_mm<-acutepanasneg[ which (acutepanasneg$group == "Mindful Meditation"),]
mean (panasneg_mm$effect_size)
sd (panasneg_mm$effect_size)
mean (panasneg_mm$effect_size)/sd(panasneg_mm$`pre panas neg`)


panasneg_breath<-acutepanasneg[ which (acutepanasneg$Type == "Breathwork"),]
mean (panasneg_breath$effect_size)
sd (panasneg_breath$effect_size)
mean (panasneg_breath$effect_size)/sd(panasneg_breath$`pre panas neg`)


panasneg_sb<-acutepanasneg[ which (acutepanasneg$group == "Slow Breathing"),]
mean (panasneg_sb$effect_size)
sd (panasneg_sb$effect_size)


panasneg_bb<-acutepanasneg[ which (acutepanasneg$group == "Box Breathing"),]
mean (panasneg_bb$effect_size)
sd (panasneg_bb$effect_size)


panasneg_so<-acutepanasneg[ which (acutepanasneg$group == "Super Oxygenation"),]
mean (panasneg_so$effect_size)
sd (panasneg_so$effect_size)

#effectsizes time spent

minperday_mm<-minperday[ which (minperday$group == "Mindful Meditation"),]
mean (minperday_mm$mintimer)
sd (minperday_mm$mintimer)

minperday_breath<-minperday[ which (minperday$Type == "Breathwork"),]
mean (minperday_breath$mintimer)
sd (minperday_breath$mintimer)

minperday_sb<-minperday[ which (minperday$group == "Slow Breathing"),]
mean (minperday_sb$mintimer)
sd (minperday_sb$mintimer)


minperday_bb<-minperday[ which (minperday$group == "Box Breathing"),]
mean (minperday_bb$mintimer)
sd (minperday_bb$mintimer)


minperday_so<-minperday[which (minperday$group == "Super Oxygenation"),]
mean (minperday_so$mintimer)
sd (minperday_so$mintimer)

#effect size dayscompleted
dayscompleted_mm<-minperday[ which (minperday$group == "Mindful Meditation"),]
mean (dayscompleted_mm$dayscompleted)
sd (dayscompleted_mm$dayscompleted)

dayscompleted_breath<-minperday[ which (minperday$Type == "Breathwork"),]
mean (dayscompleted_breath$dayscompleted)
sd (dayscompleted_breath$dayscompleted)



#stats

b <- acutestate$group == "Mindful Meditation"
state_medi <- acutestate[b,]
b <- acutestate$group == "Box Breathing"
state_box <- acutestate[b,]
b <- acutestate$group == "Super Oxygenation"
state_SuperOx <- acutestate[b,]
b <- acutestate$group == "Slow Breathing"
state_Slow <- acutestate[b,]
b <- acutestate$Type == "Breathwork"
state_breath <- acutestate[b,]


b <- acutepanaspos$group == "Mindful Meditation"
panaspos_medi <- acutepanaspos[b,]
b <- acutepanaspos$group == "Box Breathing"
panaspos_box <- acutepanaspos[b,]
b <- acutepanaspos$group == "Super Oxygenation"
panaspos_SuperOx <- acutepanaspos[b,]
b <- acutepanaspos$group == "Slow Breathing"
panaspos_Slow <- acutepanaspos[b,]
b <- acutepanaspos$Type == "Breathwork"
panaspos_breath <- acutepanaspos[b,]


b <- acutepanasneg$group == "Mindful Meditation"
panasneg_medi <- acutepanasneg[b,]
b <- acutepanasneg$group == "Box Breathing"
panasneg_box <- acutepanasneg[b,]
b <- acutepanasneg$group == "Super Oxygenation"
panasneg_SuperOx <- acutepanasneg[b,]
b <- acutepanasneg$group == "Slow Breathing"
panasneg_Slow <- acutepanasneg[b,]
b <- acutepanasneg$Type == "Breathwork"
panasneg_breath <- acutepanasneg[b,]



wilcoxpaired_pvalues <- data.frame(matrix(ncol = 3, nrow = 12))
colnames(wilcoxpaired_pvalues)<- c('Metric','Group', 'pvalue')

a<- wilcox.test(state_medi$`post state`,state_medi$`pre state`, paired = TRUE)
wilcoxpaired_pvalues$Metric[1] <- 'State Anxiety'
wilcoxpaired_pvalues$Group[1] <- 'Meditation'
wilcoxpaired_pvalues$pvalue[1] <- a$p.value


a<- wilcox.test(state_box$`post state`, state_box$`pre state`, paired = TRUE)
wilcoxpaired_pvalues$Metric[2] <- 'State Anxiety'
wilcoxpaired_pvalues$Group[2] <- 'Box Breathing'
wilcoxpaired_pvalues$pvalue[2] <- a$p.value


a<- wilcox.test(state_SuperOx$`post state`,state_SuperOx$`pre state`, paired = TRUE)
wilcoxpaired_pvalues$Metric[3] <- 'State Anxiety'
wilcoxpaired_pvalues$Group[3] <- 'Super Oxygenation'
wilcoxpaired_pvalues$pvalue[3] <- a$p.value


a<- wilcox.test(state_Slow$`post state`,state_Slow$`pre state`, paired = TRUE)
wilcoxpaired_pvalues$Metric[4] <- 'State Anxiety'
wilcoxpaired_pvalues$Group[4] <- 'Slow Breathing'
wilcoxpaired_pvalues$pvalue[4] <- a$p.value



## panas pos

a<- wilcox.test(panaspos_medi$`post panas post`,panaspos_medi$`pre panas pos`, paired = TRUE)
wilcoxpaired_pvalues$Metric[5] <- 'Panas Pos'
wilcoxpaired_pvalues$Group[5] <- 'Meditation'
wilcoxpaired_pvalues$pvalue[5] <- a$p.value



a<- wilcox.test(panaspos_box$`post panas post`, panaspos_box$`pre panas pos`, paired = TRUE)
wilcoxpaired_pvalues$Metric[6] <- 'Panas Pos'
wilcoxpaired_pvalues$Group[6] <- 'Box Breathing'
wilcoxpaired_pvalues$pvalue[6] <- a$p.value


a<- wilcox.test(panaspos_SuperOx$`post panas post`,panaspos_SuperOx$`pre panas pos`, paired = TRUE)
wilcoxpaired_pvalues$Metric[7] <- 'Panas Pos'
wilcoxpaired_pvalues$Group[7] <- 'Super Oxygenation'
wilcoxpaired_pvalues$pvalue[7] <- a$p.value


a<- wilcox.test(panaspos_Slow$`post panas post`,panaspos_Slow$`pre panas pos`, paired = TRUE)
wilcoxpaired_pvalues$Metric[8] <- 'Panas Pos'
wilcoxpaired_pvalues$Group[8] <- 'Slow Breathing'
wilcoxpaired_pvalues$pvalue[8] <- a$p.value

## panas neg

a<- wilcox.test(panasneg_medi$`post panas neg`,panasneg_medi$`pre panas neg`, paired = TRUE)
wilcoxpaired_pvalues$Metric[9] <- 'Panas Neg'
wilcoxpaired_pvalues$Group[9] <- 'Meditation'
wilcoxpaired_pvalues$pvalue[9] <- a$p.value



a<- wilcox.test(panasneg_box$`post panas neg`, panasneg_box$`pre panas neg`, paired = TRUE)
wilcoxpaired_pvalues$Metric[10] <- 'Panas Neg'
wilcoxpaired_pvalues$Group[10] <- 'Box Breathing'
wilcoxpaired_pvalues$pvalue[10] <- a$p.value


a<- wilcox.test(panasneg_SuperOx$`post panas neg`,panasneg_SuperOx$`pre panas neg`, paired = TRUE)
wilcoxpaired_pvalues$Metric[11] <- 'Panas Neg'
wilcoxpaired_pvalues$Group[11] <- 'Super Oxygenation'
wilcoxpaired_pvalues$pvalue[11] <- a$p.value


a<- wilcox.test(panasneg_Slow$`post panas neg`,panasneg_Slow$`pre panas neg`, paired = TRUE)
wilcoxpaired_pvalues$Metric[12] <- 'Panas Neg'
wilcoxpaired_pvalues$Group[12] <- 'Slow Breathing'
wilcoxpaired_pvalues$pvalue[12] <- a$p.value


#pre post paired t tests with box plots (try line plot too

state_mm<-acutestate[ which (acutestate$group == "Mindful Meditation"),]
state_breath<-acutestate[ which (acutestate$Type == "Breathwork"),]


library(dplyr)
library(reshape)


dataset<-panasneg_mm



NP<- select(dataset, 'subjectID', `pre panas neg`,`post panas neg`)

NP <- as.data.frame(NP)

NP <- melt(NP, id=c('subjectID'))

#individual subjects as lines

#boxplot
e <- ggplot(NP, aes(x = variable, y = value, label = subjectID))+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = 'grey'), size = 2, position = position_jitterdodge(),show.legend = T) +theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+geom_text(hjust=0, vjust=0)
e

#paired ttest
t.test(state_mm$`pre state`, y =state_mm$`post state`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(state_breath$`pre state`, y =state_breath$`post state`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)


t.test(panasneg_breath$`pre panas neg`,  y =panasneg_breath$`post panas neg`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(panasneg_breath$`pre panas neg`,  y =panasneg_breath$`post panas neg`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)


                     
