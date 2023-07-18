#Trait anxiety score analysis
library('ggpubr')
trait <- data.frame(matrix(ncol =5, nrow = 120 ))

#colnames(trait) <- c("subjectID", "Pre Trait", "Post Trait", "Group")

colnames(trait) <- c("subjectID", "Pre Trait", "Post Trait", "Group", "Type")

filename = BWPilot_CombinedData_20210308

for (i in 1:length(unique(filename$`Subject ID`))){
  print(i)
  id <- unique(filename$`Subject ID`)[i]
  print (id)
  b <- filename$`Subject ID` == as.numeric(id) 
  a <- filename[b,] # data for each individuaal subject

  
  round1pre<- subset(a, `Days from  Round1 Day1`<1)
  
  #ind<- a$`RSP Day` ==28 & a$Round == 1
  ind<- a$`RSP Day` ==1 & a$Round == 2
  
  
  postind <- which(ind, arr.ind = TRUE, useNames = TRUE)
  if (length(postind)==0)next()
  round1post <- a[(postind-5):postind,]
  #round1post<- subset(a, `Days from  Round1 Day1`> 1 &`Days from  Round1 Day1`< 31 )
  prepromisind <-round1pre$`Full STAI Trait Anxiety` != "."
  prepromis <- as.numeric(round1pre$`Full STAI Trait Anxiety`[prepromisind])
  postpromisind <-round1post$`Full STAI Trait Anxiety` != "."
  postpromis <- as.numeric(round1post$`Full STAI Trait Anxiety`[postpromisind])
  trait$subjectID [i] <- id
  if (length(prepromis)>0) {
    trait$`Pre Trait` [i] <- prepromis[length(prepromis)]
    
  }
  
  if (length(postpromis)>0) {
    trait$`Post Trait` [i] <- postpromis[length(postpromis)]
    
  }
  trait$Group [i] <-  a$`Round 1 Exercise`[1]
  if (trait$Group[i] == 'Mindful Meditation'){trait$Type[i]<- 'Mindfulness'}
    else {trait$Type[i]<- 'Breathwork'}
  
}
effect_trait <- as.numeric(trait$`Post Trait`) - as.numeric(trait$`Pre Trait`)

trait['delta trait']<- effect_trait


b <- trait$Group == "Mindful Meditation"
trait_medi <- trait[b,]

b <- trait$Group == "Box Breathing"
trait_box <- trait[b,]

b <- trait$Group == "Super Oxygenation"
trait_superox <- trait[b,]

b <- trait$Group == "Slow Breathing"
trait_slow <- trait[b,]
####

effect_superox <- as.numeric(trait_superOx$`Post Promis`)- as.numeric(trait_SuperOx$`Pre Promis`)
effect_medi <- as.numeric(trait_medi$`Post Promis`)- as.numeric(trait_medi$`Pre Promis`)
effect_box <- as.numeric(trait_box$`Post Promis`)- as.numeric(trait_box$`Pre Promis`)
effect_slow<- as.numeric(trait_slow$`Post Promis`) - as.numeric(trait_slow$`Pre Promis`)


wilcox.test(trait_superox$`Post Trait`,trait_superox$`Pre Trait`, paired = TRUE)
wilcox.test(trait_slow$`Post Trait`,trait_slow$`Pre Trait`, paired = TRUE)
wilcox.test(trait_box$`Post Trait`,trait_box$`Pre Trait`, paired = TRUE)
wilcox.test(trait_medi$`Post Trait`,trait_medi$`Pre Trait`, paired = TRUE)





#STATS

kruskal.test(`delta trait`~Group, data=trait)
wilcox.test(trait_Slow$`Post Trait`,trait_Slow$`Pre Trait`, paired = TRUE) #WILCOX IS 2 SAMPLE ONLY, KRUSKAL WALLIS MORE) , ~


##box plot for round and round 2


#round 1
trait <- trait [complete.cases(trait),]
trait$Group<- factor(trait$Group, levels = c('Mindful Meditation', 'Slow Breathing', 'Box Breathing', 'Super Oxygenation'))
e <- ggplot(trait, aes(x = Group, y = `delta trait`))+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "grey20"), size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

## by type t test

traitmed<- subset(trait, Type =='Mindfulness')
traitbreath <- subset(trait, Type == 'Breathwork')
t.test(traitmed$`delta trait`, y =traitbreath$`delta trait`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)


t.test(traitmed$`Pre Trait`, y =traitmed$`Post Trait`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(traitbreath$`Pre Trait`, y =traitbreath$`Post Trait`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

traitmed<- traitmed[complete.cases(traitmed),]
mean (traitmed$`delta trait`)/sd(traitmed$`Pre Trait`)
traitbreath<- traitbreath[complete.cases(traitbreath),]
mean (traitbreath$`delta trait`)/sd(traitbreath$`Pre Trait`)


#wilcoxontest

wilcox.test(traitbreath$`Post Trait`,traitbreath$`Pre Trait`, paired = TRUE)
wilcox.test(traitbreath$`delta trait`,traitmed$`delta trait`, paired = FALSE)



plot.new() ## this plot doesnt look good
for (i in 1:nrow(traitbreath)){
  if (is.na(traitbreath$`Pre Trait`[i])|is.na(traitbreath$`Post Trait`[i])) next
  par(new=T)
  plot(c(traitbreath$`Pre Trait`[i],traitbreath$`Post Trait` [i]), type= 'o', ylab = 'Trait Anxiety', xlab = 'All Breathwork', xlim=c(0,3),ylim=c(20,80), xaxt='n')
}

axis(1, at=c(1,2), labels=c('Before', 'After'))

trait$Type<- factor(trait$Type, levels = c('Mindfulness', 'Breathwork'))
trait<- trait[complete.cases(trait),]
e <- ggplot(trait, aes(x = Type, y = `delta trait`))+geom_boxplot(alpha = 0.3)+geom_point (aes(colour = "grey20"), size = 2, position = position_jitterdodge(),show.legend = F) + theme(axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = .5, face = "plain"),  axis.text.x = element_text(color = "grey20", size =20, angle = 0, hjust = .5, vjust = .5, face = "plain"),axis.title = element_blank(), axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

kruskal.test(`delta trait`~Type, data = trait)


#ttest by group

traitbreath <- subset(trait, Group =='Box Breathing')
traitmed<- subset(trait, Group =='Super Oxygenation')


t.test(traitmed$`delta trait`, y =traitbreath$`delta trait`,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)


