dat<-read.csv("BWPilot_CombinedData_20210803.csv",sep=";",na.strings=".")

dat<-read.csv("BWPilot_CombinedData_20210803_fordryad_addvars_cleaned_noround2_v3.csv",sep=";",na.strings=".")

head(dat)

colnames(dat)

dat<-dat[,1:9]

colnames(dat)<-c("subj","days","exercise","PrePANASPos","PrePANASNeg","PostPANASPos","PostPANASNeg","PreSTAI","PostSTAI")

dat<-subset(dat,days>0 & days<29)

length(unique(dat$subj))

dat$exercise<-factor(dat$exercise)

#Round 1 Exercise: Participant’s assigned intervention. Note the nomenclature is slightly different than the manuscript. Super Oxygenation = Cyclic Hyperventilation with Retention. Slow Breathing = Cyclic Sighing

dat$exercise<-factor(dat$exercise,
                     levels=c("Mindful Meditation",
                              "Box Breathing",
                              "Slow Breathing",
                              "Super Oxygenation"))

contrasts(dat$exercise)


library(lme4)

dat$PANASpos<-dat$PostPANASPos-dat$PrePANASPos

hist(dat$PANASpos)

#dat$days<-scale(dat$days,scale=FALSE)

xtabs(~subj+exercise,dat)
xtabs(~subj+days,dat)

m<-lmer(PANASpos~exercise*days + (1|subj),data = dat)
summary(m)          

dat$cond<-ifelse(dat$exercise=="Mindful Meditation","MM",
       ifelse(dat$exercise=="Box Breathing","BB",
              ifelse(dat$exercise=="Slow Breathing","CS",ifelse(dat$exercise=="Super Oxygenation","CH",NA))))

dat$cond<-factor(dat$cond)

dat$cond<-factor(dat$cond,levels=c("MM","CS","BB","CH"))

contrasts(dat$cond)

contrasts(dat$cond)<-contr.helmert(4)

contrasts(dat$cond)

xtabs(~subj+days,dat)

m_helm<-lmer(PANASpos~cond*days + (1|subj),data = dat)
summary(m_helm)          

lm_helm<-lm(PANASpos~cond*days,data = dat)
summary(lm_helm)          

acf(residuals(lm_helm))

acf(residuals(m))

op<-par(mfrow=c(2,2),pty="s")

conditions<-levels(dat$exercise)

for(j in 1:length(conditions)){
dat_agg<-with(subset(dat,exercise==conditions[j]),tapply(PANASpos,days,mean,na.rm=TRUE))

dat_agg_SD<-with(subset(dat,exercise==conditions[j]),tapply(PANASpos,days,sd,na.rm=TRUE))

lengths<-rep(NA,28)
for(i in 1:28){
lengths[i]<-dim(subset(dat,exercise==conditions[j] & days == i))
}

SE<-dat_agg_SD/sqrt(lengths)

datframe<-data.frame(y=dat_agg,
                     x=1:28,
                     lower=dat_agg-2*SE,
                     upper=dat_agg+2*SE)

plot(datframe$x,datframe$y,ylim=c(-3,max(datframe$y)+2),
     xlab="days",
     ylab="Positive Affect Change",
     main=conditions[j])
with(datframe,
arrows(x0=x,y0=lower,x1=x,y1=upper,angle=90,
       length=0)
)
abline(h=0)
Sys.sleep(2)
}

## design analysis
nMM<-length(unique(subset(dat,cond=="MM")$subj))
nCS<-length(unique(subset(dat,cond=="CS")$subj))

d<-2.751
upper<-4.887
lower<-0.615
SE<-(upper-lower)/4
stddev<-sqrt(SE^2*nMM + SE^2*nCS)

power.t.test(d=d,sd=stddev,type="two.sample",alternative="two.sided",
             power=0.80,
             strict=TRUE)


power.t.test(d=d,sd=stddev,type="two.sample",alternative="two.sided",
            n=31,
             strict=TRUE)


