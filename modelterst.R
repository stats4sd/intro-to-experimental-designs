##read in data

library(openxlsx)
library(ggplot2)
library(emmeans)
library(multcompView)
library(dplyr)

#2. Import data

hhdata <- read.xlsx("C:/Users/sdumb/Dropbox (SSD)/Legume Diversity Project/Sample_data_for_Sam.xlsx",1)
vardata<- read.xlsx("C:/Users/sdumb/Dropbox (SSD)/Legume Diversity Project/Sample_data_for_Sam.xlsx",2)

table(vardata$trad_new)

table(vardata$yield_var)


table(vardata1$yield_dro)

vardata$current_var<-as.numeric(gsub("[^0-9]","",vardata$EC20_amt_var))
vardata$current_var<-ifelse(grepl("bay",vardata$EC20_amt_var),vardata$current_var*0.33,vardata$current_var*100)

vardata$current_var

vardata$last_var<-as.numeric(gsub("[^0-9]","",vardata$EC2009_amt_var))
vardata$last_var<-ifelse(grepl("bay",vardata$EC2009_amt_var),vardata$last_var*0.33,vardata$last_var*100)

vardata1 <- subset(vardata,trad_new!="dk"&yield_var!="na")

vardata2$kebele<-recode(vardata2$kebele,`Agalo`="Agelo",`Gebisa Sori`="Gabisa Sori")

vardata2<-inner_join(vardata1,hhdata,by="hh_index")

ggplot(data=vardata2,aes(y=current_var,x=EC20_timad_var))+
  geom_point()+ylab("Yield this year (kg)")+xlab("Timad planted")

ggplot(data=vardata2,aes(y=current_var,x=EC20_timad_var))+
  geom_point()+ylab("Yield this year (kg)")+xlab("Timad planted")+
  geom_smooth(method="lm",se=FALSE)

ggplot(data=vardata2,aes(y=current_var,x=kebele))+
  geom_violin()+ylab("Yield this year (kg)")+xlab("kebele")+geom_point()

model1<-lm(current_var~EC20_timad_var,data=vardata2)
summary(model1)

model2<-lm(current_var~kebele,data=vardata2)
summary(model2)
anova(model2)

model3<-lm(current_var~kebele+EC20_timad_var,data=vardata2)
summary(model3)

vardata2$yield_var2<-recode(vardata2$yield_var,`very_low`=1,`low`=2,
                            `medium`=3,`high`=4,`very_high`=5)


model4<-lm(yield_var2~kebele,data=vardata2)
summary(model4)

table(vardata2$EC20_yield_var,vardata2$kebele)

table(vardata2$yield_var,vardata2$kebele)

