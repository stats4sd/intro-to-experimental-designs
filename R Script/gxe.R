#1. Install R packages needed

library(ggplot2)
library(emmeans)
library(doBy)
library(lmerTest)
library(multcompView)
library(reshape2)
library(lattice)

#2. Import data
vartrial <- read.csv("C:/Users/Admin/Desktop/moz multi environment trial.csv")

#3. Check and update data
summary(vartrial)
str(vartrial)

vartrial$variety<-factor(vartrial$variety)
vartrial$trial<-factor(vartrial$trial)

#4. Explore data

ggplot(data=vartrial,aes(y=yield,x=varietyname)) +
  geom_point(aes(colour=environment))

ggplot(data=vartrial,aes(y=yield,x=environment,colour=varietyname,group=varietyname)) +
  stat_summary(geom="line")

ggplot(data=vartrial,aes(y=yield,x=varietyname))+
  geom_boxplot(aes(colour=varietyname))+facet_wrap(~environment)

summaryBy(yield~varietyname+environment, data=vartrial, FUN=c(mean,median,sd))

#5. Specify a model for data
gxemodel1<-lmer(yield~varietyname*environment+(1|rep:environment), data=vartrial)

#6. Check the model
plot(gxemodel1)
qqmath(gxemodel1)

#7. Interpret the model
anova(gxemodel1, ddf="Kenward-Roger")
print(VarCorr(gxemodel1), comp=("Variance"))

ranova(gxemodel1)

#8. Present the results from the model
emmip(gxemodel1,~varietyname|environment,CIs = TRUE) +
  theme(axis.text.x = element_text(angle=90))

emmeans(gxemodel1, ~varietyname|environment)

estimatedmeans<-data.frame(emmeans(gxemodel1, ~varietyname|environment))

CLD(emmeans(gxemodel1, ~varietyname*environment))

CLD(emmeans(gxemodel1, ~varietyname|environment))

CLD(emmeans(gxemodel1, ~environment|varietyname))


library(GGEBiplots)
GxEmeans<-stattable(estimatedmeans$varietyname,estimatedmeans$environment,estimatedmeans$emmean)

gge_model1<-GGEModel(GxEmeans)

WhichWon(gge_model1)
MeanStability(gge_model1)
