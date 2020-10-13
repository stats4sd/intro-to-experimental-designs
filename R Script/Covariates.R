#1. Install R packages needed

library(ggplot2)
library(emmeans)
library(doBy)
library(lmerTest)
library(multcompView)
library(lattice)

#2. Import data

fallow <- read.csv("C:/Users/Admin/Desktop/Fallow N2.csv")

#3. Check and update data

summary(fallow)
str(fallow)
fallow$rep<-factor(fallow$rep)
fallow$plot<-factor(fallow$plot)

#4. Explore data

ggplot(data=fallow,aes(y=yield,x=treat,col=rep))+geom_point()
summaryBy(yield~treat, data=fallow, FUN=c(min,max,mean,median,sd))

#5. Specify a model for data

rcbdmodel1<-lmer(yield~treat+(1|rep),data=fallow)

#6. Check the model

plot(rcbdmodel1)
qqmath(rcbdmodel1)

#7. Interpret the model

anova(rcbdmodel1,ddf="Kenward-Roger")
print(VarCorr(rcbdmodel1), comp=("Variance"))
ranova(splitplotmodel2)

#8. Present the results from the model

emmip(rcbdmodel1,~treat,CIs = TRUE)
emmeans(rcbdmodel1, ~treat)
CLD(emmeans(rcbdmodel1, ~treat))
