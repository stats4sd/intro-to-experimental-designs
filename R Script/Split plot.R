
#1. Install R packages needed

library(ggplot2)
library(emmeans)
library(doBy)
library(lmerTest)
library(multcompView)
library(lattice)

#2. Import data

fphosphorus  <- read.csv("C:/Users/Admin/Desktop/FPhosphorus.csv")

#3. Check and update data

summary(fphosphorus)
str(fphosphorus)

fphosphorus$mainplot<-factor(fphosphorus$mainplot)
fphosphorus$subplot<-factor(fphosphorus$subplot)
fphosphorus$block<-factor(fphosphorus$block)

#4. Explore data

ggplot(data=fphosphorus,aes(y=grain,x=fallow))+geom_boxplot(aes(colour=nitrogen))
summaryBy(grain~fallow+nitrogen, data=fphosphorus, FUN=c(mean,sd))

#5. Specify a model for data

splitplotmodel1<-lmer(grain~fallow*nitrogen+(1|block/mainplot), data=fphosphorus)

#6. Check the model

plot(splitplotmodel1)
qqmath(splitplotmodel1)

splitplotmodel2<-lmer(sqrt(grain)~fallow*nitrogen+(1|block/mainplot), data=fphosphorus)

plot(splitplotmodel2)
qqmath(splitplotmodel2)

#7. Interpret the model

anova(splitplotmodel2, ddf="Kenward-Roger")
print(VarCorr(splitplotmodel2), comp=("Variance"))
ranova(splitplotmodel2)


#8. Present the results from the model

emmip(splitplotmodel2,nitrogen~fallow,CIs = TRUE,type="response")
emmeans(splitplotmodel2,~fallow,type="response")
CLD(emmeans(splitplotmodel2,~fallow,type="response"))
