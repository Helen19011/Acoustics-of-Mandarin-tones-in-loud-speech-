setwd("D:/materials/工作/学术/刘威童/实验/sounds/Acoustic_data/")
setwd("/Users/mac1/Nutstore Files/第一个研究/sounds/Acoustic_data")
library(emmeans)
library(ggplot2)
library(plyr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(readxl)
library(mgcv)


# intensity data opening --------------------------------------------------

intensity <- read_xlsx("intensitydata_allspeakers.xlsx")
str(intensity)
summary(intensity)
levels(as.factor(intensity$Tone))
intensity$index <- paste(intensity$Character,intensity$Speaker,intensity$Mode,intensity$Tone,sep = "-")


# intensity Tvalue transformation -----------------------------------------

#分组求最大值最小值
intensity <- intensity%>%group_by(Speaker)  %>% mutate(in.max=max(intensity), in.min=min(intensity))

#转换Tvalue
intensity$Tvalue <- 5*(intensity$intensity-intensity$in.min)/(intensity$in.max-intensity$in.min)

intensity_avg <- summarySE(data = intensity,measurevar = "intensity",groupvars = c("Tone","Mode"))


# intensity Tvalue visualization ------------------------------------------
meanplot <- summarySE(data = intensity,measurevar = "Tvalue",groupvars = c("Mode","Tone"),na.rm = T)
ggplot(data=intensity,aes(x=Point,y=Tvalue,group=Mode,color=Mode))+geom_smooth()+facet_grid(Tone~Gender)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")

# intensity Tvalue statistics ---------------------------------------------
intensity$Tone <- as.factor(intensity$Tone)
intensity$Tone <- relevel(intensity$Tone,ref="Tone 4")
intensity$Mode <- as.factor(intensity$Mode)
intensity$Mode <- relevel(intensity$Mode,ref="normal")
m.int <-  lmer(Tvalue ~ Tone * Mode*Gender +(1+Mode+Tone|Vowel)+(1+Tone+Mode|Speaker),
               data=intensity,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.int)
anova(m.int)
emmeans(m.int,pairwise~Mode|Tone,adjust = "tukey")


# intensity change data preparation ---------------------------------------

library(reshape2)
in_change <- dcast(intensity,Character+Tone+Vowel+Speaker+Point+Gender~Mode,value.var = "Tvalue",mean,na.rm = TRUE)
in_change$change <- in_change$loud-in_change$normal


# intensity change visualization ------------------------------------------
meanplot <- summarySE(data = in_change,measurevar = "change",groupvars = c("Point","Tone"),na.rm = T)
ggplot(data = meanplot,aes(x=Point,y=change))+geom_point()+geom_errorbar(aes(ymin=change-se,ymax=change+se),width=0.5)+facet_grid(.~Tone)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")

meanplot <- summarySE(data = in_change,measurevar = "change",groupvars = "Tone",na.rm = T)

# intensity change statistics ---------------------------------------------

in_change$TP <- in_change$Point
t <- poly((unique(in_change$TP)), 2)
in_change[,paste("ot", 1:2, sep="")] <- t[in_change$TP, 1:2]
in_change$Tone <- as.factor(in_change$Tone)
in_change$Tone <- relevel(in_change$Tone,ref="Tone 4")

m.in.change.1 <- lmer(change~(ot1+ot2)*Gender +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(in_change,Tone=="Tone 1"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.in.change.1)

m.in.change.2 <- lmer(change~(ot1+ot2)*Gender +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(in_change,Tone=="Tone 2"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.in.change.2)

m.in.change.3 <- lmer(change~(ot1+ot2)*Gender +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(in_change,Tone=="Tone 3"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.in.change.3)

m.in.change.4 <- lmer(change~(ot1+ot2)*Gender +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(in_change,Tone=="Tone 4"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.in.change.4)


anova(m.in.change.1)


# F0 Tvalue data opening---------------------------------------------------------------

f0 <- read_xlsx("F0data_allspeakers.xlsx")

##data clensing##
str(f0)
summary(f0)
f0$Tone <- revalue(f0$Tone,c(" Tone 1"="Tone 1"," Tone 2"="Tone 2"," Tone 3"="Tone 3"," Tone 4"="Tone 4"))
levels(as.factor(f0$Tone))
f0$index <- paste(f0$Character,f0$Speaker,f0$Mode,f0$Tone,sep = "-")




# F0 removing outliers ----------------------------------------------------


F0range <- f0%>%group_by(Character,Mode,Tone,Vowel,Speaker,index)%>% dplyr::summarize(F0.m=mean(F0), F0.max=max(F0),F0.min=min(F0)) %>% as.data.frame()
F0range$F0range <- F0range$F0.max-F0range$F0.min

##根据F0 range删除Tone 1极端值
possible_out1 <- F0range[which(F0range$F0range>50&F0range$Tone=="Tone 1"),]
ggplot(data=f0[which(f0$Character==possible_out1[5,1]&f0$Speaker==possible_out1[5,5]&f0$Mode==possible_out1[5,2]),],aes(x=Point,y=F0))+geom_smooth()
##possible_out里包含了71个极端值


##根据F0 range删除Tone 2极端值
possible_out2 <- F0range[which(F0range$F0range>=200&F0range$Tone=="Tone 2"),]
##画图看看是不是极端值
ggplot(data=f0[which(f0$Character==possible_out2[5,1]&f0$Speaker==possible_out2[5,5]&f0$Mode==possible_out2[5,2]),],aes(x=Point,y=F0))+geom_smooth()
##Tone 2 没有极端值

##根据F0 range删除Tone 3极端值
possible_out3 <- F0range[which(F0range$F0range>=200&F0range$Tone=="Tone 3"),]
##画图看看是不是极端值
ggplot(data=f0[which(f0$Character==possible_out3[4,1]&f0$Speaker==possible_out3[4,5]&f0$Mode==possible_out3[4,2]),],aes(x=Point,y=F0))+geom_smooth()
##Tone 3 没有极端值

##根据F0 range删除Tone 4极端值
possible_out4 <- F0range[which(F0range$F0range>=250&F0range$Tone=="Tone 4"),]
##画图看看是不是极端值
ggplot(data=f0[which(f0$Character==possible_out4[9,1]&f0$Speaker==possible_out4[9,5]&f0$Mode==possible_out4[9,2]),],aes(x=Point,y=F0))+geom_smooth()
##Speaker018 loud "客"是极端值

possible_out <- rbind(possible_out4,possible_out1)

##去除极端值
for (i in 1:nrow(possible_out)){
  f0 <- f0%>% dplyr::filter(index!=possible_out$index[i])
}

#f0 <- f0%>% dplyr::filter(index!=possible_out$index[1])

nrow(f0)
#从31580删除，还剩29880行



# f0 Tvalue transformation ------------------------------------------------


#分组求最大值最小值
f0 <- f0%>%group_by(Speaker)  %>% mutate(F0.max=max(F0), F0.min=min(F0))

#转换Tvalue
f0$Tvalue <- 5*(f0$F0-f0$F0.min)/(f0$F0.max-f0$F0.min)



# f0 Tvalue visualizaiton  -------------------------------------------
ggplot(data=f0,aes(x=Point,y=Tvalue,group=Mode,color=Mode))+geom_smooth()+facet_grid(Tone~Gender)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")


# f0 Tvalue statistics  ---------------------------------------------------


m.F0 <-  lmer(Tvalue ~ Tone * Mode*Gender +(1+Mode+Tone|Vowel)+(0+Tone+Mode|Speaker),
              data=f0,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
anova(m.F0)
emmeans(m.F0,pairwise~Mode|Tone,adjust = "tukey")




# f0 change data preparation ----------------------------------------------

F0change <- dcast(f0,Character+Tone+Vowel+Speaker+Point+Gender~Mode,value.var = "Tvalue",mean,na.rm = TRUE)
F0change$F0change <- F0change$loud-F0change$normal
F0change <- na.omit(F0change)
##alternative scripts
#F0change <- f0%>%group_by(Character,Vowel,Tone,Point,Speaker)%>% dplyr::summarize( F0.max=max(F0),F0.min=min(F0)) %>% as.data.frame()
#F0change$F0change <- F0change$F0.max-F0change$F0.min



# f0 change visualization -------------------------------------------------
meanplot <- summarySE(data = F0change,measurevar = "F0change",groupvars = c("Point","Tone"))
# plotdata <- aggregate(f0[,8],list(f0$Tone,f0$Vowel,f0$Speaker,f0$Mode),mean)
# names(plotdata) <- c("Tone","Vowel","Speaker","Mode","F0")
ggplot(data = meanplot,aes(x=Point,y=F0change))+geom_point()+geom_errorbar(aes(ymin=F0change-se,ymax=F0change+se),width=0.5)+facet_grid(.~Tone)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")


# f0 change statistics ----------------------------------------------------

cor.subset <- subset(F0change,Tone=="Tone 1")
cor.test(cor.subset$change, cor.subset$loud, method = c("pearson"))

cor.subset <- subset(F0change,Tone=="Tone 2")
cor.test(cor.subset$change, cor.subset$loud, method = c("pearson"))

cor.subset <- subset(F0change,Tone=="Tone 3")
cor.test(cor.subset$change, cor.subset$loud, method = c("pearson"))

cor.subset <- subset(F0change,Tone=="Tone 4")
cor.test(cor.subset$change, cor.subset$loud, method = c("pearson"))

F0change$TP <- F0change$Point
t <- poly((unique(F0change$TP)), 2)
F0change[,paste("ot", 1:2, sep="")] <- t[F0change$TP, 1:2]

# m.f0.change.1 <- lmer(change~(ot1+ot2)*Gender+ot1:loud+ot2:loud +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(F0change,Tone=="Tone 1"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
# anova(m.f0.change.1)
# summary(m.f0.change.1)
# 
# 
# m.f0.change.2 <- lmer(change~(ot1+ot2)*Gender+normal+Point +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(F0change,Tone=="Tone 2"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
# summary(m.f0.change.2)
# 
# m.f0.change.4 <- lmer(change~(ot1+ot2)*Gender +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(F0change,Tone=="Tone 4"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
# summary(m.f0.change.4)






# tonal space growth curve-------------------------------------------------------------


f0$TP <- f0$Point
f0$TP2 <- f0$TP^2
label <- data.frame(unique(f0$index))
for (i in 1:nrow(label)){
  thislabel <- subset(f0,index==label[i,1])
  quadraticModel <- lm(Tvalue ~ TP+TP2, data=thislabel)
  label[i,2] <-  quadraticModel$coefficients[3]
  #label[i,3] <-  summary(quadraticModel)$r.squared
  #label[i,3] <- quadraticModel$coefficients[2]
  #label[i,5] <- summary(quadraticModel)$r.squared
}
for (i in 1:nrow(label)){
  thislabel <- subset(f0,index==label[i,1])
  LinearModel <- lm(Tvalue ~ TP, data=thislabel)
  #label[i,2] <-  quadraticModel$coefficients[3]
  #label[i,3] <-  summary(quadraticModel)$r.squared
  label[i,3] <- LinearModel$coefficients[2]
  #label[i,5] <- summary(quadraticModel)$r.squared
}
names(label)[1] <- "index"
names(label)[2] <- "quadraticTerm"
names(label)[3] <- "LinearTerm"
library(tidyr)

label <- label%>% separate(index,into = c("Character","Speaker","Mode","Tone"),"-")
#先看斜率
meanplot <- summarySE(label,measurevar = "LinearTerm",groupvars = c("Tone","Mode"))
ggplot(meanplot,aes(x=Mode,y=LinearTerm))+geom_point()+geom_errorbar(aes(ymin=LinearTerm-se,ymax=LinearTerm+se))+facet_grid(.~Tone)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")+ylab("Slope")

#再看curve
meanplot <- summarySE(label,measurevar = "quadraticTerm",groupvars = c("Tone","Mode"))
ggplot(meanplot,aes(x=Mode,y=quadraticTerm))+geom_point()+geom_errorbar(aes(ymin=quadraticTerm-se,ymax=quadraticTerm+se))+facet_grid(.~Tone)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")+ylab("Curvature")

#统计
label$Tone <- as.factor(label$Tone)
label$Tone <- relevel(label$Tone,ref = "Tone 1")
label$Mode <- as.factor(label$Mode)
label$Mode <- relevel(label$Mode,ref = "normal")
m.space <- lmer(LinearTerm~Mode*Tone+(1+Mode|Speaker),data = label,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.space)
# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: LinearTerm ~ Mode * Tone + (1 + Mode | Speaker)
#    Data: label
# Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+06))
# 
#      AIC      BIC   logLik deviance df.resid 
#  -5790.9  -5727.2   2907.5  -5814.9     1483 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -8.4342 -0.4819  0.0379  0.5142  5.1585 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev. Corr
#  Speaker  (Intercept) 2.947e-05 0.005428     
#           Modeloud    3.640e-05 0.006033 1.00
#  Residual             1.161e-03 0.034075     
# Number of obs: 1495, groups:  Speaker, 33
# 
# Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)          5.091e-03  2.704e-03  3.656e+02   1.883   0.0605 .  
# Modeloud             8.589e-03  4.004e-03  5.822e+02   2.145   0.0323 *  
# ToneTone 2           3.419e-02  3.505e-03  1.462e+03   9.754  < 2e-16 ***
# ToneTone 3          -2.330e-02  3.526e-03  1.462e+03  -6.608 5.43e-11 ***
# ToneTone 4          -9.297e-02  3.488e-03  1.463e+03 -26.652  < 2e-16 ***
# Modeloud:ToneTone 2  4.434e-02  5.163e-03  1.467e+03   8.589  < 2e-16 ***
# Modeloud:ToneTone 3  2.331e-02  5.189e-03  1.467e+03   4.492 7.61e-06 ***
# Modeloud:ToneTone 4 -6.406e-02  5.177e-03  1.468e+03 -12.374  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


label$Tone <- as.factor(label$Tone)
label$Tone <- relevel(label$Tone,ref = "Tone 3")
label$Mode <- as.factor(label$Mode)
label$Mode <- relevel(label$Mode,ref = "normal")
m.space <- lmer(quadraticTerm~Mode*Tone+(1+Mode|Speaker),data = label,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.space)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: quadraticTerm ~ Mode * Tone + (1 + Mode | Speaker)
#    Data: label
# Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+06))
# 
#      AIC      BIC   logLik deviance df.resid 
# -11861.5 -11797.8   5942.7 -11885.5     1483 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.2709 -0.5345 -0.0249  0.5342  7.5036 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev.  Corr
#  Speaker  (Intercept) 5.591e-07 0.0007477     
#           Modeloud    2.975e-06 0.0017248 0.52
#  Residual             1.968e-05 0.0044367     
# Number of obs: 1495, groups:  Speaker, 33
# 
# Fixed effects:
#                       Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)          9.628e-03  3.449e-04  2.400e+02  27.914  < 2e-16 ***
# Modeloud             2.764e-03  5.419e-04  1.349e+02   5.100 1.13e-06 ***
# ToneTone 1          -1.019e-02  4.592e-04  1.432e+03 -22.202  < 2e-16 ***
# ToneTone 2          -3.791e-03  4.488e-04  1.430e+03  -8.445  < 2e-16 ***
# ToneTone 4          -1.353e-02  4.467e-04  1.433e+03 -30.295  < 2e-16 ***
# Modeloud:ToneTone 1 -4.551e-03  6.764e-04  1.436e+03  -6.728 2.47e-11 ***
# Modeloud:ToneTone 2 -3.321e-03  6.343e-04  1.430e+03  -5.235 1.89e-07 ***
# Modeloud:ToneTone 4 -1.014e-02  6.368e-04  1.434e+03 -15.929  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# ##F0 range
# F0range_clean <- F0range[-which(F0range$F0range>=50&F0range$Tone=="Tone 1"),]
# View(F0range_clean)
# F0range$Tone <- as.factor(F0range$Tone)
# F0range$Tone <- relevel(F0range$Tone,ref = "Tone 1")
# m.t2 <-  lmer(F0range ~ Tone * Mode +(1+Mode|Vowel)+(1+Tone+Mode|Speaker),
#               data=F0range,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
# 
# summary(m.t2)
# anova(m.t2)
# emmeans(m.t2,pairwise~Mode|Tone,adjust = "tukey")
# 
# 
# F0range_clean$Tone <- relevel(F0range_clean$Tone,ref = "Tone 1")
# m.t2 <-  lmer(F0range ~ Tone * Mode +(1+Mode|Vowel)+(1+Tone+Mode|Speaker),
#               data=F0range_clean,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
# library(emmeans)
# emmeans(m.t2,pairwise~Mode|Tone,adjust = "tukey")







# tonal space centroid ----------------------------------------------------
f0centroid<- f0%>%group_by(Point,Speaker)%>% mutate(F0centroid=mean(Tvalue)) %>% as.data.frame()
centroid <- summarySE(data = f0centroid,groupvars = c("Point"),measurevar = "F0centroid")
ggplot(data=centroid,aes(x=Point,y = F0centroid))+geom_line()

T1 <- f0[f0$Tone=="Tone 1",]
T2 <- f0[f0$Tone=="Tone 2",]
T3 <- f0[f0$Tone=="Tone 3",]
T4 <- f0[f0$Tone=="Tone 4",]

ggplot(data=centroid,aes(x=Point,y=F0centroid))+geom_line()+geom_smooth(data=subset(T1,Mode=="normal"),aes(x=Point,y=Tvalue),color="red")+geom_smooth(data=subset(T1,Mode=="loud"),aes(x=Point,y=Tvalue),color="green")

ggplot(data=centroid,aes(x=Point,y=F0centroid))+geom_line()+geom_smooth(data=subset(T2,Mode=="normal"),aes(x=Point,y=Tvalue),color="red")+geom_smooth(data=subset(T2,Mode=="loud"),aes(x=Point,y=Tvalue),color="green")

ggplot(data=centroid,aes(x=Point,y=F0centroid))+geom_line()+geom_smooth(data=subset(T3,Mode=="normal"),aes(x=Point,y=Tvalue),color="red")+geom_smooth(data=subset(T3,Mode=="loud"),aes(x=Point,y=Tvalue),color="green")

ggplot(data=centroid,aes(x=Point,y=F0centroid))+geom_line()+geom_smooth(data=subset(T4,Mode=="normal"),aes(x=Point,y=Tvalue),color="red")+geom_smooth(data=subset(T4,Mode=="loud"),aes(x=Point,y=Tvalue),color="green")

f0centroid$distance <- f0centroid$Tvalue-f0centroid$F0centroid
f0centroid$Tone <- relevel(as.factor(f0centroid$Tone),ref = "Tone 3")
m.centroid <- lmer(distance~Tone*Mode+(1+Tone+Mode|Speaker)+(1+Tone+Mode|Vowel),data=f0centroid)
m.centroid2 <- lmer(distance~Tone*Mode+(1+Tone+Mode|Speaker)+(1+Tone+Mode|Vowel),data=f0centroid)
m.centroid3 <- lmer(distance~Tone*Mode+(1+Tone+Mode|Speaker)+(1+Tone+Mode|Vowel),data=f0centroid)
m.centroid4 <- lmer(distance~Tone*Mode+(1+Tone+Mode|Speaker)+(1+Tone+Mode|Vowel),data=f0centroid)
summary(m.centroid2)



# synchronization of F0 and intensity change  -----------------------------
##F0 and intensity
#f0_intensity <- merge(F0change,in_change,by=c("Character","Speaker","Tone","Gender","Point","Vowel"))
F0change$changelabel <- "F0"
in_change$changelabel <- "intensity"
colnames(F0change)[colnames(F0change) == "F0change"] <- "change"
f0_intensity_new <- rbind(F0change,in_change)

t <- poly((unique(f0_intensity_new$Point)), 2)
f0_intensity_new[,paste("ot", 1:2, sep="")] <- t[f0_intensity_new$Point, 1:2]

m.change.1 <- lmer(change~(ot1+ot2)*Tone*Gender*changelabel +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=f0_intensity_new,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
anova(m.change.1)

m.change.T1 <- lmer(change~(ot1+ot2)*Gender*changelabel +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(f0_intensity_new,Tone=="Tone 1"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.change.T4)
anova(m.m.change.T4anova(m.change.T1)
m.change.T2 <- lmer(change~(ot1+ot2)*Gender*changelabel +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(f0_intensity_new,Tone=="Tone 2"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
anova(m.change.T2)
m.change.T3 <- lmer(change~(ot1+ot2)*Gender*changelabel +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(f0_intensity_new,Tone=="Tone 3"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
anova(m.change.T3)
m.change.T4 <- lmer(change~(ot1+ot2)*Gender*changelabel +(0+ot2|Speaker)+(0+ot1|Speaker)+(0+ot1|Vowel)+(0+ot2|Vowel),data=subset(f0_intensity_new,Tone=="Tone 4"),REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
anova(m.change.T4)



meanplot <- summarySE(data = f0_intensity_new,measurevar = "change",groupvars = c("Point","Tone","changelabel","Gender"),na.rm = T)
ggplot(data = meanplot,aes(x=Point,y=change,group=changelabel,color=changelabel))+geom_point()+geom_errorbar(aes(ymin=change-se,ymax=change+se),width=0.5)+geom_smooth()+facet_grid(.~Tone)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")




# Vowel space -------------------------------------------------------------


##数据预处理
formant <- read_xlsx('formantdata.xlsx')
View(formant)
str(formant)
formant$Tone <- as.factor(formant$Tone)
levels(formant$Tone)
library(plyr)
#改完了也保存好了，下面两行不用运行了
# formant$Tone <- revalue(formant$Tone,c("Tone 1 "="Tone 1","Tone 2 "="Tone 2","Tone 3 "="Tone 3","Tone 4 "="Tone 4"))
# formant$Tone <- revalue(formant$Tone,c("Tone  2"="Tone 2","Tone  3"="Tone 3","Tone 3  "="Tone 3"))
# write.csv(formant,"formantdata_allspeakers.csv",row.names = F)
formant$Mode <- substr(formant$File,1,nchar(formant$File)-3)
formant$Speaker <-paste("speaker",substr(formant$File,nchar(formant$File)-2,nchar(formant$File)),sep = "") 
formant_mid <- subset(formant,Point==2)
formant_mid$index <- paste(formant_mid$Character,formant_mid$Speaker,formant_mid$Mode)


##清洗数据
formant_mid <- subset(formant_mid,Vowel=="u"&F2>=1500)
formant_mid <- formant_mid%>%group_by(Vowel)%>%mutate(grandmeanF1=mean(F1),grandmeanF2=mean(F2),sdF1=sd(F1),sdF2=sd(F2))

formant_mid$F1outlier <- ifelse(formant_mid$F1>=formant_mid$grandmeanF1-2*formant_mid$sdF1&formant_mid$F1<=formant_mid$grandmeanF1+2*formant_mid$sdF1,"no","yes")
formant_mid <- formant_mid[which(formant_mid$F1outlier=="no"),]
formant_mid$F2outlier <- ifelse(formant_mid$F2>=formant_mid$grandmeanF2-2*formant_mid$sdF2&formant_mid$F2<=formant_mid$grandmeanF2+2*formant_mid$sdF2,"no","yes")
formant_mid <- formant_mid[which(formant_mid$F2outlier=="no"),]

nrow(formant_mid)

# possible_outy <- formant_mid[which(formant_mid$F1>=1000&formant_mid$Vowel=="y"),]
# possible_outi <- formant_mid[which(formant_mid$F1>=1000&formant_mid$Vowel=="i"),]
# possible_out <- rbind(possible_outi,possible_outy)
# ##去除极端值
# for (i in 1:nrow(possible_out)){
#   formant_mid <- formant_mid%>% dplyr::filter(index!=possible_out$index[i])
# }



#f0 <- f0%>% dplyr::filter(index!=possible_out$index[1])


rm(formant_mid_outlier)
# #draw vowel ecllips
ggplot(formant_mid, aes(F2, F1, group = Vowel,color=Vowel)) +
  geom_point(alpha = .5) +
#geom_text(data = loud_mean, aes(x = F2_mean, y = F1_mean, label = Vowel), fontface = "bold")+
  stat_ellipse(data = formant_mid, level = 0.50, geom = "polygon", alpha = 0.5, aes(fill = Vowel)) +
  facet_grid(.~Mode)+
  theme_bw() +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
   ylim(1200,200)+xlim(3500,400)+scale_fill_brewer(palette="Set1")+scale_color_brewer(palette="Set1")

# 
# 
# 
# #scale_color_manual(breaks = c("loud","normal"), values = c("orange", "gray40")) 

# formant_mid$Mode <- as.factor(formant_mid$Mode)
# formant_mid$Mode <- relevel(formant_mid$Mode,ref = "normal")
# formant_mid$Vowel <- as.factor(formant_mid$Vowel)
# formant_mid$Vowel <- relevel(formant_mid$Vowel,ref = "y")
# 
# m1 <- lmer(F1 ~ Vowel * Mode +(1|Tone)+(1+Mode|Speaker),
#      data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
# summary(m1)
# emmeans(m1,pairwise~Mode|Vowel,adjust = "tukey")
# anova(m1)
# 
# 
# 
# m2 <- lmer(F2 ~ Vowel * Mode +(1|Tone)+(1|Speaker),
#            data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
# summary(m2)
# emmeans(m2,pairwise~Mode|Vowel,adjust = "tukey")
# 
# anova(m1)
# 
# ##post hoc analysis
# emmeans(m2,pairwise~Mode|Vowel,adjust = "tukey")
# emmeans(m1,pairwise~Vowel|Mode,adjust = "tukey")


formant_mid$speaker_mode <- paste(formant_mid$Speaker,formant_mid$Mode,sep = "-")

formant_mid <- formant_mid%>%group_by(speaker_mode)%>%mutate(meanF1=mean(F1),meanF2=mean(F2))

formant_mid$OD <- sqrt((formant_mid$F1-formant_mid$meanF1)^2+(formant_mid$F2-formant_mid$meanF2)^2)
meanplot <- summarySE(formant_mid,measurevar = "OD",groupvars = c("Vowel","Mode"))
ggplot(meanplot,aes(x=Mode,y=OD))+geom_point()+facet_grid(.~Vowel)+geom_errorbar(aes(ymin=OD-se,ymax=OD+se))
formant_mid$Vowel_num <- ifelse(formant_mid$Vowel=="a",-3,ifelse(formant_mid$Vowel=="o",-2,ifelse(formant_mid$Vowel=="e",-1,ifelse(formant_mid$Vowel=="i",1,ifelse(formant_mid$Vowel=="u",2,3)))))
formant_mid$Tone_sum <- ifelse(formant_mid$Tone=="Tone 1", -2, ifelse(formant_mid$Tone=="Tone 2", -1, ifelse(formant_mid$Tone=="Tone 3",1,2)))
formant_mid$Mode_sum <- ifelse(formant_mid$Mode=="normal",1,-1)
m.OD.sum <- lmer(OD~Vowel_num*Mode_sum+(1+Mode_sum|Speaker)+(1+Mode_sum|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.OD.sum)
m.OD <- lmer(OD~Vowel*Mode+(1+Mode|Speaker)+(1+Mode|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
m.OD.reduce1 <- lmer(OD~Vowel+Mode+(1+Mode|Speaker)+(1+Mode|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
m.OD.reduce2 <- lmer(OD~Vowel+(1+Mode|Speaker)+(1+Mode|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
m.OD.reduce3 <- lmer(OD~Mode+(1+Mode|Speaker)+(1+Mode|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.OD)
anova(m.OD,m.OD.reduce1)
anova(m.OD.reduce1,m.OD.reduce2)
anova(m.OD.reduce1,m.OD.reduce3)
emmeans(m.OD,pairwise~Vowel|Mode,adjust = "tukey")

m.F1.sum <- lmer(F1~Vowel_num*Mode_sum+(1+Mode_sum+Vowel_num|Speaker)+(Vowel_num|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.F1.sum)

m.F1 <- lmer(F1~Vowel*Mode+(1+Mode+Vowel|Speaker)+(Vowel|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
step(m.F1)
# optimal model
# Model found:
# F1 ~ Vowel + Mode + (1 + Mode + Vowel | Speaker) + (Vowel | Tone) + Vowel:Mode
m.F1.reduce1 <- lmer(F1~Vowel+Mode+(1+Mode+Vowel|Speaker)+(Vowel|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
m.F1.reduce2 <- lmer(F1~Vowel+(1+Mode+Vowel|Speaker)+(Vowel|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
m.F1.reduce3 <- lmer(F1~Mode+(1+Mode+Vowel|Speaker)+(Vowel|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
anova(m.F1,m.F1.reduce1)
anova(m.F1.reduce1,m.F1.reduce2)
anova(m.F1.reduce1,m.F1.reduce3)

m.F2.sum <- lmer(F2~Vowel_num*Mode_sum+(1+Vowel_num+Mode_sum|Speaker)+(1|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.F2.sum)
m.F2 <- lmer(F2~Vowel*Mode+(1+Mode|Speaker)+(Mode|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
step(m.F2)
# optimal model
# Model found:
# F2 ~ Vowel + Mode + (1 + Mode + Vowel | Speaker) + (Mode | Tone) + Vowel:Mode
m.F2.reduce1 <- lmer(F2~Vowel+Mode+(1|Speaker)+(Mode|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
m.F2.reduce2 <- lmer(F2~Vowel+(1|Speaker)+(Mode|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
m.F2.reduce3 <- lmer(F2~Mode+(1|Speaker)+(Mode|Tone),data=formant_mid,REML = FALSE,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
anova(m.F2,m.F2.reduce1)
anova(m.F2.reduce1,m.F2.reduce2)
anova(m.F2.reduce1,m.F2.reduce3)

emmeans(m.F2,pairwise~Mode|Vowel,adjust = "tukey")



meanplot <- summarySE(formant_mid,measurevar = "OD",groupvars = c("Vowel","Mode"))
ggplot(meanplot,aes(x=Vowel,y=OD,group=Mode,color=Mode))+geom_point()+geom_errorbar(aes(ymin=OD-se,ymax=OD+se))

formant_mid_OD2vowel_F1 <- dcast(formant_mid[,c(3,6:10)],Tone+Mode+Speaker~Vowel,value.var = "F1",mean,na.rm = TRUE)
formant_mid_OD2vowel_F1 <- formant_mid_OD2vowel_F1[,c(1:3,6,9)]
names(formant_mid_OD2vowel_F1)[4] <- "F1.i"
names(formant_mid_OD2vowel_F1)[5] <- "F1.y"

formant_mid_OD2vowel_F2 <- dcast(formant_mid[,c(4,6:10)],Tone+Mode+Speaker~Vowel,value.var = "F2",mean,na.rm=TRUE)
formant_mid_OD2vowel_F2 <- formant_mid_OD2vowel_F2[,c(1:3,6,9)]
names(formant_mid_OD2vowel_F2)[4] <- "F2.i"
names(formant_mid_OD2vowel_F2)[5] <- "F2.y"


formant_mid_OD2vowel <- merge(formant_mid_OD2vowel_F1,formant_mid_OD2vowel_F2,by=c("Tone","Mode","Speaker"))
formant_mid_OD2vowel$iyOD <- sqrt((formant_mid_OD2vowel$F1.i-formant_mid_OD2vowel$F1.y)^2+(formant_mid_OD2vowel$F2.i-formant_mid_OD2vowel$F2.y)^2)
meanplot <- summarySE(formant_mid_OD2vowel,groupvars = "Mode",measurevar = "iyOD",na.rm = T)
ggplot(meanplot,aes(x=Mode,y=iyOD))+geom_point()

library(dplyr)
tryF1 <- formant_mid %>% group_by(Vowel) %>% summarize(mean=mean(F1),sd=sd(F1)) %>% as.data.frame()

tryF2 <- formant_mid %>% group_by(Vowel) %>% summarize(mean=mean(F2),sd=sd(F2)) %>% as.data.frame()

#%>% filter(count>=3) 
#%>% inner_join(df) 
#%>% select(Genes,Values)
formant_mid <-subset(formant_mid,Vowel=="-") 
for (i in 1:6){
  thisvowel <- subset(formant_mid,Vowel==tryF1[i,1])
  nooutlier <- subset(thisvowel,F1>=tryF1[i,2]-3*tryF1[i,3]&F1<=tryF2[i,2]+3*tryF2[i,3]&F2>=tryF2[i,2]-3*tryF2[i,3]&F2<=tryF2[i,2]+3*tryF2[i,3])
  formant_mid <- rbind(formant_mid,nooutlier)
}


#%>% filter(count>=3) 
#%>% inner_join(df) 
#%>% select(Genes,Values)
formant_mid <-subset(formant_mid,Vowel=="-") 
for (i in 1:6){
  thisvowel <- subset(formant_mid,Vowel==try[i,1])
  nooutlier <- subset(thisvowel,F1>=try[i,2]-3*try[i,3]&F1<=try[i,2]+3*try[i,3])
  formant_mid <- rbind(formant_mid,nooutlier)
}
##画图
library(ggplot2)
# 
# #F1
# meanplot <- summarySE(data = subset(formant_mid,F1<1500),measurevar = "F1",groupvars = c("Mode","Vowel"))
# plotdata <- aggregate(formant_mid[,3],list(formant_mid$Tone,formant_mid$Vowel,formant_mid$Speaker,formant_mid$Mode),mean)
# names(plotdata) <- c("Tone","Vowel","Speaker","Mode","F1")
# View(plotdata)
# 
# #F2
# meanplot <- summarySE(data = subset(formant_mid),measurevar = "F2",groupvars = c("Mode","Vowel"))
# plotdata <- aggregate(formant_mid[,4],list(formant_mid$Tone,formant_mid$Vowel,formant_mid$Speaker,formant_mid$Mode),mean)
# names(plotdata) <- c("Tone","Vowel","Speaker","Mode","F2")
# View(plotdata)
# 
# ggplot(data=meanplot,aes(x=Mode,y=F2))+geom_point()+geom_errorbar(aes(ymin=F2-se,ymax=F2+se),position = position_dodge(0),width=0.5)+geom_violin(data = formant_mid,aes(x=Mode,y=F2,fill=Mode,color=Mode),alpha=0.2)+geom_jitter(data=formant_mid,size=0.1,aes(x=Mode,y=F2), width = 0.08)+facet_grid(.~Vowel)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")
# #+ ylim(1200,200)+xlim(3500,400)
#   #geom_violin(data = plotdata,aes(x=Vowel,y=F1,fill=Mode,color=Mode),alpha=0.5)+
#   #geom_jitter(data=plotdata,size=0.1,aes(x=Vowel,y=F1), width = 0.08)
# 
# meanplot <- summarySE(data = formant_mid,measurevar = "F2",groupvars = c("Mode","Vowel"))
# plotdata <- aggregate(formant_mid[,4],list(formant_mid$Tone,formant_mid$Vowel,formant_mid$Speaker,formant_mid$Mode),mean)
# names(plotdata) <- c("Tone","Vowel","Speaker","Mode","F2")
# View(plotdata)
# 
# ggplot(data=meanplot,aes(x=Mode,y=F2))+geom_point()+geom_errorbar(aes(ymin=F2-se,ymax=F2+se),position = position_dodge(0),width=0.5)+geom_violin(data = plotdata,aes(x=Mode,y=F2,fill=Mode,color=Mode),alpha=0.2)+geom_jitter(data=plotdata,size=0.1,aes(x=Mode,y=F2), width = 0.08)+facet_grid(.~Vowel)
# 
# #看看高元音和非高元音的变化大小
# meanplot <- summarySE(data = formant_mid,measurevar = "F2",groupvars = c("Mode","Vowel_attr"))
# names(plotdata) <- c("Tone","Vowel","Speaker","Mode","F2")
# ggplot(data = meanplot,aes(x=Mode,y=F2,color=Vowel_attr))+geom_point()+geom_errorbar(aes(ymin=F2-se,ymax=F2+se),width=0.5)
# 
# 
# library(dplyr)
# loud <- formant %>% dplyr::filter(Mode=="loud")
# normal <- formant %>% dplyr::filter(Mode=="normal")
# formant_mean <- formant%>% group_by(Vowel,Speaker)%>%mutate(F1_mean=mean(F1),F2_mean=mean(F2))
# loud_mean <- formant_mean %>% dplyr::filter(Mode=="loud")
# normal_mean <- formant_mean %>% dplyr::filter(Mode=="normal")
# 
# ##画个元音图
# meanplot1 <- summarySE(data = formant_mid,measurevar = "F1",groupvars = c("Mode","Vowel"))
# meanplot2 <- summarySE(data = formant_mid,measurevar = "F2",groupvars = c("Mode","Vowel"))
# meanplot <- merge(meanplot1,meanplot2,by=c("Mode","Vowel"))
# ggplot(meanplot, aes(F2, F1, group = Mode,color=Mode,label = Mode))+geom_point()+facet_grid(.~Vowel)+geom_label()+ ylim(1200,200)+xlim(3500,400)
# 
# 

# duration ----------------------------------------------------------------


library(emmeans)
library(ggplot2)
library(plyr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
duration <- read_xlsx("duration.xlsx")
duration$Speaker <- substr(duration$File,nchar(duration$File)-2,nchar(duration$File))
duration$Mode <- substr(duration$File,1,nchar(duration$File)-3)
duration$Tone <- revalue(duration$Tone,c("Tone 3 "="Tone 3"))
levels(as.factor(duration$Tone))
# meanplot <- summarySE(data = duration,measurevar = "Duration",groupvars = c("Mode","Tone"),na.rm=T)
# 
# library(ggplot2)
# point_plot <- summarySE(duration,measurevar = "Duration",groupvars = c("Speaker","Tone","Mode"))
# ggplot(data=meanplot,aes(x=Mode,y=Duration))+geom_bar(stat = "identity",position = position_dodge(),fill="white",color="black")+geom_errorbar(aes(ymin=Duration-se,ymax=Duration+se))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")+facet_grid(.~Tone)+geom_jitter(data = duration,aes(x=Mode,y=Duration),size=0.05)+geom_violin(data = duration,aes(x=Mode,y=Duration),fill="blue",alpha=0.1)
duration$Tone_sum <- ifelse(duration$Tone=="Tone 1", -2, ifelse(duration$Tone=="Tone 2", -1, ifelse(duration$Tone=="Tone 3",1,2)))
duration$Mode_sum <- ifelse(duration$Mode=="normal",1,-1)
duration$Tone_sum <- as.numeric(duration$Tone_sum)
duration$Mode_sum <- as.numeric(duration$Mode_sum)
dur.m <- lmer(Duration~Mode_sum*Tone_sum+(1|Speaker),data = duration,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(dur.m)
dur.m <- lmer(Duration~Mode*Tone+(1+Tone+Mode|Speaker)+(1+Tone+Mode|Vowel),data = duration,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
dur.m2 <- lmer(Duration~Mode+Tone+(1+Tone+Mode|Speaker)+(1+Tone+Mode|Vowel),data = duration)
summary(dur.m)
anova(dur.m)
emmeans(dur.m,pairwise~Mode|Tone,adjust = "tukey")






# supplementary -----------------------------------------------------------

new <- data.frame(x=(1:20))
x <- F0change[F0change$Tone=="Tone 1"&F0change$Gender=="Female",]$Point
y <-F0change[F0change$Tone=="Tone 1"&F0change$Gender=="Female",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
F0est1 <- predict(cubic.fit,new,se=TRUE)
F0est1 <- as.data.frame(F0est1)
## Plot the fit and the Bayesian confidence intervals
F0est1$Point <- c(1:20)
F0est1$Tone <- "Tone 1"
F0est1$Gender<- "Female"
colnames(F0est1)[colnames(F0est1) == "fit"] <- "fit.F0"
colnames(F0est1)[colnames(F0est1) == "se.fit"] <- "se.fit.F0"


x <- F0change[F0change$Tone=="Tone 2"&F0change$Gender=="Female",]$Point
y <-F0change[F0change$Tone=="Tone 2"&F0change$Gender=="Female",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
F0est2 <- predict(cubic.fit,new,se=TRUE)
F0est2 <- as.data.frame(F0est2)
## Plot the fit and the Bayesian confidence intervals
F0est2$Point <- c(1:20)
F0est2$Tone <- "Tone 2"
F0est2$Gender<- "Female"
colnames(F0est2)[colnames(F0est2) == "fit"] <- "fit.F0"
colnames(F0est2)[colnames(F0est2) == "se.fit"] <- "se.fit.F0"


x <- F0change[F0change$Tone=="Tone 3"&F0change$Gender=="Female",]$Point
y <-F0change[F0change$Tone=="Tone 3"&F0change$Gender=="Female",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
F0est3 <- predict(cubic.fit,new,se=TRUE)
F0est3 <- as.data.frame(F0est3)
## Plot the fit and the Bayesian confidence intervals
F0est3$Point <- c(1:20)
F0est3$Tone <- "Tone 3"
F0est3$Gender<- "Female"
colnames(F0est3)[colnames(F0est3) == "fit"] <- "fit.F0"
colnames(F0est3)[colnames(F0est3) == "se.fit"] <- "se.fit.F0"


x <- F0change[F0change$Tone=="Tone 4"&F0change$Gender=="Female",]$Point
y <-F0change[F0change$Tone=="Tone 4"&F0change$Gender=="Female",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
F0est4 <- predict(cubic.fit,new,se=TRUE)
F0est4 <- as.data.frame(F0est4)
## Plot the fit and the Bayesian confidence intervals
F0est4$Point <- c(1:20)
F0est4$Tone <- "Tone 4"
F0est4$Gender<- "Female"
colnames(F0est4)[colnames(F0est4) == "fit"] <- "fit.F0"
colnames(F0est4)[colnames(F0est4) == "se.fit"] <- "se.fit.F0"


new <- data.frame(x=(1:20))
x <- F0change[F0change$Tone=="Tone 1"&F0change$Gender=="Male",]$Point
y <-F0change[F0change$Tone=="Tone 1"&F0change$Gender=="Male",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
F0est5 <- predict(cubic.fit,new,se=TRUE)
F0est5 <- as.data.frame(F0est5)
## Plot the fit and the Bayesian confidence intervals
F0est5$Point <- c(1:20)
F0est5$Tone <- "Tone 1"
F0est5$Gender<- "Male"
colnames(F0est5)[colnames(F0est5) == "fit"] <- "fit.F0"
colnames(F0est5)[colnames(F0est5) == "se.fit"] <- "se.fit.F0"


x <- F0change[F0change$Tone=="Tone 2"&F0change$Gender=="Male",]$Point
y <-F0change[F0change$Tone=="Tone 2"&F0change$Gender=="Male",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
F0est6 <- predict(cubic.fit,new,se=TRUE)
F0est6 <- as.data.frame(F0est6)
## Plot the fit and the Bayesian confidence intervals
F0est6$Point <- c(1:20)
F0est6$Tone <- "Tone 2"
F0est6$Gender<- "Male"
colnames(F0est6)[colnames(F0est6) == "fit"] <- "fit.F0"
colnames(F0est6)[colnames(F0est6) == "se.fit"] <- "se.fit.F0"


x <- F0change[F0change$Tone=="Tone 3"&F0change$Gender=="Male",]$Point
y <-F0change[F0change$Tone=="Tone 3"&F0change$Gender=="Male",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
F0est7 <- predict(cubic.fit,new,se=TRUE)
F0est7 <- as.data.frame(F0est7)
## Plot the fit and the Bayesian confidence intervals
F0est7$Point <- c(1:20)
F0est7$Tone <- "Tone 3"
F0est7$Gender<- "Male"
colnames(F0est7)[colnames(F0est7) == "fit"] <- "fit.F0"
colnames(F0est7)[colnames(F0est7) == "se.fit"] <- "se.fit.F0"


x <- F0change[F0change$Tone=="Tone 4"&F0change$Gender=="Male",]$Point
y <-F0change[F0change$Tone=="Tone 4"&F0change$Gender=="Male",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
F0est8 <- predict(cubic.fit,new,se=TRUE)
F0est8 <- as.data.frame(F0est8)
## Plot the fit and the Bayesian confidence intervals
F0est8$Point <- c(1:20)
F0est8$Tone <- "Tone 4"
F0est8$Gender<- "Male"
colnames(F0est8)[colnames(F0est8) == "fit"] <- "fit.F0"
colnames(F0est8)[colnames(F0est8) == "se.fit"] <- "se.fit.F0"



est_F0 <- rbind(F0est1,F0est2,F0est3,F0est4,F0est5, F0est6,F0est7,F0est8)


new <- data.frame(x=(1:20))
x <- in_change[in_change$Tone=="Tone 1"&in_change$Gender=="Female",]$Point
y <-in_change[in_change$Tone=="Tone 1"&in_change$Gender=="Female",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
intest1 <- predict(cubic.fit,new,se=TRUE)
intest1 <- as.data.frame(intest1)
## Plot the fit and the Bayesian confidence intervals
intest1$Point <- c(1:20)
intest1$Tone <- "Tone 1"
intest1$Gender <- "Female"
colnames(intest1)[colnames(intest1) == "fit"] <- "fit.intensity"
colnames(intest1)[colnames(intest1) == "se.fit"] <- "se.fit.intensity"


x <- in_change[in_change$Tone=="Tone 2"&in_change$Gender=="Female",]$Point
y <-in_change[in_change$Tone=="Tone 2"&in_change$Gender=="Female",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
intest2 <- predict(cubic.fit,new,se=TRUE)
intest2 <- as.data.frame(intest2)
## Plot the fit and the Bayesian confidence intervals
intest2$Point <- c(1:20)
intest2$Tone <- "Tone 2"
intest2$Gender <- "Female"
colnames(intest2)[colnames(intest2) == "fit"] <- "fit.intensity"
colnames(intest2)[colnames(intest2) == "se.fit"] <- "se.fit.intensity"


x <- in_change[in_change$Tone=="Tone 3"&in_change$Gender=="Female",]$Point
y <-in_change[in_change$Tone=="Tone 3"&in_change$Gender=="Female",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
intest3 <- predict(cubic.fit,new,se=TRUE)
intest3 <- as.data.frame(intest3)
## Plot the fit and the Bayesian confidence intervals
intest3$Point <- c(1:20)
intest3$Tone <- "Tone 3"
intest3$Gender <- "Female"
colnames(intest3)[colnames(intest3) == "fit"] <- "fit.intensity"
colnames(intest3)[colnames(intest3) == "se.fit"] <- "se.fit.intensity"


x <- in_change[in_change$Tone=="Tone 4"&in_change$Gender=="Female",]$Point
y <-in_change[in_change$Tone=="Tone 4"&in_change$Gender=="Female",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
intest4 <- predict(cubic.fit,new,se=TRUE)
intest4 <- as.data.frame(intest4)
## Plot the fit and the Bayesian confidence intervals
intest4$Point <- c(1:20)
intest4$Tone <- "Tone 4"
intest4$Gender <- "Female"
colnames(intest4)[colnames(intest4) == "fit"] <- "fit.intensity"
colnames(intest4)[colnames(intest4) == "se.fit"] <- "se.fit.intensity"

new <- data.frame(x=(1:20))
x <- in_change[in_change$Tone=="Tone 1"&in_change$Gender=="Male",]$Point
y <-in_change[in_change$Tone=="Tone 1"&in_change$Gender=="Male",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
intest5 <- predict(cubic.fit,new,se=TRUE)
intest5 <- as.data.frame(intest5)
## Plot the fit and the Bayesian confidence intervals
intest5$Point <- c(1:20)
intest5$Tone <- "Tone 1"
intest5$Gender <- "Male"
colnames(intest5)[colnames(intest5) == "fit"] <- "fit.intensity"
colnames(intest5)[colnames(intest5) == "se.fit"] <- "se.fit.intensity"


x <- in_change[in_change$Tone=="Tone 2"&in_change$Gender=="Male",]$Point
y <-in_change[in_change$Tone=="Tone 2"&in_change$Gender=="Male",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
intest6 <- predict(cubic.fit,new,se=TRUE)
intest6 <- as.data.frame(intest6)
## Plot the fit and the Bayesian confidence intervals
intest6$Point <- c(1:20)
intest6$Tone <- "Tone 2"
intest6$Gender <- "Male"
colnames(intest6)[colnames(intest6) == "fit"] <- "fit.intensity"
colnames(intest6)[colnames(intest6) == "se.fit"] <- "se.fit.intensity"


x <- in_change[in_change$Tone=="Tone 3"&in_change$Gender=="Male",]$Point
y <-in_change[in_change$Tone=="Tone 3"&in_change$Gender=="Male",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
intest7 <- predict(cubic.fit,new,se=TRUE)
intest7 <- as.data.frame(intest7)
## Plot the fit and the Bayesian confidence intervals
intest7$Point <- c(1:20)
intest7$Tone <- "Tone 3"
intest7$Gender <- "Male"
colnames(intest7)[colnames(intest7) == "fit"] <- "fit.intensity"
colnames(intest7)[colnames(intest7) == "se.fit"] <- "se.fit.intensity"


x <- in_change[in_change$Tone=="Tone 4"&in_change$Gender=="Male",]$Point
y <-in_change[in_change$Tone=="Tone 4"&in_change$Gender=="Male",]$change
cubic.fit <- ssanova(y~x)
## Obtain estimates and standard errors on a grid
intest8 <- predict(cubic.fit,new,se=TRUE)
intest8 <- as.data.frame(intest8)
## Plot the fit and the Bayesian confidence intervals
intest8$Point <- c(1:20)
intest8$Tone <- "Tone 4"
intest8$Gender <- "Male"
colnames(intest8)[colnames(intest8) == "fit"] <- "fit.intensity"
colnames(intest8)[colnames(intest8) == "se.fit"] <- "se.fit.intensity"

est_int <- rbind(intest1,intest2,intest3,intest4,intest5, intest6, intest7,intest8)

est_F0_int <- inner_join(est_F0, est_int, by = c("Point", "Tone","Gender"))

#ggplot(data = est_F0_int, aes(x=Point,y = fit.F0+1.96*se.fit.F0), color = "blue")+geom_line(aes(y = fit.F0-1.96*se.fit.F0), color = "blue")+geom_line(aes(y = fit.intensity+1.96*se.fit.intensity), color = "red")+geom_line(aes(y = fit.intensity-1.96*se.fit.intensity), color = "red")+geom_line(aes(y = fit.F0+1.96*se.fit.F0), color = "blue")+facet_grid(.~Tone)+theme(panel.background = element_blank())+ylim()
ggplot(data = est_F0_int, aes(x=Point,y = fit.F0), color = "blue")+geom_line(aes(y = fit.F0), color = "blue")+geom_line(aes(y = fit.intensity), color = "green")+facet_grid(Gender~Tone)+theme(panel.background = element_blank())+ylab("Predicted T-value")


sink("model_results_Table1.txt")
summary(m.int)
sink()                                     
sink("model_results_Table2.txt")
summary(m.F0)
sink()
sink("model_results_Table3(1).txt")
summary(m.change.T1)
sink() 
sink("model_results_Table3(2).txt")
summary(m.change.T2)
sink() 
sink("model_results_Table3(3).txt")
summary(m.change.T3)
sink() 
sink("model_results_Table3(4).txt")
summary(m.change.T4)
sink() 

sink("model_results_Table4(1).txt")
summary(m.change.T1)
sink() 
sink("model_results_Table4(2).txt")
summary(m.change.T2)
sink() 
sink("model_results_Table4(3).txt")
summary(m.change.T3)
sink() 
sink("model_results_Table4(4).txt")
summary(m.change.T4)
sink() 
