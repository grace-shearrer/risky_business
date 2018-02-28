library(reshape)
library(nlme)
library(ggplot2)
#palatability ratings
all_pal<-read.table("~/Google Drive/choco_ana/palatability.csv", sep=",", header=T)
names(all_pal)
water<-c("subject","W1VASwater1")
h2o<-all_pal[water]
head(h2o)
risk<-read.table("~/Google Drive/choco_ana/check_data.csv", sep=",", header=T)
head(risk)
data<-merge(risk, h2o, by="subject")
head(data)
apply(data[11:14], 2, summary)
#checking normality
data$sqA1<-sqrt(21-data$W1VASmA1)
data$sqB1<-sqrt(21-data$W1VASmB1)
data$sqC1<-sqrt(21-data$W1VASmC1)
data$sqD1<-sqrt(21-data$W1VASmD1)#use the normal
data$sqWater<-sqrt(21-data$W1VASwater1)# use the normal

hist_func(data, data$sqWater)
############normalized###########################################
#getting data into long form
names(data)
myvars<-c("subject","sqA1","sqB1","sqC1","sqD1","sqWater","risk")
data2<-data[myvars]
head(data2)
long_data<-melt(data2, id.vars = c("subject", "risk"))
head(long_data)
names(long_data)<-c("subject", "risk", "taste", "rating")
chlong_data$taste <- factor(chlong_data$taste,
                            levels = c("sqA1","sqB1","sqC1","sqD1","sqWater"),
                            labels = c("high fat high sugar", "high fat low sugar", "low fat high sugar", "low fat low sugar", "tasteless"))

#baseline model
model1<-lme(rating ~ 1, random = ~1|subject/taste, data = long_data, method ="ML", na.action = na.omit)
summary(model1)
#risk model
model_risk<-lme(rating ~ risk, random = ~1|subject/taste, data = long_data, method ="ML", na.action = na.omit)
summary(model_risk)
#risk and taste model
model_tastes<-lme(rating ~ risk+taste, random = ~1|subject/taste, data = long_data, method ="ML", na.action = na.omit)
summary(model_tastes)
#interaction model
model_int<-lme(rating ~ risk+taste+risk:taste, random = ~1|subject/taste, data = long_data, method ="ML", na.action = na.omit)
summary(model_int)
#looking at the graph
means<-summarySE(long_data, measurevar = "rating", groupvars = c("taste","risk"), na.rm = TRUE)
head(means)
plot2<-ggplot(means, aes(risk, mean, group=taste, colour=taste, fill=taste)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.5, position=position_dodge(.9), colour="black")+
  facet_grid(.~taste)
plot2
leastsquare = lsmeans(model_tastes, 
                      pairwise ~ taste, 
                      adjust="bon")
x<-leastsquare$contrasts
x<-as.matrix(x)
write.table(x,"~/Google Drive/choco_ana/palatability_results.csv", sep=",", row.names=F)
#graphing untransformed
myvars<-c("subject","W1VASmA1","W1VASmB1","W1VASmC1","W1VASmD1","W1VASwater1","risk")
data2<-data[myvars]
head(data2)
chlong_data<-melt(data2, id.vars = c("subject", "risk"))
head(chlong_data)
names(chlong_data)<-c("subject", "risk", "taste", "rating")
chlong_data$taste <- factor(chlong_data$taste,
                    levels = c("W1VASmA1","W1VASmB1","W1VASmC1","W1VASmD1","W1VASwater1"),
                    labels = c("high-fat/high-sugar", "high-fat", "high-sugar", "low-fat/low-sugar", "tasteless"))
chlong_data$risk <- factor(chlong_data$risk,
                            levels = c("AtRisk","NoRisk"),
                            labels = c("High Risk", "Low Risk"))
means<-summarySE(chlong_data, measurevar = "rating", groupvars = c("taste", "risk"), na.rm = TRUE)
means
cbPalette <- c("#CCCCCC", "#999999","#333333","#CCCCC9", "#CCCCC3")
plot3<-ggplot(means, aes(risk, mean, group=taste, colour=taste, fill=taste)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.5, position=position_dodge(.9), colour="black")+
  facet_grid(.~taste)+theme_bw() + scale_fill_manual(values=cbPalette)+scale_color_manual(values=cbPalette)+
  theme(panel.background = element_rect(fill = "transparent"), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_blank(),axis.text.x  = element_text(size=20))+
  theme(axis.title.y = element_blank(),axis.text.y  = element_text(size=20))+
  theme(legend.text = element_text(size = 26))+
  theme(legend.title = element_text(size=35, face="bold"))+
  theme(plot.margin = unit(c(1,6,1,6),"cm"), plot.background = element_rect(fill = "transparent"))+ theme(legend.position="none")+theme(strip.text.x = element_text(size=18),
                                                                                                                                       strip.background = element_rect(colour="black", fill="transparent"))
plot3

