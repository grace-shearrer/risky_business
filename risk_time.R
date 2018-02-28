library(reshape)
library(ggplot2)
library(lme4)
library(nlme)
library(plyr)
library(splines)
library(lmtest)
library(pbkrtest)
library(MuMIn)
library(psych)
data0<-read.table("~/Google Drive/choco_ana/Choco_W1-4_BMI_complete.csv", header=T, sep=",")
dim(data0)
head(data0)
data1<-read.table("~/Google Drive/choco_ana/check_data.csv", header=T, sep=",")
data<-merge(data0, data1, by="subject")
head(data)
dim(data)
myvars<-c("subject","W1BMI","W2BMI","W3BMI","W4BMI","risk", "MotherBMI", "FatherBMI")
data2<-data[myvars]

mytable <- table(data2$risk, data2$W4BMI) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)


data_melt<-melt(data2, id=c("subject","risk","MotherBMI", "FatherBMI"))
head(data_melt)
names(data_melt)<-c("subject","risk", "MotherBMI", "FatherBMI", "time","BMI")
data_melt$time
data_melt$Time[data_melt$time=="W1BMI"]<-1
data_melt$Time[data_melt$time=="W2BMI"]<-2
data_melt$Time[data_melt$time=="W3BMI"]<-3
data_melt$Time[data_melt$time=="W4BMI"]<-4
data_melt$Time
data_melt$Time<-as.numeric(as.character(data_melt$Time))
ggplot(data_melt, aes(time, BMI, group=subject, shape=subject, color=subject)) + geom_line() + geom_point()+ theme(legend.position="none")+ facet_grid(. ~ risk)
means<-summarySE(data_melt, measurevar = "BMI", groupvars = c("risk","time"))
head(means)
names(data_melt)
cbPalette <- c("#FF9900", "#0099FF")
plot1<-ggplot(means, aes(x=time, y=mean, colour=risk, group=risk, shape=risk, fill=risk)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, colour="black") +
  geom_line() +
  geom_point(aes(colour=risk))+theme_bw()+ scale_fill_manual(values=cbPalette)+scale_color_manual(values=cbPalette)+
  theme(panel.background = element_rect(fill = "transparent"), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_blank(),axis.text.x  = element_blank())+
  theme(axis.title.y = element_blank(),axis.text.y  = element_text(size=25, color="black"))+
  theme(legend.text = element_text(size = 25))+
  theme(plot.margin = unit(c(1,6,1,6),"cm"), plot.background = element_rect(fill = "transparent"), legend.background = element_rect(fill = "transparent"), legend.box.background = element_rect(fill = "transparent"))+
  geom_line(size=1.5) + 
  geom_point(size=6)+
  theme(legend.position="none")+ 
  coord_cartesian(ylim=c(20, 25))
plot1
#pdf("~/Google Drive/choco_ana/risk/BMIovertime.jpeg", width=12, height=12)
#print(plot1)
#dev.off()

head(data_melt)
t4<-subset(means, means$time == "W4BMI")
t4
library(plyr)
t4$risk<-revalue(t4$risk, c("AtRisk"="high risk", "NoRisk"="low risk"))
plot<-ggplot(t4, aes(x=risk, y=mean, fill=risk)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,
                color="black",
                position=position_dodge(.9))+theme_bw()+ scale_fill_manual(values=cbPalette)+scale_color_manual(values=cbPalette)+
  theme(panel.background = element_rect(fill = "transparent"), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_blank(),axis.text.x  = element_text(size=26, color="black"))+
  theme(axis.title.y = element_blank(),axis.text.y  = element_text(size=26, color="black"))+
  theme(legend.text = element_text(size = 26))+
  theme(plot.margin = unit(c(1,6,1,6),"cm"), plot.background = element_rect(fill = "transparent"), legend.background = element_rect(fill = "transparent"), legend.box.background = element_rect(fill = "transparent"))+
  theme(legend.position="none")

plot
pdf("~/Google Drive/choco_ana/risk/time4_bar.jpeg", width=12, height=12)
print(plot)
dev.off()


time4<-subset(data_melt, data_melt$Time==4)
head(time4)
t.test(time4$BMI~time4$risk)

fm0 <- lmer(BMI ~ Time + (Time|subject), data_melt)

fm3 <- lmer(BMI ~ Time + risk + (Time|subject), data_melt)
lrtest(fm0,fm3)
summary(fm3)
coefs <- data.frame(coef(summary(fm3)))
# get the KR-approximated degrees of freedom
df.KR <- get_ddf_Lb(fm3, fixef(fm3))
# get p-values from the t-distribution using the t-values and approximated
# degrees of freedom
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))
coefs

fm4 <- lmer(BMI ~ Time + MotherBMI + FatherBMI+ (Time|subject), data_melt)
summary(fm4)
lrtest(fm3,fm4)

coefs <- data.frame(coef(summary(fm4)))
# get the KR-approximated degrees of freedom
df.KR <- get_ddf_Lb(fm4, fixef(fm4))
# get p-values from the t-distribution using the t-values and approximated
# degrees of freedom
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))
coefs


a1 <- aov(BMI~risk+time, data=data_melt) 
summary(a1) #Type I
drop1(a1,~.,test="F") #Type III
TukeyHSD(a1)

slopes<-coef(fm3)$subject
ranef(fm3)$subject
write.table(slopes,"~/Google Drive/choco_ana/risk/slopes.txt", sep=" ")


fm3spline <- lmer(BMI ~ ns(Time, df=2) + (Time|subject), data_melt)
lrtest(fm3, fm3spline)

ggplot(data_melt, aes(x=Time, y=BMI, group=risk, color=risk)) +geom_point() + geom_smooth(method = "lm")
ggplot(data_melt, aes(x=Time, y=BMI, group=risk, color=risk)) +geom_point() + geom_smooth(method = "lm", formula = y ~ ns(x,2))

# extract coefficients
coefs <- data.frame(coef(summary(fm3)))
# get the KR-approximated degrees of freedom
df.KR <- get_ddf_Lb(fm3, fixef(fm3))
# get p-values from the t-distribution using the t-values and approximated
# degrees of freedom
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))
coefs

fm4 <- lmer(BMI ~ Time + risk + (Time|subject), data_melt)
summary(fm4)
fm4[1]

lrtest(fm3,fm4)
#not different
# extract coefficients
coefs4 <- data.frame(coef(summary(fm4)))
# get the KR-approximated degrees of freedom
df.KR4 <- get_ddf_Lb(fm4, fixef(fm4))
# get p-values from the t-distribution using the t-values and approximated
# degrees of freedom
coefs4$p.KR4 <- 2 * (1 - pt(abs(coefs4$t.value), df.KR4))
coefs4

r.squaredGLMM(fm4)
#fixed effects (marginal R2) and that incorporating the random effects (conditional R2).

mult_func<-function(df){
  x<-df[33]*df[31]
  return(x)
}
names(data)<-c("subject","W1BMI", "W2BMI",  "W3BMI","W4BMI", "W1FatherHt", "W1FatherWt","W1MotherHt","W1MotherWt" ,"W1FEQFatherEd",     
   "W1FEQMotherEd","W1Hispanic","W1Race" , "W1Race_2", "W1VASmA1", "W1VASmB1","W1VASmC1", "W1VASmD1", "Hunger" , "FatherBMI",         
  "MotherBMI","father_ovob","mother_ovob","Intercept1","Slope1" ,"intercept_demeaned" ,"slope_demeaned","risk", "Interaction")
head(data)
slopes_new<-read.table("~/Google Drive/choco_ana/risk/slopes.txt", sep="\t", header=T)
head(slopes_new)
data<-join(data,slopes_new)

head(data)
data$risk_fact<-revalue(data$risk, c("NoRisk"=0, "AtRisk"=1))
head(data)
data$risk_fact<-as.numeric(as.character(data$risk_fact))
head(data)
dim(data)
a<-mult_func(data)
names(a)<-"interaction2"
head(a)
write.table(data,"~/Google Drive/choco_ana/risk/all_data_newInt2.csv", sep=",", row.names=F)
