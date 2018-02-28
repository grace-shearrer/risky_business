library(remef)
library(multcomp)
library(MASS)
library(ggplot2)
library(reshape)
cbPalette <- c("#000000", "#33CC33", "#0066CC", "#FF3333", "#9900CC", "#FFFF00")
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

fit_function<-function(x,y,data){
  fit<-lm(y~x, data=data, na.rm=T)
  sumfit<-summary(fit)
  final<-coef(summary(fit))[, 4]
  final[2:4]
}

fit_function2<-function(x,y,data){
  library(lm.beta)
  fit<-lm(y~x, data=data, na.rm=T)
  beta<-lm.beta(fit)
  y<-coef(summary(beta))[,2]
  y[2:3]
}

CI_low_function<-function(x,y,data){
  fit<-lm(y~x, data=data, na.rm=T)
  a<-confint.default(fit)[,1] #2.5%
  a[2:3]
}

CI_high_function<-function(x,y,data){
  fit<-lm(y~x, data=data, na.rm=T)
  b<-confint.default(fit)[,2] #97.5%
  b[2:3]
}



meanBar<-function(df, measurevar, groupvar){
  means <- summarySE(df, measurevar=measurevar, groupvars=groupvar)
  means$ymin<-means$mean-means$se
  means$ymax<-means$mean+means$se
  plot1<-ggplot(means, aes_string(groupvar, "mean", fill=groupvar)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=ymin, ymax=ymax),
                  width=.2)+ scale_colour_manual(values=cbPalette)
  return(list(measurevar, plot1, means))
}

add_func<-function(df){
  mean<-as.numeric(df[4])
  se<-as.numeric(df[6])
  x<-mean+se
  return(x)
}

sub_func<-function(df){
  mean<-as.numeric(df[4])
  se<-as.numeric(df[6])
  x<-mean-se
  return(x)
}


hist_func<-function(data, variable){
  plot1<-ggplot(data, aes(variable))+geom_histogram()
  return(plot1)
}


OVOB<-function(BMI, data){
  data$ov_ob[BMI<18.5]<-"Underweight"
  data$ov_ob[BMI>=18.5 & BMI<25]<-"Normalweight"
  data$ov_ob[BMI>=25 & BMI<30]<-"Overweight"
  data$ov_ob[BMI>=30]<-"Obese"
  return(data$ov_ob)
}

BMI_cal<-function(height, weight){
  weight_kg<-(weight/2.20462)
  height_m<-(height*2.54)/100
  BMI=weight_kg/(height_m)^2
  return(BMI)
}


mult_func<-function(df){
  x<-df[33]*df[31]
  return(x)
}