#BMI calculator
#KG/M2
#2.20462lb/1kg
#2.54cm/1in
test<-read.table("~/Google Drive/choco_ana/High_Low_Risk_Data.csv", sep=",", header=T)
head(test)
tail(test)
BMI_cal<-function(height, weight){
  weight_kg<-(weight/2.20462)
  height_m<-(height*2.54)/100
  BMI=weight_kg/(height_m)^2
  return(BMI)
}

test$FatherBMI<-BMI_cal(test$W1FatherHt, test$W1FatherWt)
test$MotherBMI<-BMI_cal(test$W1MotherHt, test$W1MotherWt)
head(test)

OVOB<-function(BMI, data){
  data$ov_ob[BMI<18.5]<-"Underweight"
  data$ov_ob[BMI>=18.5 & BMI<25]<-"Normalweight"
  data$ov_ob[BMI>=25 & BMI<30]<-"Overweight"
  data$ov_ob[BMI>=30]<-"Obese"
  return(data$ov_ob)
}

test$father_ovob<-OVOB(test$FatherBMI, test)
test$father_ovob

test$mother_ovob<-OVOB(test$MotherBMI, test)
test$mother_ovob
head(test)
write.table(test, "~/Google Drive/choco_ana/BMI_parents.csv", row.names = F, sep=",")

# parent<-read.table("~/Google Drive/choco_ana/BMI_parents.csv", header=T, sep=",")
# head(parent)
# kid<-read.table("~/Google Drive/choco_ana/slope_int.txt", header=T, sep="\t")
# head(kid)
# total<-merge(parent, kid, by=c("subject"))
data<-read.table("~/Google Drive/choco_ana/total_data.txt", header=T, sep="\t")
head(data)
summary(data)
data$risk[data$father_ovob=="Overweight" & data$mother_ovob == "Overweight"]<-"AtRisk"
data$risk[data$father_ovob=="Obese" & data$mother_ovob == "Obese"]<-"AtRisk"
data$risk[data$father_ovob=="Obese" & data$mother_ovob == "Overweight"]<-"AtRisk"
data$risk[data$father_ovob=="Overweight" & data$mother_ovob == "Obese"]<-"AtRisk"
data$risk[data$father_ovob=="Normalweight" & data$mother_ovob == "Normalweight"]<-"NoRisk"
data$risk[data$father_ovob=="Normalweight" & data$mother_ovob == "Overweightweight"]<-"NoRisk"
data$risk[data$father_ovob=="Overweight" & data$mother_ovob == "Normalweight"]<-"NoRisk"
data$risk[data$father_ovob=="Obese" & data$mother_ovob == "Normalweight"]<-"NoRisk"
data$risk[data$father_ovob=="Normalweight" & data$mother_ovob == "Obese"]<-"NoRisk"

myvars<-c("subject", "risk")
data2<-data[myvars]
head(data2)
data2<-na.omit(data2)
head(data2)
data2
data2$risk<-as.factor(data2$risk)
summary(data2$risk)
