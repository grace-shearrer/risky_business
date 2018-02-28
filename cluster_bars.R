library(ggplot2)
data<-read.table("~/Google Drive/choco_ana/risk/final_pe.csv", header=T, sep=",")
head(data)
data$stat[data$stat=="1"]<-"low risk"
data$stat[data$stat=="2"]<-"high risk"

data10<-subset(data,data$cope == "cope10")
data10
data10$cluster[data10$cluster=="5"]<-"parietal operculum"
data10$cluster[data10$cluster=="4"]<-"frontal orbital cortex"
data10$cluster[data10$cluster=="3"]<-"insula"

ggplot(data10, aes(x=cluster, y=PE_mean, fill=stat)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PE_mean-PE_std, ymax=PE_mean+PE_std),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+ylab(
                  "Parameter estimate")+xlab(" ")+ scale_fill_discrete(
                    name="Obesity Risk")+theme(
                      axis.text.x  = element_text(size=25))+theme(axis.title.y = element_text(size=25),
                        axis.text.y  = element_text(size=25))

data15<-subset(data,data$cope == "cope15")
data15$cluster[data15$cluster=="8"]<-"parietal operculum"
data15$cluster[data15$cluster=="7"]<-"central opercular cortex"
data15$cluster[data15$cluster=="6"]<-"juxtapositional lobule"

ggplot(data15, aes(x=cluster, y=PE_mean, fill=stat)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PE_mean-PE_std, ymax=PE_mean+PE_std),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+ylab("Parameter estimate")+xlab(" ")+ scale_fill_discrete(name="Obesity Risk")+theme(
                  axis.text.x  = element_text(size=25))+theme(axis.title.y = element_text(size=25),
                                                              axis.text.y  = element_text(size=25))

data16<-subset(data,data$cope == "cope16")
data16$cluster[data16$cluster=="7"]<-"central opercular cortex"
data16$cluster[data16$cluster=="6"]<-"thalamus"
data16$cluster[data16$cluster=="5"]<-"caudate"

ggplot(data16, aes(x=cluster, y=PE_mean, fill=stat)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PE_mean-PE_std, ymax=PE_mean+PE_std),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+ylab("Parameter estimate")+xlab(" ")+ scale_fill_discrete(name="Obesity Risk")+theme(
                  axis.text.x  = element_text(size=25))+theme(axis.title.y = element_text(size=25),
                                                              axis.text.y  = element_text(size=25))

data19<-subset(data,data$cope == "cope19")
data19$cluster[data19$cluster=="2"]<-"supramarginal gyrus"
data19$cluster[data19$cluster=="1"]<-"planum temporale"
#data19$cluster[data19$cluster=="5"]<-"caudate"

ggplot(data19, aes(x=cluster, y=PE_mean, fill=stat)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PE_mean-PE_std, ymax=PE_mean+PE_std),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+ylab("Parameter estimate")+xlab(" ")+ scale_fill_discrete(name="Obesity Risk")+theme(
                  axis.text.x  = element_text(size=25))+theme(axis.title.y = element_text(size=25),
                                                              axis.text.y  = element_text(size=25))

data20<-subset(data,data$cope == "cope20")
data20$cluster[data20$cluster=="1"]<-"parietal operculum"
#data19$cluster[data19$cluster=="1"]<-"planum temporale"
#data19$cluster[data19$cluster=="5"]<-"caudate"

ggplot(data20, aes(x=cluster, y=PE_mean, fill=stat)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PE_mean-PE_std, ymax=PE_mean+PE_std),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+ylab("Parameter estimate")+xlab(" ")+ scale_fill_discrete(name="Obesity Risk")+theme(
                  axis.text.x  = element_text(size=25))+theme(axis.title.y = element_text(size=25),
                                                              axis.text.y  = element_text(size=25))

data21<-subset(data,data$cope == "cope21")
data21$cluster[data21$cluster=="8"]<-"parietal operculum"
data21$cluster[data21$cluster=="7"]<-"cingulate gyrus"
data21$cluster[data21$cluster=="6"]<-"juxapositional lobule (a)"
data21$cluster[data21$cluster=="5"]<-"postcentral gyrus (a)"
data21$cluster[data21$cluster=="4"]<-"postcentral gyrus (b)"
data21$cluster[data21$cluster=="3"]<-"juxapositional lobule (b)"

ggplot(data21, aes(x=cluster, y=PE_mean, fill=stat)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PE_mean-PE_std, ymax=PE_mean+PE_std),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+ylab("Parameter estimate")+xlab(" ")+ scale_fill_discrete(name="Obesity Risk")+theme(
                  axis.text.x  = element_text(size=25))+theme(axis.title.y = element_text(size=25),
                                                              axis.text.y  = element_text(size=25))

data22<-subset(data,data$cope == "cope22")
data22$cluster[data22$cluster=="11"]<-"central opercular cortex"
data22$cluster[data22$cluster=="10"]<-"parietal operculum"
data22$cluster[data22$cluster=="9"]<-"middle temporal gyrus (a)"
data22$cluster[data22$cluster=="8"]<-"juxapositional lobule"
data22$cluster[data22$cluster=="7"]<-"cingulate gyrus"
data22$cluster[data22$cluster=="6"]<-"middle temporal gyrus (b)"
data22$cluster[data22$cluster=="5"]<-"postcentral gyrus (a)"
data22$cluster[data22$cluster=="4"]<-"postcentral gyrus (b)"

ggplot(data22, aes(x=cluster, y=PE_mean, fill=stat)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PE_mean-PE_std, ymax=PE_mean+PE_std),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+ylab("Parameter estimate")+xlab(" ")+ scale_fill_discrete(name="Obesity Risk")+theme(
                  axis.text.x  = element_text(size=15))+theme(axis.title.y = element_text(size=25),
                                                              axis.text.y  = element_text(size=25))

data9<-subset(data,data$cope == "cope9")
data9$cluster[data9$cluster=="3"]<-"parietal operculum"
data9$cluster[data9$cluster=="2"]<-"precentral gyrus"

ggplot(data9, aes(x=cluster, y=PE_mean, fill=stat)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PE_mean-PE_std, ymax=PE_mean+PE_std),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+ylab("Parameter estimate")+xlab(" ")+ scale_fill_discrete(name="Obesity Risk")+theme(
                  axis.text.x  = element_text(size=25))+theme(axis.title.y = element_text(size=25),
                                                              axis.text.y  = element_text(size=25))
sum_data<-read.table("~/Google Drive/choco_ana/total_data.txt", header=T, sep="\t")
head(sum_data)
data2<-subset(sum_data, sum_data$risk != "<NA>")
head(data2)
library(psych)
describeBy(data2$Intercept, data2$risk)
describeBy(data2$MotherBMI, data2$risk)
describeBy(data2$FatherBMI, data2$risk)
summary(data2$risk)
