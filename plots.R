library(ggplot2)
data<-read.table("~/Google Drive/choco_ana/risk/PE2.csv", sep=",", header=T)
levels(data$ROI)
levels(data$contrast)
PEbar<-function(data, y, x){
  library(ggplot2)
  cbPalette <- c("#FF9900", "#0099FF")
  milks<-subset(data, data$contrast==y & data$ROI==x)
  plot<-ggplot(milks, aes(x=risk, y=mean, fill=risk)) + guides(fill=FALSE)+
    geom_bar(colour="black", stat="identity", position=position_dodge()) + geom_errorbar(
      aes(ymin=mean-std, ymax=mean+std), colour="black", width=.1, position=position_dodge(.9))+ 
    scale_fill_manual(values=cbPalette)+
    theme(panel.background = element_rect(fill = "transparent"), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.title.x = element_blank(),axis.text.x  = element_text(size=26))+
    theme(axis.title.y = element_blank(),axis.text.y  = element_text(size=26))+
    theme(legend.text = element_text(size = 26))+
    theme(legend.title = element_text(size=32, face="bold"))+
    theme(plot.margin = unit(c(1,6,1,6),"cm"), plot.background = element_rect(fill = "transparent"), legend.background = element_rect(fill = "transparent"), legend.box.background = element_rect(fill = "transparent"))
  
  return(plot)
}
PEbar(data, "LFHS milkshake > tasteless", "caudate")

