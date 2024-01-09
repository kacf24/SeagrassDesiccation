library(readxl)
library(dplyr)
library(ggplot2)
setwd("C:/Users/Rocky/Box Sync/Li6400 Experiments")
Experiment2Mass <- data.frame(read_excel("Experiment2.xlsx", sheet = "Mass Data"))
Experiment2Mass$key <- paste0(Experiment2Mass$Species,Experiment2Mass$Trial,Experiment2Mass$Time)


files<-list.files("Data\\Experiment2")

for(i in 1:length(files)){
  temp <- data.frame(read_excel(paste0("Data\\Experiment2\\",files[i])))
  fn<-sub('\\.xlsx$', '', files[i])
  temp$key <- paste0(temp$Species,temp$Trial,temp$Time)
  temp<-merge(temp,Experiment2Mass,by="key")
  temp <- temp[,c(2:5,10)]
  colnames(temp) <- c("Species","Trial","Time","FvFm","Mass")
  temp$WaterContent <- temp$Mass/max(temp$Mass)*100
  assign(files[i],temp)
  rm(temp)
}

test_data <- bind_rows(mget(grep(pattern = ".xlsx$", x = ls(), 
                                 value = TRUE)), .id = 'filename')


test_data<-test_data[-c(which(test_data$Species == "Rm")),]


a<-ggplot(test_data, aes(Time, FvFm, color = Species)) + geom_point()+facet_wrap(~Species) + theme_classic(base_size = 18)+xlab("Time")+scale_color_brewer(palette = "BrBG", direction = 1)+ theme(legend.position = "none")+labs(tag = "A")+ylab("Fv/Fm")+geom_smooth(se=F)
b<-ggplot(test_data, aes(WaterContent, FvFm, color = Species)) + geom_point()+facet_wrap(~Species) + theme_classic(base_size = 18)+xlab("Relative Biomass")+scale_color_brewer(palette = "BrBG", direction = 1)+ theme(legend.position = "none")+labs(tag = "B")+ylab("Fv/Fm")+geom_smooth(se=F)

library(gridExtra)
z<-grid.arrange(a,b)

ggsave("Fig3.eps",z,height=2500,width=3000,units="px",dpi=300)

