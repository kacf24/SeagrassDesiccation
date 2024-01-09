library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)

Mass <- read_excel("C:/Users/Rocky/Box Sync/Li6400 Experiments/Experiment1.xlsx")
Area <- read_excel("C:/Users/Rocky/Box Sync/Li6400 Experiments/Experiment1.xlsx", sheet = "Area")
Mass$Key <- paste0(Mass$Species,Mass$Type,Mass$Trial)
Area$Key <- paste0(Area$Species,Area$Type,Area$Trial)

Mass<-Mass[-c(which(Mass$Species=="Rm")),]
Area<-Area[-c(which(Area$Species=="Rm")),]

Data <- merge(Mass,Area,by="Key")
Data <- Data[,c(2,3,4,5,6,12)]
colnames(Data) <- c("Species","Type","Trial","Time","Mass","SA")


Data$Mass <- Data$Mass/Data$SA
Data$Key <- paste0(Data$Species,Data$Type,Data$Trial)
keys<-unique(Data$Key)


summary(glm(Mass~Species+Type+Time,data=Data,family=Gamma))


agg <- Data %>%
  group_by(Species, Time) %>%
  summarise(Massy = mean(Mass), std = sd(Mass))

agg$std[which(is.na(agg$std))]<-0




newDat<-NULL
for(i in 1:length(keys)){
  sub<-Data[which(Data$Key == keys[i]),]
  sub$Mass<-sub$Mass/max(sub$Mass, na.rm = TRUE)
  newDat<-rbind(newDat,sub)
}


agg2 <- newDat %>%
  group_by(Species, Time) %>%
  summarise(Massy = mean(Mass), std = sd(Mass))
agg2$std[which(is.na(agg2$std))]<-0

summary(glm(Mass~Species*Type+Time,data=Data,family=Gamma()))

a<-ggplot(agg) +
  aes(x = Time, y = Massy, colour = Species) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_brewer(palette = "BrBG", direction = 1) +
  theme_classic(base_size = 18) +labs(y = Biomass~(g/cm^2))+geom_linerange((aes(ymin=Massy-std,ymax=Massy+std)))+xlim(0,30)+ labs(tag = "A")
b<-ggplot(agg2) +
  aes(x = Time, y = Massy, colour = Species) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_brewer(palette = "BrBG", direction = 1) +
  theme_classic(base_size = 18) +labs(y = Relative~Biomass)+geom_linerange((aes(ymin=Massy-std,ymax=Massy+std)))+xlim(0,10)+facet_grid(rows=vars(Species))+ theme(legend.position = "none")+geom_smooth(se=FALSE,span=4000)+ labs(tag = "B")


z<-grid.arrange(a,b)
ggsave("Fig1.eps",z,height=2500,width=3000,units="px",dpi=300)
