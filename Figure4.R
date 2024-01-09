library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)



mround <- function (x, accuracy, f = round) {
  f(x/accuracy) * accuracy
}


setwd("C:/Users/Rocky/Box Sync/Li6400 Experiments")
Experiment3Area <- data.frame(read_excel("Experiment3.xlsx", sheet = "Area"))
Experiment3Area$key <- paste0(Experiment3Area$Species,Experiment3Area$Trial)


mod.con<-load("con_mod.rda")
mod.wl<-load("samp_wl_mod.rda")


files<-list.files("Data\\Experiment3\\Modified\\Licor")

for(i in 1:length(files)){
  temp <- data.frame(read_excel(paste0("Data\\Experiment3\\Modified\\Licor\\",files[i])))
  fn<-sub('\\.xlsx$', '', files[i])   
  colnames(temp)[1] <- "y"
  temp$Time <- temp$y/2
  
  temp$WaterContent <- predict(newdata = temp,Tt_con.ws)
  temp<-temp[c(25,18,19,24,57)]
  
  
  temp$SA<-Experiment3Area[c(which(Experiment3Area$key == fn)),5]
  temp$FDA <- (temp$Flow*0.000001)/(temp$SA*0.0001)
  temp$Photo <- (temp$CO2R-temp$CO2S)*temp$FDA
  
  
  temp$Con <- predict(newdata = temp,Tt_con.ws)
  temp$Final <- temp$Photo - temp$Con
  
  
  assign(files[i],temp)
  rm(temp)
}

test_data <- bind_rows(mget(grep(pattern = ".xlsx$", x = ls(), 
                                 value = TRUE)), .id = 'filename')
test_data$PARi <- mround(test_data$PARi,5)

  agg <- test_data %>%
  group_by(PARi,) %>%
  summarise(Final = mean(Final))


a<-ggplot(test_data, aes(PARi, Photo)) + 
  geom_point() + geom_smooth() + theme_classic() + xlab("PAR") + ylab("Photosynthetic Rate")
b<-ggplot(test_data, aes(PARi, Con)) + 
  geom_point() + geom_smooth() + theme_classic() + xlab("PAR") + ylab("Photosynthetic Rate")
c<-ggplot(test_data, aes(PARi, Final)) + 
  geom_point() + geom_smooth() + theme_classic() + xlab("PAR") + ylab("Photosynthetic Rate")

library(gridExtra)

grid.arrange(a,b,c)

files<-list.files("Data\\Experiment3\\Modified\\PAM")
He<-read_xlsx(paste0("Data\\Experiment3\\Modified\\PAM\\",files[1]))
Hw<-read_xlsx(paste0("Data\\Experiment3\\Modified\\PAM\\",files[2]))
Sf<-read_xlsx(paste0("Data\\Experiment3\\Modified\\PAM\\",files[3]))
Tt<-read_xlsx(paste0("Data\\Experiment3\\Modified\\PAM\\",files[4]))

all_PAM<-rbind(He,Hw,Tt,Sf)
agg <- all_PAM %>%
  group_by(Species,`1:PAR`, Time) %>%
  summarise(ETR = mean(`1:ETR`), std = sd(`1:ETR`))

agg$std[which(is.na(agg$std))]<-0

agg$Time <- as.character(agg$Time)
agg[c(which(agg$Time=="0")),3]<-"0 minutes"
agg[c(which(agg$Time=="60")),3]<-"60 minutes"

ggplot(agg, aes(`1:PAR`,ETR, col = Species))+geom_point()+theme_classic(base_size = 18)+ facet_grid(rows = vars(Time))+scale_color_brewer(palette = "BrBG", direction = 1)+xlab("Photosynthetic Photon Flux Density")+ylab("Relative Electron Transport Rate")+geom_linerange((aes(ymin=ETR-std,ymax=ETR+std)))+geom_smooth(se=F)
ggsave("Fig4.eps",height=2500,width=3000,units="px",dpi=300)


#For calculating RLC parameters
library(phytotools)
temp <- agg %>% filter(Species == "Sf") %>% filter(Time == "0 minutes")
PAR=temp$`1:PAR`
ETR=temp$ETR
fit = fitPGH(PAR,ETR)
plot(x=PAR, y=ETR)
alpha.rlc = fit$alpha[1]
beta.rlc = fit$beta[1]
ps.rlc = fit$ps[1]
ETRmax = ps.rlc*(alpha.rlc/(alpha.rlc + beta.rlc))*(beta.rlc/(alpha.rlc+beta.rlc))^(beta.rlc/alpha.rlc)
Ek = ETRmax/alpha.rlc 
with(fit, {
  P <- ps.rlc*(1-exp(-1*alpha.rlc*PAR/ps.rlc))*exp(-1*beta.rlc*PAR/ps.rlc) # the PGH model equation
  lines(PAR,P)
})



