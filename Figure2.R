library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)
library(gamm4)
library(anytime)

setwd("C:/Users/Rocky/Box Sync/Li6400 Experiments/Data/Experiment1/Modified/Controls/")


###### SET UP FOR CONTROL-BASED WATER LOSS MODELS######
species <- c("Tt", "He", "Rm", "Hw", "Sf")

for (a in 1:length(species)) {
  # CALL IN BIOMASS OBJECT FROM EXCEL FILE, SUBSET TO JUST CONTROL MEASUREMENTS OF SPECIES, READ IN AREA, SUBTRACT BY AREA TO NORMALALIZE TO BIOMASS
  biomass <- data.frame(read_excel("C:/Users/Rocky/Box Sync/Li6400 Experiments/Experiment1.xlsx", sheet = "Mass Data"))
  biomass <- biomass[c(which(biomass$Type == "C" & biomass$Species == species[a])), ]
  area <- read_excel("C:/Users/Rocky/Box Sync/Li6400 Experiments/Experiment1.xlsx", sheet = "Area")
  area <- filter(area, Type == "C") %>% filter(Species == species[a])
  area <- area[, c(3, 6)]
  biomass <- merge(biomass, area, by = "Trial")
  biomass$Mass <- biomass$Mass / biomass$SA
  biomass <- biomass[, c(2, 3, 1, 4, 5)]
  # CALCULATES RELATIVE BIOMASS AT EACH POINT
  for (q in 1:5) {
    biomass[which(biomass$Trial == q), 5] <- biomass[which(biomass$Trial == q), 5] / biomass[min(which(biomass$Trial == q)), 5]
  }
  
  colnames(biomass)[5] <- "y"
  if(species[a] == "Hw"){
    biomass<-biomass[-c(which(biomass$Trial==4|biomass$Trial==2)),]
  }
  
  #plot(y~Time,data=biomass,main=species[a])
  
assign(paste0(species[a], "_con.ws"), gam(y ~ s(Time, k = 4), data = biomass))
}
  
##### DETERMINE CONTROL-PHOTOSYNTHETIC RATES######
dat2 <- NULL
files <- list.files()
files <- files[-c(1)]
i <- 1

for (i in 1:length(files)) {
  dat <- read_excel(files[i])
  dat$HHMMSS <- anytime(paste("2/3/2000", dat$HHMMSS))
  dat$Obs <- as.numeric(difftime(dat$HHMMSS, dat$HHMMSS[1])) / 60
  fn <- sub("\\.xlsx$", "", files[i])
  sp <- substr(fn, 1, 2)
  type <- substr(fn, 3, 3)
  rep <- as.numeric(substr(fn, 4, 4))
  dat <- dat[c(which(dat$StableF == "1")), ]
  dat <- dat[c(1, 18, 19, 24)]
  dat$Obs <- as.numeric(dat$Obs)
  area <- data.frame(read_excel("C:/Users/Rocky/Box Sync/Li6400 Experiments/Experiment1.xlsx", sheet = "Area"))
  area <- area[c(which(area$Type == "C")), ]
  area <- area[c(which(area$Species == sp)), ]
  area <- area[c(which(area$Trial == rep)), ]
  # Calculate Photosynthetic rate
  dat$SA <- area$SA
  dat$FDA <- (dat$Flow * 0.000001) / (dat$SA * 0.0001)
  dat$Photo <- ((dat$CO2R - dat$CO2S)) * dat$FDA
  dat <- dat[, c(1, 7)]
  dat$sp <- sp
  dat2 <- rbind(dat2, dat)
}



colnames(dat2)[1] <- "Time"
dat2 <- data.frame(dat2)

species <- c("Tt", "He", "Rm", "Hw", "Sf")
for (a in 1:length(species)) {
  sub <- data.frame(dat2[which(dat2$sp == species[a]), 1])
  colnames(sub)[1] <- "Time"
  dat2[which(dat2$sp == species[a]), 4] <- predict(get(paste0(species[a], "_con.ws")), newdata = sub)
}

dat2 <- dat2[-c(1324),]
dat3 <- dat2


aggregate(Photo~sp,dat2,FUN=mean)

ggplot(dat2) +
  aes(x = V4, y = Photo, colour = sp) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(sp)) +
  xlab("Relative Biomass") +
  ylab("Carbon Uptake Rate")

library(readxl)
library(dplyr)
library(ggplot2)
library(drc)
library(aomisc)
setwd("C:/Users/Rocky/Box Sync/Li6400 Experiments/Data/Experiment1/Modified/")

###### SET UP FOR EXPERIMENTAL-BASED WATER LOSS MODELS######
species <- c("Tt", "He", "Rm", "Hw", "Sf")

for (a in 1:length(species)) {
  biomass <- data.frame(read_excel("C:/Users/Rocky/Box Sync/Li6400 Experiments/Experiment1.xlsx", sheet = "Mass Data"))
  biomass <- biomass[c(which(biomass$Type == "E" & biomass$Species == species[a])), ]
  for (q in 1:5) {
    biomass[which(biomass$Trial == q), 5] <- biomass[which(biomass$Trial == q), 5] / biomass[min(which(biomass$Trial == q)), 5]
  }
  colnames(biomass)[5] <- "y"
  #assign(paste0(species[a],"_exp.ws"),nls(y ~ SSasymp(Time, yf, y0, log_alpha), data = biomass))
  assign(paste0(species[a], "_exp.ws"), gam(y ~ s(Time, k = 4), data = biomass))
  #model.exp.wc<-nls(y ~ SSasymp(Time, yf, y0, log_alpha), data = biomass)
  #plot(y = biomass$y * 100, x = biomass$Time, xlab = "Time", ylab = "Water exptent", main = species[a], ylim = c(0, 100))
  #points(x=biomass$Time, y= predict(model.exp.wc,newdata = biomass)*100, col = "red")
}

##### DETERMINE EXPERIMENTAL PHOTOSYNTHETIC RATES######
dat2 <- NULL
files <- list.files()
files <- files[-c(1)]
i <- 1
for (i in 1:length(files)) {
  dat <- read_excel(files[i])
  dat$HHMMSS <- anytime(paste("1/1/2000", dat$HHMMSS))
  dat$Obs <- as.numeric(difftime(dat$HHMMSS, dat$HHMMSS[1], "seexpds")) / 60
  
  fn <- sub("\\.xlsx$", "", files[i])
  sp <- substr(fn, 1, 2)
  type <- "E"
  rep <- as.numeric(substr(fn, 3, 3))
  
  dat <- dat[c(which(dat$StableF == "1")), ]
  dat <- dat[c(1, 18, 19, 24)]
  
  
  # par(mfrow=c(1,5))
  # plot(x=dat$Obs,y=dat$Photo,main=fn,xlab = "Time (min)",ylab="Fake Photo")
  area <- data.frame(read_excel("C:/Users/Rocky/Box Sync/Li6400 Experiments/Experiment1.xlsx", sheet = "Area"))
  area <- area[c(which(area$Type == "E")), ]
  area <- area[c(which(area$Species == sp)), ]
  area <- area[c(which(area$Trial == rep)), ]
  
  # Calculate Photosynthetic rate
  dat$SA <- area$SA
  dat$FDA <- (dat$Flow * 0.000001) / (dat$SA * 0.0001)
  dat$Photo <- ((dat$CO2R - dat$CO2S)) * dat$FDA
  dat <- dat[, c(1, 7)]
  dat$sp <- sp
  dat2 <- rbind(dat2, dat)
}
colnames(dat2)[1] <- "Time"
dat2 <- data.frame(dat2)

for (a in 1:length(species)) {
  sub <- data.frame(dat2[which(dat2$sp == species[a]), 1])
  colnames(sub)[1] <- "Time"
  dat2[which(dat2$sp == species[a]), 4] <- predict(get(paste0(species[a], "_exp.ws")), newdata = sub)
}


for (a in 1:length(species)) {
  # exptrolPhoto
  sub <- data.frame(dat3[which(dat3$sp == species[a]), ])
  assign(paste0(species[a], "_PHOTO_exp.ws"), gam(Photo ~ s(`V4`, k = 6), data = sub))
  
  # ExperimentalPhoto
  sub <- data.frame(dat2[which(dat2$sp == species[a]), ])
  assign(paste0(species[a], "_PHOTO_EXP.ws"), gam(Photo ~ s(`V4`, k = 6), data = sub))
}

dat4 <- dat2
sub <- dat2[which(dat2$sp == "Tt"), ]
sub[, 4] <- seq(0, 1, length.out = nrow(sub))
dat4[which(dat4$sp == "Tt"), 4] <- sub[, 4]
dat4[which(dat4$sp == "Tt"), 2] <- predict(Tt_PHOTO_EXP.ws, newdata = sub) - predict(Tt_PHOTO_exp.ws, newdata = sub)


sub <- dat2[which(dat2$sp == "He"), ]
sub[, 4] <- seq(0, 1, length.out = nrow(sub))
dat4[which(dat4$sp == "He"), 4] <- sub[, 4]
dat4[which(dat4$sp == "He"), 2] <- predict(He_PHOTO_EXP.ws, newdata = sub) - predict(He_PHOTO_exp.ws, newdata = sub)

sub <- dat2[which(dat2$sp == "Sf"), ]
sub[, 4] <- seq(0, 1, length.out = nrow(sub))
dat4[which(dat4$sp == "Sf"), 4] <- sub[, 4]
dat4[which(dat4$sp == "Sf"), 2] <- predict(Sf_PHOTO_EXP.ws, newdata = sub) - predict(Sf_PHOTO_exp.ws, newdata = sub)


sub <- dat2[which(dat2$sp == "Rm"), ]
sub[, 4] <- seq(0, 1, length.out = nrow(sub))
dat4[which(dat4$sp == "Rm"), 4] <- sub[, 4]
dat4[which(dat4$sp == "Rm"), 2] <- predict(Rm_PHOTO_EXP.ws, newdata = sub) - predict(Rm_PHOTO_exp.ws, newdata = sub)

sub <- dat2[which(dat2$sp == "Hw"), ]
sub[, 4] <- seq(0, 1, length.out = nrow(sub))
dat4[which(dat4$sp == "Hw"), 4] <- sub[, 4]
dat4[which(dat4$sp == "Hw"), 2] <- predict(Hw_PHOTO_EXP.ws, newdata = sub) - predict(Hw_PHOTO_exp.ws, newdata = sub)


dat4[which(dat4$sp == "Tt"), 2] <- dat4[which(dat4$sp == "Tt"), 2] + min(max(dat4[which(dat4$sp == "Tt"), 2])) / max(dat4[which(dat4$sp == "Tt"), 2])
dat4[which(dat4$sp == "He"), 2] <- dat4[which(dat4$sp == "He"), 2] + min(max(dat4[which(dat4$sp == "He"), 2])) / max(dat4[which(dat4$sp == "He"), 2])
dat4[which(dat4$sp == "Sf"), 2] <- dat4[which(dat4$sp == "Sf"), 2] + min(max(dat4[which(dat4$sp == "Sf"), 2])) / max(dat4[which(dat4$sp == "Sf"), 2])
dat4[which(dat4$sp == "Rm"), 2] <- dat4[which(dat4$sp == "Rm"), 2] + min(max(dat4[which(dat4$sp == "Rm"), 2])) / max(dat4[which(dat4$sp == "Rm"), 2])
dat4[which(dat4$sp == "Hw"), 2] <- dat4[which(dat4$sp == "Hw"), 2] + min(max(dat4[which(dat4$sp == "Hw"), 2])) / max(dat4[which(dat4$sp == "Hw"), 2])

dat4 <- dat4[-c(which(dat4$sp == "Sf" & dat4$V4 < 0.4)),]


dat3$type <- "Dead"
dat2$type <- "Natural"
dat4$type <- "Residual"

dat2 <- rbind(dat2, dat3)
dat2 <- rbind(dat2, dat4)



dat2<-dat2[-c(which(dat2$sp=="Rm")),]
colnames(dat2)[3] <- "Species"



ggplot(dat2) +
  aes(x = V4, y = Photo, colour = Species) +
  geom_point(shape = "circle", size = 0.75)+
  scale_color_hue(direction = 1) +
  theme_classic(base_size = 18) +
  facet_grid(cols = vars(type), rows = vars(Species)) +
  xlab("Relative Biomass") +
  ylab(expression("Carbon Assimilation(" ~ μmol ~ C ~ m^-2 ~ s^-1 ~ ")")) +
  geom_hline(yintercept = 0)+scale_color_brewer(palette = "BrBG", direction = 1)+ theme(axis.text.x = element_text(angle = -45))


ggsave("Fig2a.eps",height=2500,width=3000,units="px",dpi=300)


