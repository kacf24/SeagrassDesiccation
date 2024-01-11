library(readr)
library(psychrolib)
library(ggplot2)
library(lubridate)
library(anytime)
library(latex2exp)
SetUnitSystem("IP")

#####time2season function from HydroTSM package

time2season <- function(x, out.fmt="months", type="default") {
  
  # Checking that 'class(x)==Date'
  #if ( ( !( class(x) %in% c("Date", "POSIXct", "POSIXt") ) ) && TRUE ) 
  if (!( is(x, "Date") | is(x, "POSIXct") | is(x, "POSIXt") )) 
    stop("Invalid argument: 'x' must be in c('Date', 'POSIXct', 'POSIXt') !")
  
  # Checking the class of out.fmt
  if (is.na(match(out.fmt, c("seasons", "months") ) ) )
    stop("Invalid argument: 'out.fmt' must be in c('seasons', 'months')")
  
  # Checking that the user provided a valid value for 'type'   
  valid.types <- c("default", "FrenchPolynesia")    
  if (length(which(!is.na(match(type, valid.types )))) <= 0)  
    stop("Invalid argument: 'type' must be in c('default', 'FrenchPolynesia')")
  
  ####################
  months <- format(x, "%m")
  
  if (type=="default") {
    winter <- which( months %in% c("12", "01", "02") )
    spring <- which( months %in% c("03", "04", "05") )
    summer <- which( months %in% c("06", "07", "08") )
    autumn <- which( months %in% c("09", "10", "11") ) 
  } else if (type=="FrenchPolynesia") {
    winter <- which( months %in% c("12", "01", "02", "03") )
    spring <- which( months %in% c("04", "05") )
    summer <- which( months %in% c("06", "07", "08", "09") )
    autumn <- which( months %in% c("10", "11") ) 
  } # ELSE end
  
  # Creation of the output, with the same length of the 'x' input
  seasons <- rep(NA, length(x))
  
  if (out.fmt == "seasons") {
    
    seasons[winter] <- "Winter"
    seasons[spring] <- "Spring"
    seasons[summer] <- "Summer"
    seasons[autumn] <- "Fall"
    
  } else { # out.fmt == "months"
    
    if (type=="default") {
      seasons[winter] <- "DJF"
      seasons[spring] <- "MAM"
      seasons[summer] <- "JJA"
      seasons[autumn] <- "SON"
    } else  if (type=="FrenchPolynesia") {
      seasons[winter] <- "DJFM"
      seasons[spring] <- "AM"
      seasons[summer] <- "JJAS"
      seasons[autumn] <- "ON"
    } # IF end
    
  } # IF end
  
  return(seasons)
  
} # 'time2season' END




#Load in data
model <- read_csv("C:/Users/Rocky/Desktop/LI6400_FinalFigures/model.csv")
tides <- read_csv("C:/Users/Rocky/Desktop/LI6400_FinalFigures/tides.csv")

#Trim out junk data
model<-model[complete.cases(model$temp),]
model<-model[complete.cases(model$pressure),]
model<-model[complete.cases(model$wspd),]
model<-model[complete.cases(model$rh),]

#Calculate Model Parameters
model$psi <- model$pressure/2.036
model$theta = 25 + 19 * model$wspd
model$SatHumRatio <- GetSatHumRatio(model$temp,model$psi)
model$HumRatio <- GetHumRatioFromRelHum(model$temp, model$rh/100, model$psi)
model$evap <- model$theta*(model$SatHumRatio - model$HumRatio)


#Switch tides from MLLW to MLW
tides$`Predicted (m)` <- tides$`Predicted (m)`+0.037


tides<-aggregate(`Predicted (m)`~Date,tides,FUN=min)
tides$Date<-mdy(tides$Date)
model$valid_time_gmt <- anytime(model$valid_time_gmt,"GMT")
model$Date <- date(model$valid_time_gmt)
model<-model[,c(15,14)]
model<-merge(model,tides,by="Date")

model<-model[-c(which(model$`Predicted (m)` > 0)),]
model$Season<-time2season(model$Date,"seasons")
cbPalette <- c("#F0E442","#D55E00","#009E73","#0072B2")
mean(model$evap)
sd(model$evap)
ggplot(data=model)+geom_point(aes(x=evap,y=`Predicted (m)`,color=Season))+theme_classic(base_size = 18)+scale_colour_manual(values=cbPalette)+ylab("Tidal Height (m MLW)")+xlab(TeX("Water Evaporation Rate (kg H$_2$O m$^{-2}$ h$^{-1}$)"))+geom_vline(xintercept=0.839304,linetype=3,linewidth=1.5)

ggsave("Fig5.eps",height=2500,width=3000,units="px",dpi=300)

aggregate(evap~Season,model,FUN=sum)
aggregate(evap~Season,model,FUN=mean)
aggregate(evap~Season,model,FUN=sd)
#Divide these values by 4
aggregate(evap~Season,model,FUN=length)

