###Library, theme, and directory####
library(ggplot2) ##
library(plotly)
library(readxl) ##
library(tidyverse) ##
library(stats)
library(magrittr)
library(dplyr) ##
library(ggpubr)
library(readr)
library(raster)
library(lubridate) ##
library(patchwork)
library(dataRetrieval) ##
library(openxlsx)
library(viridis)
library(coin)
library(FSA)
library(ggstatsplot)
library(exactRankTests)

theme_set(theme_classic()+
            theme(
              plot.title = element_text(size = 20),  # Title size
              axis.title.x = element_text(size = 18),                # X-axis title size
              axis.title.y = element_text(size = 18),                # Y-axis title size
              axis.text = element_text(size = 15),                   # Axis tick label size
              legend.title = element_text(size = 14),                 # Legend title size
              legend.text = element_text(size = 14)                   # Legend text size
            ))

setwd("/Users/pooirodriguez/Manuscript_Codes/RScripts and files")

#######################
#######Load Data#######
#######################
###Load and clean discharge data ####
#start and end date for discharge graph over the ENTIRE field season
startD <- "2023-09-24"
endD <- "2024-09-14"

#start and end date for discharge graph over the WINTER field season
winterStart <- "2023-12-01"
winterEnd <- "2024-02-28"

#Radford USGS gauge
site <- "03171000"
#USGS Discharge (flow_inst) in cfs and Stage (GH_inst) in feet parameter ID
param <- c("00060", "00065")

#Retrieve USGS guage data for discharge values at Radford site and organize columns
Qdata<- readNWISuv(site, param, startD, endD) |> renameNWISColumns()
names(Qdata) <- c("agency", "site", "Date", "Flow", "Flow_cfs", "Stage", "Stage_ft", "tz_cd") 

#convert cfs to cms
# 1 cfs = 0.028316847 cms
Qdata$FlowM <- Qdata$Flow * 0.028316847

#graph discharge and save for later
dischargeGraph<- ggplot(Qdata, aes(Date, (FlowM)))+ 
  geom_line(size = 2)+
  ylab("Discharge (cms)")

##DONE

###Load and clean field data ####
fieldD <- read_xlsx("Sampling.xlsx", sheet = "comparison")

#make a "site" column whether it is radford or kentland based on SampleID
#note! intensive is not a specific site..could be claytor or kentland
fieldD$site <- NA
for (i in 1:nrow(fieldD)){
  if(substring(fieldD$SampleID[i], 5, 5) == "K"){
    fieldD$site[i] <- "Kentland"
  }else{
    if(substring(fieldD$SampleID[i], 5, 5) == "R"){
      fieldD$site[i] <- "Radford"
    }else{
      fieldD$site[i] <- "Intensive"
    }
  }
}
#manually make the one "upstream test" at Kentland Farm
x <- which(fieldD$SampleID == "upstream test")
fieldD$site[x] <- "Kentland"

#out of the 'intensive' samples, the first round was at Claytor Lake, the rest were at Kentland Farm
int <- which(fieldD$site == "Intensive")
for(i in int){
  x <- substring(fieldD$SampleID[i], 7, 7) |>  as.numeric()
  if(x == 1){ 
    fieldD$site[i] <- "Claytor"
  }else{
    fieldD$site[i] <- "Kentland"
  }
}

#make a "place" column for where the sample is along the river's cross-section
fieldD$place <- NA
#notes:
#row #55 is the upstream test sample that does not fit this model
#samples after row #70 were all from the thalweg
for (i in 1:54){
  x <- substring(fieldD$SampleID[i], 10, 12) |> as.numeric()
  if(x %% 3 == 1){
    fieldD$place[i] <- "Bank"
  }else{if(x %% 3 == 2){
    fieldD$place[i] <- "Thalweg"
  }else{
    fieldD$place[i] <- "Middle"
  }}
}
for (i in 56:70){ 
  x <- substring(fieldD$SampleID[i], 10, 12) |> as.numeric()
  if(x %% 3 == 1){
    fieldD$place[i] <- "Bank"
  }else{if(x %% 3 == 2){
    fieldD$place[i] <- "Thalweg"
  }else{
    fieldD$place[i] <- "Middle"
  }}
}
#everything that is left was taken from the Thalweg. See notes above.
fieldD$place[which(is.na(fieldD$place))] <-  "Thalweg"

#make sure coords are both numeric 
fieldD$Lat <- as.numeric(fieldD$Lat)
fieldD$Lon <- as.numeric(fieldD$Lon)

##DONE

###Load and clean mp data ####
#load the final count columns from the raw MP data and make the first row the column names
mpD <- read_xlsx("MPdata.xlsx", sheet = "comp") |> dplyr::select('...23':'...27')  |>  drop_na('...27')
names(mpD) <- mpD[1,] # may come up with error regarding tibble but its okay to ignore
mpD <- mpD[-1,]

#get rid of headers
mpD <-mpD[-which(mpD$`Sample ID` == 'Sample ID'),]

#make sure numbers are numeric
mpD$FIBERS <- as.numeric(mpD$FIBERS)
mpD$FRAGS <- as.numeric(mpD$FRAGS)
mpD$FILM <- as.numeric(mpD$FILM)
mpD$AVGTOTAL <- as.numeric(mpD$AVGTOTAL)

##DONE

####Join field water quality measurements and microplastic concentration estimates by sampleID and clean dataset ####
data <- full_join(mpD, fieldD, by = c('Sample ID'= 'SampleID'))

###we see that first 9 MP samples are missing some data parameters; 
##let's fill those in:

#give place to mp data without ysi readings
for (i in 1:9){
  data$site[i] <- "Kentland"
  x <- substring(data$'Sample ID'[i], 10, 12) |> as.numeric()
  if(x %% 3 == 1){
    data$place[i] <- "Bank"
  }else{if(x %% 3 == 2){
    data$place[i] <- "Thalweg"
  }else{
    data$place[i] <- "Middle"
  }}
}

#add dates to beginning non-ysi mp measurements
data$Date[1:3] <- as.POSIXct('2023-09-24', format = '%Y-%m-%d', tz = "UTC")
data$Date[4:9] <- as.POSIXct('2023-10-08', format = '%Y-%m-%d', tz = "UTC")
#add time
#note: the day is not correct but will be deleted in a few lines
data$Time_SW[1:9] <- as.POSIXct(c("4:01", "4:06", "4:10", "11:11", "11:21", "11:24", "5:38", "5:42", "5:45"), format = '%H:%M', tz = "UTC")

#add 12 hours to afternoon times for them all to be in 24-hour time
#note: we never took samples before 9 am, so if hour time is less than that we know it was during the afternoon
for (i in 1:nrow(data)){
  if (as.numeric(format(data$Time_SW[i], "%H")) < 9){ 
    data$Time_SW[i] <- data$Time_SW[i] + 12*60*60 
  }
}
#create true time column without incorrect date from original time column
data$time <- format(data$Time_SW, format = '%H:%M', tz = "UTC")
#include correct date to time column
data$time <- as.POSIXct(paste(data$Date, data$time), format="%Y-%m-%d %H:%M", tz = "EST")
#separate month
data$month <- month(data$time)


#add discharge from radford gauge to these new dates!!
data$Discharge[1:9] <- as.numeric(c(1910, 1910, 1860, 1330, 1330, 1280, 1220, 1220, 1220))
data$Stage[1:9] <- as.numeric(c(1.56, 1.56,1.56,1.36,1.36,1.36,1.34, 1.34, 1.34))
#get rid of 6 that I could not count
data <- data %>% drop_na(AVGTOTAL)
#get rid of upstream test
data <- data[-c(which(data$`Sample ID` == "upstream test")),]


#seasonality###
data$season <- c(NA)
for (i in 1:nrow(data)){
  if (data$month[i] == 12 | data$month[i] == 1 | data$month[i] == 2){
    data$season[i] <- "Winter"
  }
  if (data$month[i] == 9 | data$month[i] == 10 | data$month[i] == 11){
    data$season[i] <- "Fall"
  }
  if (data$month[i] == 6 | data$month[i] == 7 | data$month[i] == 8){
    data$season[i] <- "Summer"
  }
  if (data$month[i] == 3 | data$month[i] == 4 | data$month[i] == 5){
    data$season[i] <- "Spring"
  }
}
#save data so far as predata
predata <- data

#get rid of bias of blanks = 16.9
data$MPadj <- data$AVGTOTAL-16.9


######Kentland analysis####
kentlandD <- data[c(which(data$site == "Kentland")),]

ggplot(kentlandD, aes(Turbidity_SW, AVGTOTAL))+
  geom_point()
K <- boxplot(kentlandD$AVGTOTAL)$out
x <-  kentlandD$Date[36]
kentlandD <- kentlandD[-c(which(kentlandD$AVGTOTAL %in% K)),]
kentlandD <- kentlandD[-c(which(kentlandD$Date == x)),]

ggplot(kentlandD, aes((Turbidity_SW), (AVGTOTAL), color = season))+ 
  geom_point(size = 3.5)+
  geom_smooth(
    method = "lm",            # Linear model
    formula = ((y)~x),
    se = TRUE,                   # Show confidence interval
    color = "black",          # Change line color
    linewidth = 1.5,                  # Line thickness
    linetype = "solid"           # Line type (solid, dashed, etc.)
  ) +
  labs(color = "Season")+
  ylab("MP particles per liter")+
  xlab("Turbidity (FNU)")+
  theme(legend.position="none")+
  scale_color_manual(values = c( "#FFB000", "#FE6100", "#DC267F", "#785EF0")) +
  facet_wrap(~(season))#, scale = "free")

#visualize mp and turb change through seasons
ggplot(kentlandD, aes(season, MPadj, color = season))+
  geom_violin(linewidth = 1)+
  #geom_jitter(height = 0, width = 0.1) +
  #geom_dotplot(binaxis='y', stackdir='center', position = 'dodge', dotsize=1)+
  #ggtitle("Spring difference")+
  scale_color_manual(values = c( "#FFB000", "#FE6100", "#DC267F", "#785EF0")) +
  theme(legend.position="none")+
  ylab("MP count per liter")


#get rid of mp counts that are under 0
kentadj <- kentlandD[-c(which(kentlandD$MPadj <= 0)),] #0.102 #log10(mpadj) 0.104 #log10(turb) 0.1475 #log-log 0.1484

ggplot(kentadj, aes(Turbidity_SW, MPadj))+
  geom_point()+
  facet_wrap(~(season), scales="free")

springK <- kentadj[which(kentadj$season == "Spring"),] #0.1778 #log10(mpadj) 0.1691 #log10(turb) 0.2297 #log-log 0.1887
summerK <- kentadj[which(kentadj$season == "Summer"),] #0.0110 #log10(mpadj) 0.0219 #log10(turb) 0.0525 #log-log 0.0741
fallK <-   kentadj[which(kentadj$season == "Fall"),]   #0.1782 #log10(mpadj) 0.2927 #log10(turb) 0.2037 #log-log 0.3386
winterK <- kentadj[which(kentadj$season == "Winter"),] #0.4798 #log10(mpadj) 0.5412 #log10(turb) 0.3204 #log-log 0.4526
####best
winterKent <- kentlandD[which(kentlandD$season == "Winter"),] #0.5548
####
fallKent <-   kentlandD[which(kentlandD$season == "Fall"),]   #0.1782
summerKent <- kentlandD[which(kentlandD$season == "Summer"),] #0.01099
springKent <- kentlandD[which(kentlandD$season == "Spring"),] #0.1801

#with avgtotal: #0.5548 #log10(avgtotal) 0.6198 #log10(turb) 0.4235 #log-log 0.5696
y <- log10(winterKt$AVGTOTAL)
x <- (winterKt$Turbidity_SW)
modelData <- data.frame(x,y)
plot(modelData)
model1 <- lm((y) ~ (x), data = modelData)
summary(model1)

#y = 1.27 + 0.04*log10(x)
#y = 14.413 + 4.352*x
#updated: without 2/14
#log10(y) = 1.248757 + 0.035326 * x
ggplot(winterKent, aes(Turbidity_SW, (AVGTOTAL)))+
  geom_point(size = 3.5)+
  geom_smooth(
    method = "lm",            # Linear model
    formula = ((y)~x),
    se = TRUE,                   # Show confidence interval
    color = "black",          # Change line color
    linewidth = 1.5,                  # Line thickness
    linetype = "solid"           # Line type (solid, dashed, etc.)
  ) +
  ylab("MP particles per liter")+
  xlab("Turbidity (FNU)")


########MP Proxy check#####
x <- seq(-1, 0.3, length.out = 100)
y <- 10^(1.273948 + 0.037497 * (16.183 +55.729 * x))
modelData <- data.frame(x,y)

ggplot(modelData, aes(x, (y)))+
  geom_point(size = 3.5)





############mp, turb, disch graph####

a<-ggplot(kentlandD, aes(time, MPadj))+#, color = name, shape = name))+ 
  geom_point( size = 4)+
  # geom_smooth(
  #   method = "lm",            # Linear model
  #   #formula = (log(y)~x),
  #   se = TRUE,                   # Show confidence interval
  #   color = "purple",          # Change line color
  #   linewidth = 1.5,                  # Line thickness
  #   linetype = "solid"           # Line type (solid, dashed, etc.)
  # ) +
  #scale_color_manual(values = c( "#CC6666", "#9999CC", "#66CC99", "black")) +
  #scale_color_manual(values = c( "#5fab2e", "lightblue")) +
  #labs(color = "Particle Shape", shape = "Particle Shape")+
  ylab("MP/L")+
  theme(
    axis.title.x = element_blank()  # Only remove the x-axis title
  )+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 1))  # Merge the legends
b <- ggplot(kentlandD, aes(time, Turbidity_SW))+
  geom_point( size = 4)+
  xlab("Date")+
  theme(
    axis.title.x = element_blank()  # Only remove the x-axis title
  )+
  ylab("Turbidity (FNU)")

c<- ggplot(Qdata, aes(Date, (FlowM)))+# color = month)) +  
  geom_line(size = 2)+
  ylab("Discharge (cms)")

a/b/c







