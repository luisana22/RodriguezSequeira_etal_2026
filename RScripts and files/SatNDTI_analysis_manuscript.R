library(ggplot2)
library(plotly)
library(readxl)
library(tidyverse)
library(stats)
theme_set(theme_classic()+
            theme(
              plot.title = element_text(size = 20),  # Title size
              axis.title.x = element_text(size = 20),                # X-axis title size
              axis.title.y = element_text(size = 20),                # Y-axis title size
              axis.text = element_text(size = 15),                   # Axis tick label size
              legend.title = element_text(size = 12),                 # Legend title size
              legend.text = element_text(size = 10)                   # Legend text size
            ))

setwd("/Users/pooirodriguez/Manuscript_Codes/RScripts and files")

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

#####Clean and organize sat data ####
#load

NDTIwo <- read_xlsx("NDTI_4manuscript.xlsx")
#rename and clean columns
names(NDTIwo) <- NDTIwo[1,]
NDTIwo <- NDTIwo[-c(1),-c(1)]
l <- length(NDTIwo)
NDTIwo <- NDTIwo[,-c(l)]
names(NDTIwo)[(l-3):(l-1)] <- c("SampleID", "Coordinates", "Date")

satD <- pivot_longer(NDTIwo, cols = -c("SampleID", "Coordinates", "Date")) ## %>% drop_na()
colnames(satD) <- c("SampleID", "Coordinates", "Date", "SatDate", "NDTI")

##correct column type (dates to date and values to numbers)
satD$SatDate <- as.character(satD$SatDate) %>% as.Date(format = "%Y%m%d") %>% format("%m/%d/%y")
satD$Date <- as.Date(satD$Date, format = "%m/%d/%y")
satD$SatDate <- as.Date(satD$SatDate, format = "%m/%d/%y")
satD$NDTI <- as.numeric(satD$NDTI)

byD <- satD[which(satD$SatDate == satD$Date), ] %>% drop_na()

compareBD <- full_join(byD, fieldD, by = 'SampleID') %>% drop_na(SatDate)

####regression analysis####
x<- compareBD$NDTI
y<- (compareBD$Turbidity_SW)

modelData <- data.frame(x,y)
plot(modelData)
model1 <- lm(y ~ x, data = modelData)
summary(model1)

modelData <- data.frame(x,y)
plot(modelData)
c1 <- min(y) * 0.5
model1 <- lm((y - c1) ~ x, data = modelData)
summary(model1)
startV <- list(a=exp(coef(model1)[1]), b=coef(model1)[2], c=c1)
model <- nls(y ~ a * exp(b * x) + c, data = modelData, start = startV)
summary(model)
p <- coef(model)
line <- p["a"] * exp(p["b"] * x) + p["c"]


ggplot(compareBD, aes(NDTI, (Turbidity_SW)))+ 
  geom_point(size = 4)+
  #geom_line(aes(x, line), color = "red", linewidth = 3)+
  geom_smooth(
    method = "lm",            # Linear model
    formula = ((y)~x),
    se = TRUE,                   # Show confidence interval
    color = "black",          # Change line color
    linewidth = 1.5,                  # Line thickness
    linetype = "solid"           # Line type (solid, dashed, etc.)
  )+
  labs(color = "Season")+
  xlab("NDTI")+
  ylab("in insitu Turbidity (FNU)")





