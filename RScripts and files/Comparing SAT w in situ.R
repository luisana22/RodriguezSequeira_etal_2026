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
library(writexl)

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


winterKent <- read_xlsx("winterKent.xlsx")

# 
# MPproxyGEE <- read_xlsx("MPproxy_new.xlsx")
# 
# 
# #rename and clean columns
# names(MPproxyGEE) <- MPproxyGEE[1,]
# MPproxyGEE <- MPproxyGEE[-c(1),-c(1)]
# l <- length(MPproxyGEE)
# MPproxyGEE <- MPproxyGEE[,-c(l)]
# names(MPproxyGEE)[(l-3):(l-1)] <- c("SampleID", "Coordinates", "Date")
# 
# satD <- pivot_longer(MPproxyGEE, cols = -c("SampleID", "Coordinates", "Date")) ## %>% drop_na()
# colnames(satD) <- c("SampleID", "Coordinates", "Date", "SatDate", "MPproxy")
# 
# ##correct column type (dates to date and values to numbers)
# satD$SatDate <- as.character(satD$SatDate) %>% as.Date(format = "%Y%m%d") %>% format("%m/%d/%y")
# satD$Date <- as.Date(satD$Date, format = "%m/%d/%y")
# satD$SatDate <- as.Date(satD$SatDate, format = "%m/%d/%y")
# satD$MPproxy <- as.numeric(satD$MPproxy)
# 
# byD <- satD[which(satD$SatDate == satD$Date), ] %>% drop_na()
# 
# satINSITU <- full_join(byD, winterKent, by = c('SampleID' = 'Sample ID')) #%>% drop_na(SatDate)
# 
# 
# ####regression analysis####
# # turb = 16.578 + 55.729 *ndti 
# x<- satINSITU$MPproxy
# y<- (satINSITU$AVGTOTAL)
# 
# modelData <- data.frame(x,y)
# #plot(modelData)
# model1 <- lm(y ~ x, data = modelData)
# summary(model1)
# 
# ggplot(satINSITU, aes(MPproxy, (AVGTOTAL)))+ 
#   geom_point(size = 4)+
#   #geom_line(aes(x, line), color = "red", linewidth = 3)+
#   geom_smooth(
#     method = "lm",            # Linear model
#     formula = ((y)~x),
#     se = TRUE,                   # Show confidence interval
#     color = "black",          # Change line color
#     linewidth = 1.5,                  # Line thickness
#     linetype = "solid"           # Line type (solid, dashed, etc.)
#   )+
#   labs(color = "Season")+
#   xlab("MP proxy")+
#   ylab("In situ MP")
#  
# # n = 5; r2 = 0.559
# # in sity mp = 177.2824 + -0.8373 * mpproxy
# 
# write_xlsx(satINSITU, "/Users/pooirodriguez/Manuscript_Codes/RScripts and files/satINSITU.xlsx")
# 




MpwinterproxyBB <- read_xlsx("MP_timeseries_winter.xlsx") #at blacksburg points

names(MpwinterproxyBB) <- MpwinterproxyBB[1,]
MpwinterproxyBB <- MpwinterproxyBB[-1,]
MpwinterproxyBB$date <- as.Date(MpwinterproxyBB$date)
MpwinterproxyBB$MP <- as.numeric(MpwinterproxyBB$MP)

ggplot(MpwinterproxyBB, aes(date, MP))+
  geom_point()
wK <- winterKent[,c(4, 6)]
data <- full_join(MpwinterproxyBB, wK, by = c('date'= 'Date'))

df_long <- data %>%
  pivot_longer(
    cols = c(MP, AVGTOTAL),     # the two columns you want to plot
    names_to = "series",       # name for new “variable name” column
    values_to = "MP"        # name for new “value” column
  ) 

df_long <- mutate(df_long, date = as.Date(date, format = "%Y-%m-%d"))



ggplot(df_long, aes(x = date, y = MP, color = series)) +#, shape = highlight)) +
  geom_point(size = 3) +
  scale_color_manual(
    # manually set colors for each series
    values = c("MP" = "blue", "AVGTOTAL" = "lightgreen"),
    name   = "MP/L measurements",                          # legend title for colour
    labels = c("MP1" = "Satellite-derived", "AVGTOTAL" = "In situ")  # legend labels
  ) +
  scale_shape_manual(
    #name   = "",   # ← legend title
    values = c("normal" = 16, "star" = 8),
    guide = "none"
    #breaks = "star",
    #labels = "Included in Fig. 6"
  )+
  labs(
    x = "Date",
    y = "MP/L"
  )+
  theme(
    legend.text = element_text(size = 10),        # smaller legend labels
    legend.title = element_text(size = 10),       # smaller legend title
    legend.key.size = unit(0.5, "cm")            # smaller legend symbols / key boxes
  ) 


