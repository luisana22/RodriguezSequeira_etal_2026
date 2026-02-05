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
setwd("/Users/pooirodriguez/Library/CloudStorage/OneDrive-VirginiaTech/Research/Data")
#setwd("C:/Users/lrs/OneDrive - Virginia Tech/Research/Data")

MPproxyGEE <- read_xlsx("MP proxy.xlsx")

names(MPproxyGEE) <- MPproxyGEE[1,]
MPproxyGEE <- MPproxyGEE[-1,-c(1,4)]
MPproxyGEE$date <- as.Date(MPproxyGEE$date)
MPproxyGEE$MP1 <- as.numeric(MPproxyGEE$MP1)

ggplot(MPproxyGEE, aes(date, MP1))+
  geom_point()
wK <- winterKent[,c(4, 6)]
data <- full_join(MPproxyGEE, wK, by = c('date'= 'Date'))

df_long <- data %>%
  pivot_longer(
    cols = c(MP1, AVGTOTAL),     # the two columns you want to plot
    names_to = "series",       # name for new “variable name” column
    values_to = "MP"        # name for new “value” column
  ) 

df_long <- mutate(df_long, date = as.Date(date, format = "%Y-%m-%d"))

df_long <- df_long %>% 
  mutate(
    highlight = if_else(
      series == "MP1" & date %in% highlight_dates, 
      "star", 
      "normal"
    )
  )

ggplot(df_long, aes(x = date, y = MP, color = series, shape = highlight)) +
  geom_point(size = 4) +
  scale_color_manual(
    # manually set colors for each series
    values = c("MP1" = "blue", "AVGTOTAL" = "lightgreen"),
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

