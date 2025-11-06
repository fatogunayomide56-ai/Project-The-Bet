
# Project 1: The Bet

# The “Bet” between Julian Simon and Paul Ehrlich, 1980
# During the late 70s business professor Julian Simon challenged biologist 
# Paul Ehrlich to choose any raw material he wanted and a date more than a year away.  
# The wager: the inflation-adjusted prices would decrease rather than increase. 
# The bet was formalized in 1980, with 1990 as the payoff date. 

#' Erlich chose a basket of copper, chromium, nickel, tin, and tungsten; 'twas
#' valued at $1,000 in 1980.

# 
#
#
# Calculate the real price of Tungsten by dividing the Nominal Price Series by 
# the Consumer Price Index for All Urban Consumers: All Items from FRED. ("CPIAUCSL")

# Generate a properly documented dygraph of the nominal price of tungsten together
# with the cpi.  

# Generate a properly documented dygraph of the real price of tungsten.  
# How did it perform over the decade (1980-1990) in question?  


setwd("C:/Users/Ayo/Dropbox/classes/ForecastingSpring25/Week1")
options(digits = 3, scipen = 99999)
remove(list = ls())
graphics.off()

#load libraries
suppressWarnings({
  suppressPackageStartupMessages({
    library(tidyverse)
    library(pdfetch)
    library(lubridate)  
    library(ggplot2)
    library(forecast)
    
    library(tsbox)
    library(tsibble)
    library(timetk)
    library(TSstudio)
    
    library(rio)
    #imports from websites and excel sheets
    library(readxl)
    library(tidyr)
    library(stringr)
    library(dygraphs)
    library(quantmod)
  })
})


      download.file(url = "https://pubs.usgs.gov/sir/2012/5188/tables/tungsten.xlsx", 
              destfile = "tungsten_3.xlsx")

#load tungsten  (here are 3 choices)
      library(readxl)
      tungsten_3 <- read_excel("tungsten_3.xlsx")
      View(tungsten_3)
    tungsten <- import("tungsten.xlsx", skip = 2)
    tungsten <- read_excel("tungsten.xlsx", skip = 2, col_types = c("numeric", "guess", "numeric"))
tungsten <- import("https://pubs.usgs.gov/sir/2012/5188/tables/tungsten.xlsx", skip = 2)

psych::headTail(tungsten)
View(tungsten)
tungsten[1:2] = NULL #removes 2 column that i dont need look at the file or excel 
str(tungsten)


tungsten = na.omit(tungsten)  # to remove the NAs
tungsten.ts <- ts(tungsten, start=c(1959), end=c(2010), frequency=1) #Convert to annual ts
dim(tungsten)
ts_info(tungsten.ts)
autoplot(tungsten.ts)



      getSymbols("CPIAUCSL", src = "FRED")  # download the consumer price index
      cpi = CPIAUCSL
      head(cpi)
      tail(cpi)
      str(cpi)

# convert to time series
cpi.ts = ts(cpi, start = c(1947,1), frequency = 12)  
    cpi.ts = tsbox::ts_ts(cpi)  # alternatively
ts_info(cpi.ts)
#convert to annual series and shorten
cpi.annual = window(cpi.ts, start = c(1959,1), end = c(2010, 12), frequency = 1)
dim(cpi.annual)
ts_info(cpi.annual)

# convert nominal to real by dividing nominal price by CPI and multiplying by 100
real_P_tungsten = tungsten.ts*100/cpi.annual

#Plot the two series (tungsten and the inflation adjusted price 
# of tungsten) with autoplot and autolayer
autoplot(tungsten.ts) + autolayer(real_P_tungsten)

# Plot the tungstern real price series 
autoplot(real_P_tungsten)

# Used dygraph to plot 

dygraph(real_P_tungsten)

# repeat the dygraph of real price of tungsten; identify the period of "the bet:"
# i.e. 1980-1990; add bells and whistles

dygraph(real_P_tungsten, main = "Real Price of Tungsten", 
        ylab = "Real Price, ") %>% 
  dyOptions(
    fillGraph=FALSE, 
    drawGrid = TRUE, 
    #colors=c("darkred", "blue", "black", "navyblue", "orange")
    ) %>%
  dyRangeSelector() %>%
  #dyCrosshair(direction = "vertical") %>%
  dyCrosshair(direction = "horizontal") %>%
  dyRoller(rollPeriod = 6, showRoller = TRUE) %>%
  dyShading(from = "1980-1-1", 
            to = "1990-12-31", 
            color = "lightpink") %>% 
  dyLegend(show = "follow") %>%
  #dySeries("BigfatIndex", 
  #         strokeWidth = 5, 
  #         strokePattern = "dashed") %>%
  dyHighlight(
    highlightCircleSize = 5, 
    highlightSeriesBackgroundAlpha = 0.5, 
    hideOnMouseOut = FALSE
  )  


#' Assemble a dygraph of the performance of coopper. 



#load copper  (here are 3 choices)
copper <- import("C:/Users/Fatogun/Downloads/copper (1).xlsx", skip = 2)

psych::headTail(copper)
View(copper)
copper[1:2] = NULL
str(copper)

# to remove the NAs
copper <- na.omit(copper)

copper.ts <- ts(copper, start=c(1959), end=c(2010), frequency=1) #Convert to annual ts
dim(copper)
ts_info(copper.ts)
autoplot(copper.ts)

      getSymbols("CPIAUCSL", src = "FRED")  # download the consumer price index
      cpi = CPIAUCSL
      head(cpi)
      tail(cpi)
      str(cpi)

# convert to time series
cpi.ts = ts(cpi, start = c(1947,1), frequency = 12)  
    cpi.ts = tsbox::ts_ts(cpi)  # alternatively
ts_info(cpi.ts)
#convert to annual series and shorten
cpi.annual = window(cpi.ts, start = c(1959,1), end = c(2010, 12), frequency = 1)
dim(cpi.annual)
ts_info(cpi.annual)

# convert nominal to real by dividing nominal price by CPI and multiplying by 100
real_P_copper = copper.ts*100/cpi.annual

#Plot the two series (tungsten and the inflation adjusted price 
# of tungsten) with autoplot and autolayer
autoplot(copper.ts) + autolayer(real_P_copper)

# Plot the tungstern real price series 
autoplot(real_P_copper)

# Used dygraph to plot 

dygraph(real_P_copper)

# dygraph of real price of copper; identify the period of "the bet:"
# i.e. 1980-1990; add bells and whistles

dygraph(real_P_copper, main = "Real Price of Copper", 
        ylab = "Real Price, ") %>% 
  dyOptions(
    fillGraph=FALSE, 
    drawGrid = TRUE, 
    #colors=c("darkred", "blue", "black", "navyblue", "orange")
  ) %>%
  dyRangeSelector() %>%
  #dyCrosshair(direction = "vertical") %>%
  dyCrosshair(direction = "horizontal") %>%
  dyRoller(rollPeriod = 6, showRoller = TRUE) %>%
  dyShading(from = "1980-1-1", 
            to = "1990-12-31", 
            color = "lightpink") %>% 
  dyLegend(show = "follow") %>%
  #dySeries("BigfatIndex", 
  #         strokeWidth = 5, 
  #         strokePattern = "dashed") %>%
  dyHighlight(
    highlightCircleSize = 5, 
    highlightSeriesBackgroundAlpha = 0.5, 
    hideOnMouseOut = FALSE
  )  

 # ===================== #

