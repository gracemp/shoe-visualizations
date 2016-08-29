require(dplyr)
require(DT)
require(lubridate)
require(ggplot2)
require(ggthemes)
require(lazyeval)
require(stringr)
require(stringi)
require(shiny)
require(streamgraph)
require(scales)
require(rvest)
require(shinyBS)
require(sunburstR)
require(htmlwidgets)
require(RColorBrewer)
require(plotly)



shoeData <- readRDS("shoedata/stream-data.rds")
shoeData$Brand <- str_trim(shoeData$Brand)
shoeData$Dollars <- str_trim(shoeData$Dollars)
shoeData$Dollars <- as.numeric(shoeData$Dollars)
colnames(shoeData)[1] <- "Time.Period"
colnames(shoeData)[3] <- "Class"
shoeData$Time.Period <- as.Date(shoeData$Time.Period)
minDate <- min(shoeData$Time.Period)
maxDate <- max(shoeData$Time.Period)
shoeData$Brand <- stri_trans_general(shoeData$Brand, "az-title")
shoeData$Class <- as.character(shoeData$Class)
shoeData$Footwear.Style <- as.character(shoeData$Footwear.Style)
shoeData$Class[shoeData$Class == "Slippers"] <- " Slippers"
shoeData$Footwear.Style[shoeData$Footwear.Style == "Slippers"] <- "  Slippers"
shoeData$Footwear.Style[shoeData$Footwear.Style == "Not Specified"] <- "Unspecified"
shoeData$Footwear.Style[shoeData$Footwear.Style == "Outdoor Sandal"] <- " Outdoor Sandal"
shoeData$Footwear.Style[shoeData$Footwear.Style == "Water"] <- " Water"
shoeData$Footwear.Style[shoeData$Footwear.Style == "Sneaker"] <- " Sneaker"
colnames(shoeData)[10] <- "Price"

natData <- readRDS("shoedata/nat_data.rds")
natData$SUB_CLASS_DESCRIPTION <- str_trim(natData$SUB_CLASS_DESCRIPTION)
natData <- natData[natData$SUB_CLASS_DESCRIPTION %in% c('Tailored','Weekend', "Basic", "Detailed", "Dress",
                                                     "Other", "Open", "Cold Weather","Active", "Casual", 
                                                     "Bootie", "Oxfords", "Evening", "Slippers") , ]

rfmData <- readRDS("shoedata/RFM_SCORE.rds")
rfmData$group <- ifelse(rfmData$Score %in% c(3,4,5), "1",
                        ifelse(rfmData$Score %in% c(6,7,8), "2",
                               ifelse(rfmData$Score %in% c(9,10,11, 12), "3",
                                      ifelse(rfmData$Score %in% c(13,14,15), "4", ""))))
rfmData$Emailable <- ifelse(rfmData$Emailable == "Y", 1, 0)
                               
                               
shoeDataTemp <- shoeData %>% subset(Brand == "Naturalizer") %>% select(Item.Description, Footwear.Type, Footwear.Style)
shoeDataTemp <- shoeDataTemp[!duplicated(shoeDataTemp$Item.Description),]

nSumData <- natData %>%subset(TRANSACTION_DATE >= "2013-01-01")
nSumData$TRANSACTION_DATE <- floor_date(nSumData$TRANSACTION_DATE, "month")
nSumData <- nSumData %>% select (TRANSACTION_DATE, UNITS, DOLLARS, STYLE_SHORT_NAME) %>% group_by(TRANSACTION_DATE, STYLE_SHORT_NAME) %>%
  summarise(UPT = mean(UNITS), UNITS = sum(UNITS), AOV = mean(DOLLARS), DOLLARS = sum(DOLLARS))
names(nSumData)[names(nSumData) =="STYLE_SHORT_NAME"] <- "Item.Description"
summaryData <- merge(shoeDataTemp, nSumData, by = "Item.Description")
rm(shoeDataTemp, nSumData)
