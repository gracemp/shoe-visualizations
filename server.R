
library(shiny)
shinyServer(function(input, output, session) {
  streamFilter <- reactive({
    lc <- input$clicked_layer
    m <- input$measure
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    columns_selected <- c("Time.Period","Class","Footwear.Type", "Footwear.Style", "Brand", m)
    sData <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
    sData <- sData[c("Time.Period","Class","Footwear.Type", "Footwear.Style", "Brand", m)]
    sData <- subset(sData, Time.Period >= dateStart & Time.Period <= dateEnd)
    
    if (!is.null(lc) && lc %in% unique(shoeData$Footwear.Type)) {

      columns_selected <- c("Time.Period","Footwear.Type", "Class", "Footwear.Style", "Brand", m)
      sData <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
      sData <- sData[c("Time.Period","Footwear.Type", "Class", "Footwear.Style", "Brand", m)]
      sData <- subset(sData, Time.Period >= dateStart & Time.Period <= dateEnd)
      sData <- subset(sData, sData[2] == lc)
    }
    if (!is.null(lc) && lc %in% unique(shoeData$Class)) {

      columns_selected <- c("Time.Period","Class", "Footwear.Style", "Brand", m)
      sData <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
      sData <- sData[c("Time.Period","Class", "Footwear.Style", "Brand", m)]
      sData <- subset(sData, Time.Period >= dateStart & Time.Period <= dateEnd)
      sData <- subset(sData, sData[2] == lc)
    }
    if (!is.null(lc) && lc %in% unique(shoeData$Footwear.Style)) {

      columns_selected <- c("Time.Period","Footwear.Style", "Brand", m)
      sData <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
      sData <- sData[c("Time.Period","Footwear.Style", "Brand", m)]
      sData <- subset(sData, Time.Period >= dateStart & Time.Period <= dateEnd)
      sData <- subset(sData, sData[2] == lc)
    }
    cols_selected <- colnames(sData[, c(1, 3, ncol(sData))])
    sData <- subset(sData, select = colnames(sData) %in% cols_selected)
    dots <- sapply(cols_selected[1:2], . %>% {as.formula(paste0('~', .))})
    sData <- sData %>% group_by_(.dots = dots) %>% summarise_(Measure = interp(~sum(var, na.rm = TRUE), var = as.name(m)))
    sData$Measure <- ceiling(sData$Measure)
    colnames(sData)[2] <- "Drill"
    sData
  })
  
  # 
  output$mainPlot <- renderStreamgraph({
    sData <- streamFilter()

    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    validate(
      need(dateEnd-dateStart > 31, "Choose a larger date range")
    )
    validate(
      need(!is.null(sData[1,1]), " No data, try again")
    )
    # sData$Measure <- ifelse(sData$Measure == "Dollars", paste0("$", comma(sData$Measure)), comma(sData$Measure))
    streamgraph(data = sData, key = "Drill", value = "Measure" , date = "Time.Period") %>%
      sg_axis_x(tick_interval = 3, tick_units = "month", tick_format = "%b-%Y") %>%
      sg_axis_y(tick_count = element_blank())  %>% sg_fill_manual(values = wesanderson::wes_palette("GrandBudapest"))
  })

  
  # summary box

  output$vis <- renderText({
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
 
    paste(dateStart, "-", dateEnd)
  })
  output$hops<- renderUI({
    g <- input$drilldown
    tempData <- subset(shoeData, select = g)
    tempData <- unique(tempData[1])
    tempData <- tempData[-4,]
    selectInput('hops',  "Breakdown", choices = c( 'All',  tempData), selected = "All" )
  })
  output$totalSales <- renderDataTable({
    
    if(input$hops=="All"){
      columns_selected <- c("Brand", "Time.Period","Dollars", "Units")
      totalSum <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
      totalSum <- totalSum %>% mutate(Time.Period = format( Time.Period, "%Y"))%>% group_by (Time.Period) %>% 
        summarise( Units = sum(Units), Dollars = sum(Dollars))
      totalSum$Dollar.Growth <- (paste0( "%", round(((totalSum$Dollars - lag(totalSum$Dollars))/lag(totalSum$Dollars) *100), 2)))
      totalSum$Unit.Growth <- (paste0( "%", round(((totalSum$Units - lag(totalSum$Units))/lag(totalSum$Units) *100), 2)))
      
    }
    else {
      
      g <- input$drilldown
      columns_selected <- c("Brand", "Time.Period", "Dollars", "Units", g)
      totalSum <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
      colnames(totalSum)[colnames(totalSum) == g] <- "Breakdown"
      totalSum <- subset(totalSum,Breakdown == input$hops)
      totalSum <- totalSum %>% mutate(Time.Period = format( Time.Period, "%Y"))%>% group_by (Time.Period) %>% 
        summarise( Units = sum(Units), Dollars = sum(Dollars))
      totalSum$Dollar.Growth <- (paste0( "%", round(((totalSum$Dollars - lag(totalSum$Dollars))/lag(totalSum$Dollars) *100), 2)))
      totalSum$Unit.Growth <- (paste0( "%", round(((totalSum$Units - lag(totalSum$Units))/lag(totalSum$Units) *100), 2)))
    }
    totalSum$Dollars <- paste0( "$", comma(round(totalSum$Dollars/1000000000, 2)), " billion")
    totalSum$Units <- comma(totalSum$Units)
   totalSum <- t(totalSum)
      colnames(totalSum) <- totalSum[1, ]
    totalSum <- totalSum[-1, ]
    totalSum
    
  }, options= list(pageLength=1, filter=0, info=0, paging=FALSE, lengthChange= FALSE, ordering=FALSE), selection= 'none', rownames=TRUE)
  
  output$cSales <- renderDataTable({
    if (input$hops == "All"){
      columns_selected <- c("Brand", "Time.Period","Dollars", "Units")
      totalSum <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
      totalSum <- subset( totalSum, totalSum$Brand %in% c("Sam Edelman", "Fergie", "LifeStride", "Ryka", "Naturalizer",
                                                          "Via Spiga", "Dr. Scholl's", "Carlos", "Bzees", "Franco Sarto", "Vince", "Diane von Furstenberg"))
      totalSum <- totalSum %>% mutate(Time.Period = format( Time.Period, "%Y"))%>% group_by (Time.Period) %>% 
        summarise( Units = sum(Units), Dollars = sum(Dollars))
      totalSum$Dollar.Growth <- (paste0( "%", round(((totalSum$Dollars - lag(totalSum$Dollars))/lag(totalSum$Dollars) *100), 2)))
      totalSum$Unit.Growth <- (paste0( "%", round(((totalSum$Units - lag(totalSum$Units))/lag(totalSum$Units) *100), 2)))
    }
    else{
      
      g <- input$drilldown
      columns_selected <- c("Brand", "Time.Period", "Dollars", "Units",  g)
      totalSum <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
      colnames(totalSum)[colnames(totalSum) == g]<- "Breakdown"
      totalSum <- subset( totalSum, totalSum$Breakdown == input$hops)
      totalSum <- subset( totalSum, totalSum$Brand %in% c("Sam Edelman", "Fergie", "LifeStride", "Ryka", "Naturalizer",
                                                          "Via Spiga", "Dr. Scholl's", "Carlos", "Bzees", "Franco Sarto", "Vince", "Diane von Furstenberg"))
      validate(
        need(!is.na(totalSum[1,4]), "Category not applicable")
      )
      totalSum <- totalSum %>% mutate(Time.Period = format( Time.Period, "%Y"))%>% group_by (Time.Period) %>% 
        summarise( Units = sum(Units), Dollars = sum(Dollars))
      totalSum$Dollar.Growth <- (paste0( "%", round(((totalSum$Dollars - lag(totalSum$Dollars))/lag(totalSum$Dollars) *100), 2)))
      totalSum$Unit.Growth <- (paste0( "%", round(((totalSum$Units - lag(totalSum$Units))/lag(totalSum$Units) *100), 2)))
    }
    totalSum$Dollars <- paste0( "$", comma(round(totalSum$Dollars/1000000, 2)), " million")
    totalSum$Units <- comma(totalSum$Units)
    totalSum <- t(totalSum)
    colnames(totalSum) <- totalSum[1, ]
    totalSum <- totalSum[-1, ]
    totalSum
    
  },  options= list(pageLength=1, filter=0, info=0, paging=FALSE, lengthChange= FALSE, ordering=FALSE), selection= 'none', rownames=TRUE)
  

  # filter for Top Brands
  brandFilter <- reactive({
    g <- input$drilldown
    m <- input$measure
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    validate(
      need(dateEnd-dateStart > 31, "Choose a larger date range")
    )
    columns_selected <- c("Time.Period","Brand", m)
    brandData <- subset( shoeData, select = colnames(shoeData) %in% columns_selected)
    brandData <- subset(brandData, Time.Period >= dateStart & Time.Period <= dateEnd)
    brandData <- subset(brandData, !Brand %in% c("All Other Brands", "Private Label"))
    brandData <- brandData %>% group_by(Brand) %>% summarise_(Measure = interp(~sum(var, na.rm = TRUE), var = as.name(m)))
    brandData$Measure <- ceiling(brandData$Measure)
    head(arrange(brandData, desc(Measure)), n=25)
  })
  # filter for Top Items
  itemFilter <- reactive({
    g <- input$drilldown
    m <- input$measure
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    validate(
      need(dateEnd-dateStart > 0, "Choose a larger date range")
    )
    columns_selected <- c("Time.Period","Brand", "Item.Description", m)
    itemData <- subset( shoeData, select = colnames(shoeData) %in% columns_selected)
    itemData <- subset(itemData, Time.Period >= dateStart & Time.Period <= dateEnd)
    itemData <- subset(itemData, !Item.Description %in% c("All Other"))
    itemData <- itemData %>% group_by(Item.Description, Brand) %>% summarise_(Measure = interp(~sum(var, na.rm = TRUE), var = as.name(m)))
    itemData$Measure <- ceiling(itemData$Measure)
    head(arrange(itemData, desc(Measure)), n = 25)
  })
  # filter for fastest growing
  fastFilter <- reactive({
    x <- input$switch
    if(input$measure == "By Dollars")({
      met <- "Dollars"
    })
    else({
      met <-"Units"
    })
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    validate(
      need(dateEnd-dateStart > 31, "Choose a larger date range")
    )
    columns_selected <-c("Time.Period", "Brand", met )
    fastData <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
    fastData <- subset(fastData, Time.Period >= dateStart & Time.Period <= dateEnd)
    fastData <- fastData %>% group_by(Time.Period, Brand) %>% summarise_(Measure = interp(~sum(var, na.rm = TRUE), var = as.name(met)))
    fastData$m_share <- fastData$Measure / sum(fastData$Measure)
    fastData$quant <- dplyr::dense_rank(fastData$m_share)
    tt <- quantile(fastData$quant)[4]
    fastData <- fastData[fastData$quant >= tt[[1]],]
    list_of_brands <- list()
    for( i in unique(fastData$Brand)){
      tempData <- subset(fastData, Brand == i)
      tempData <- tempData[order(tempData$Time.Period),]
      tempData$Measure <- as.numeric(tempData$Measure)
      tempData <- tempData[tempData$Measure >0 , ]
      tempData$m_lag <- lag(tempData$Measure)
      tempData <- tempData[!is.na(tempData$Measure),]
      tempData <- tempData[!is.na(tempData$m_lag),]
      tempData$growth <- (tempData$Measure/ tempData$m_lag) -1
      temp_d <- data.frame(brand = i , growth = mean(tempData$growth, na.rm = T))
      list_of_brands <- rbind(list_of_brands, temp_d)
    }
    fastest_growing <- list_of_brands[!is.na(list_of_brands$growth),]
    fastest_growing
  })
  fastFilter2 <- reactive({
   if(input$growb == "Naturalizer"){
      fastData <- fastFilter()
      fastData <- subset( fastData, brand %in% c( "Naturalizer", "Cole Haan", "Clarks", "Aerosoles", "Easy Spirit", "Dansko"))
    }
     else if(input$growb == "Dr. Scholl's"){
       fastData <- fastFilter()
       fastData <- subset( fastData, brand %in% c( "Dr. Scholl's", "Aerosoles", "Born", "Nine West", "Steve Madden"))
     } 
      else if(input$growb == "Ryka"){
        fastData <- fastFilter()
        fastData <- subset( fastData, brand %in% c( "Ryka", "Aerosoles", "Skechers", "Nike", "Clarks"))
      }
    else if(input$growb == "Via Spiga"){
      fastData <- fastFilter()
      fastData <- subset( fastData, brand %in% c( "Via Spiga", "Franco Sarto", "Cole Haan", "Stuart Weitzman", "Ivanka Trump"))
    }
    
    else{
      fastData <- fastFilter()
    }
fastData
  })
  
  # output
  
  # top brands output
  output$leftDT <- DT::renderDataTable({
    fastData <- fastFilter()
    brandData <- brandFilter()
    brandData <- merge(brandData, fastData, by.x = "Brand", by.y = "brand")
    colnames(brandData) <- c( "Brand", input$measure, "Growth")
    brandData$Growth <- paste0(round(brandData$Growth*100,2), "%")
    if (input$measure == "Dollars"){
      brandData <- arrange(brandData, desc(Dollars))
      brandData$Dollars<-comma(brandData$Dollars)
      brandData$Dollars <- paste0( "$", brandData$Dollars)
    }
    else{
      brandData <- arrange(brandData, desc(Units))
      brandData$Units <- comma(brandData$Units)
    }
    brandData
  },
  options= list(pageLength=5, filter=0, info=0, paging=TRUE, lengthChange= FALSE, ordering=FALSE), selection= 'none',  rownames=FALSE)
  
  # top items output +clickable
  output$rightDT <- DT::renderDataTable({
    itemData <- itemFilter()
    colnames(itemData) <- c( "Item", "Brand", input$measure)
    if (input$measure == "Dollars"){
      itemData$Dollars <- comma(itemData$Dollars)
      itemData$Dollars <- paste0( "$", itemData$Dollars)
    }
    else{
      itemData$Units <- comma(itemData$Units)
    }
    itemData
  },
  options= list(pageLength=5, filter=0, info=0, paging=TRUE, lengthChange= FALSE, ordering=FALSE), selection= 'single', rownames=FALSE)
  
  # data for conditional panel-- top 10 items
  output$itemCondPanel <- renderUI({
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    ff <- input$rightDT_row_last_clicked
    if (!is.null(ff)){
      itemData <- itemFilter()
      colnames(itemData) <- c("Item", input$measure)
      if (input$measure == "Dollars"){
        itemData$Dollars <- comma(itemData$Dollars)
        itemData$Dollars <- paste0( "$", itemData$Dollars)
      }
      cols <-  subset(shoeData, Time.Period >= dateStart & Time.Period <= dateEnd)
      cols <- cols[(cols$Item.Description == as.character(ff)),]
      col1 <- cols[1, "Brand"]
      col2 <- cols[1, "Footwear.Style"]
      col3 <- mean(cols$Price)
      str1 <- paste(col1)
      str2 <- paste(col2)
      str3 <- paste( "$", round(col3, 2))
      url_prep <- URLencode(paste(ff, col1, "shoes"))
      url_to_scrape  <- paste0("http://www.bing.com/images/search?q=", url_prep)
      url_to_scrape2 <- paste0("http://www.zappos.com/", url_prep)
      shoe_image <- read_html(url_to_scrape2)
      s_image <- shoe_image %>% html_nodes(".product") %>% html_node('img') %>% html_attr("src")
      HTML(paste(str1, str2, str3, sep = '<br/>'))
    }
  })
  output$pic<-renderUI({
    ff <- input$rightDT_row_last_clicked
    if (!is.null(ff)){
      cols <-shoeData[(shoeData$Item.Description == as.character(ff)), ]
      col1 <- cols[1, "Brand"]
      url_prep <- URLencode(paste(ff, col1, "shoes"))
      url_to_scrape  <- paste0("http://www.bing.com/images/search?q=", url_prep)
      shoe_image <- read_html(url_to_scrape)
      s_image <- shoe_image %>% html_nodes("img") %>% html_attr("src")
      str3 <- str_trim(s_image[4])
      HTML(paste0('<img src="', str3 ,'"</img>'))
    }
  })
  output$condPanel <- renderUI({
  if (is.null(input$rightDT_row_last_clicked)){
    paste(("Click on a row for more information"))
  }
    else{
      HTML(paste0(htmlOutput('itemCondPanel'), htmlOutput('pic')))
      }
  })
  ##carousel data
  output$Tops1 <- renderPlot({
    fastData <- fastFilter2()
    fastData <- head(arrange(fastData, desc(growth)), n=5)
    top1 <- filter(shoeData, Brand == fastData[1, "brand"])
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    top1 <-  subset(top1, Time.Period >= dateStart & Time.Period <= dateEnd)
    top1 <-select(top1, Time.Period, Dollars) %>% group_by(Time.Period) %>% summarise(Dollars = sum(Dollars))
    fastData$growth <- paste0(round(fastData$growth * 100, digits = 2), "%")
    p<- ggplot(data = top1, aes(x = Time.Period, weight = Dollars, group = 1)) +
      xlab("Date") + ylab("Revenue") + geom_histogram(binwidth = 15, fill = '#353C4F') +
      theme_fivethirtyeight() + ggtitle(paste0("Brand: ", fastData[1, 1], "\n", "Growth Rate: ", fastData[1, 2])) +
      scale_y_continuous(labels = comma)
  p
  })
  output$Tops2 <- renderPlot({
    fastData <- fastFilter2()
    fastData <- head(arrange(fastData, desc(growth)), n=5)
    top1 <- filter(shoeData, Brand == fastData[2, "brand"])
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    top1 <-  subset(top1, Time.Period >= dateStart & Time.Period <= dateEnd)
    top1 <- select(top1, Time.Period, Dollars) %>% group_by(Time.Period) %>% summarise(Dollars = sum(Dollars))
    fastData$growth <- paste0(round(fastData$growth * 100, digits = 2), "%")
    p<- ggplot(data = top1, aes(x = Time.Period, weight = Dollars, group = 1)) +
      geom_histogram(binwidth = 15, fill = '#353C4F') + xlab("Date") + ylab("Revenue") +
      theme_fivethirtyeight() + ggtitle(paste0("Brand: ", fastData[2, 1], "\n", "Growth Rate:  ", fastData[2, 2])) + 
      scale_y_continuous(labels = comma)
    p
  })
  output$Tops3 <- renderPlot({
    fastData <- fastFilter2()
    fastData <- head(arrange(fastData, desc(growth)), n=5)
    top1 <- filter(shoeData, Brand == fastData[3, "brand"])
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    top1 <-  subset(top1, Time.Period >= dateStart & Time.Period <= dateEnd)
    top1 <- select(top1, Time.Period, Dollars) %>% group_by(Time.Period) %>% summarise(Dollars = sum(Dollars))
    fastData$growth <- paste0(round(fastData$growth * 100, digits = 2), "%")
    p<- ggplot(data = top1, aes(x = Time.Period, weight = Dollars, group = 1)) +
      geom_histogram(binwidth = 15, fill = '#353C4F') + xlab("Date") + ylab("Revenue") +
      theme_fivethirtyeight() + ggtitle(paste0("Brand: ", fastData[3, 1], "\n", "Growth Rate:  ", fastData[3, 2]))+
      scale_y_continuous(labels = comma)
    p
  })
  output$Tops4 <- renderPlot({
    fastData <-fastFilter2()
    fastData <- head(arrange(fastData, desc(growth)), n=5)
    top1 <- filter(shoeData, Brand == fastData[4, "brand"])
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    top1 <-  subset(top1, Time.Period >= dateStart & Time.Period <= dateEnd)
    top1 <- select(top1, Time.Period, Dollars) %>% group_by(Time.Period) %>% summarise( Dollars = sum(Dollars))
    fastData$growth <- paste0(round(fastData$growth * 100, digits = 2), "%")
    p<- ggplot(data = top1, aes(x = Time.Period, weight = Dollars, group = 1)) +
      geom_histogram(binwidth = 15, fill = '#353C4F') + xlab("Date") + ylab("Revenue") +
      theme_fivethirtyeight() + ggtitle(paste0("Brand: ", fastData[4, 1], "\n", "Growth Rate:  ", fastData[4, 2]))+
      scale_y_continuous(labels = comma)
    p
  })
  output$Tops5 <- renderPlot({
    fastData <- fastFilter2()
    fastData <- head(arrange(fastData, desc(growth)), n=5)
    top1 <- filter(shoeData, Brand == fastData[5, "brand"])
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    top1 <-  subset(top1, Time.Period >= dateStart & Time.Period <= dateEnd)
    top1 <- select(top1, Time.Period, Dollars) %>% group_by(Time.Period) %>% summarise( Dollars = sum(Dollars))
    fastData$growth <- paste0(round(fastData$growth * 100, digits = 2), "%")
    p<- ggplot(data = top1, aes(x = Time.Period, weight = Dollars, group = 1)) +
      geom_histogram(binwidth = 15, fill = '#353C4F') + xlab("Date") + ylab("Revenue") +
      theme_fivethirtyeight() + scale_y_continuous(labels = comma) + 
      ggtitle(paste0("Brand: ", fastData[5, 1], "\n", "Growth Rate: ", fastData[5, 2]))
   p
  })
  
  elFilter <- reactive({
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    columns_selected <-c("Time.Period", "Brand", "Units", "Price")
    eData <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
    eData <- subset(eData, Time.Period >= dateStart & Time.Period <= dateEnd)
    eData <- eData %>% group_by(Time.Period, Brand) %>% summarise(Units = sum(Units), Price = mean(Price))
    colnames(eData)[1] <- "Month"
    eData
  })
  
  eFilterTop <- reactive({
    eData <- elFilter()
    list_of_brands <- list()
    for(i in unique(eData$Brand)){
      tempData <- subset(eData, Brand == i)
      tempData <- tempData[order(tempData$Month), ]
      tempData <- tempData[tempData$Units > 0, ]
      tempData$u_lag <- lag(tempData$Units)
      tempData$unitChange <- ((tempData$Units - tempData$u_lag) / tempData$u_lag) - 1
      tempData <- tempData[tempData$Price > 0, ]
      tempData$p_lag <- lag(tempData$Price)
      tempData <- tempData[!is.na(tempData$p_lag), ]
      tempData$priceChange <- ((tempData$Price - tempData$p_lag) / tempData$p_lag) - 1
      tempData <- tempData[tempData$priceChange != 0, ]
      tempData$pE <- (tempData$unitChange/tempData$priceChange)
      temp_d <- data.frame(Brand = i , pE= mean(tempData$pE))
      list_of_brands <- rbind(list_of_brands, temp_d)
    }
    highestPE <- list_of_brands[!is.na(list_of_brands$pE), ]
    highestPE <- arrange(highestPE, desc(pE))
  })
  
  bMatch <- eventReactive(input$applyB, ignoreNULL = F, {
    stri_trans_general(input$bChoice, 'az-title')
  })
  
  output$stackPlot <- renderPlot({
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    validate(
        need(dateEnd-dateStart > 62, "Choose a larger date range")
        )
    eData <- elFilter()
    validate(
      need(bMatch() %in% eData$Brand, "Name not in data set-- try again")
      
    )
    tempData <- subset(eData, Brand == bMatch())
    range01 <- function(x){(x-min(x))/(max(x)-min(x))}
    tempData$Units <- range01(tempData$Units)
    tempData$Price <- range01(tempData$Price)
     ggplot(tempData, aes(Month)) + 
      geom_smooth(aes(y = Units, colour ='Units')) + 
      geom_smooth(aes(y = Price, colour = 'Price')) +
      ylab("")+ xlab("Time") + theme_fivethirtyeight() + 
       scale_color_manual(values = wesanderson::wes_palette("Moonrise2")) + theme(axis.text.y = element_blank())
    
    
  })
  output$eText <- renderText({
    eData <- eFilterTop()
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    validate(
      need(dateEnd-dateStart > 62, "")
    ) 
    validate(
      need(bMatch() %in% eData$Brand, "")
    )
    eData <- subset(eData, Brand == bMatch())
    paste("Price Elasticity:" , " " , round(eData[1, 2], 2), "--" ,
          ifelse((eData[1, 2] < -1 | eData[1, 2] > 1), paste("Relatively Elastic"), 
                 ifelse((eData[1, 2] > -1 & eData[1, 2] < 1), paste( "Relatively Inelastic"), 
                        ifelse((eData[1, 2] == -1 & eData[1, 2] == 1), paste( "Unit Elastic"), print("")))))
  })
  
    bubbleFilter <- reactive({
    dateStart <- input$daterange[1]
    dateEnd <- input$daterange[2]
    validate(
      need(dateEnd-dateStart > 0, "Choose a larger date range")
    )
    columns_selected <- c("Time.Period", "Brand", "Category", "Dollars", "Units", "Price")
    bData <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
    bData <- subset(bData, Time.Period >= dateStart & Time.Period <= dateEnd)
    ourBrands <- c("Naturalizer", "Ryka", "Lifestride", "Dr. Scholl's","Sam Edelman", "Fergie", 
                   "Via Spiga", "Franco Sarto")
    if(input$bubbleb == "Lifestyle"){
    bData <- subset(bData, Brand %in% c("Naturalizer", "Ryka", "Lifestride", "Dr. Scholl's",
                                        "Clarks", "Born", "Nine West", " Ak Anne Klein", "Aerosoles", "Sperry",
                                        "Skechers", "Cole Haan", "Easy Spirit", "Dansko"))
    bData$group <- ifelse(bData$Brand %in% ourBrands, 1, 2)
    }
    else{
      bData <- subset(bData, bData$Brand %in% c("Sam Edelman", "Fergie", 
                                          "Via Spiga", "Franco Sarto", 
                                          "Nine West","Vince Camuto", " Michael Michael Kors",
                                          "Ivanka Trump", "Coach", "Cole Haan", "Tory Burch", "Kate Spade",
                                          "Stuart Weitzman"))
      bData$group <- ifelse(bData$Brand %in% ourBrands, 1, 2)
    }
    
    bData <- bData %>% group_by(Brand, group) %>% summarise(Units = sum(Units), Dollars = sum(Dollars), Price = mean(Price))
    })
  output$bubblePlot <- renderPlot({
    bData <- bubbleFilter()
    colors <- c("DarkSalmon","LightBlue")
    p <- ggplot(bData, aes(Units, Price, label = Brand)) + 
      geom_point(aes( size = Dollars, colour = group), stat = "identity") + scale_color_gradient( low ="#FD6467", high = "#F1BB7B",guide = FALSE)+ 
      scale_size(range= c(5,30), labels = comma) + ggrepel::geom_text_repel(size = 5) + labs( x = "Units", y= "Average Price")+
      scale_x_continuous(labels = comma) 
    p
  })
 
  
  
  
  
  
  #====================== Naturalizer Tab ========================
  ### generally filter natData
  # 
  output$burst <- renderSunburst({
    sbData <- qFilter()
    m <- input$natMeasure
    sbData$MATERIAL_DESCRIPTION <- str_trim(sbData$MATERIAL_DESCRIPTION)
    sbData <- sbData%>%subset(!is.null(sbData$MATERIAL_DESCRIPTION))
    sbData$MATERIAL_DESCRIPTION <- str_replace_all(string=sbData$MATERIAL_DESCRIPTION, pattern = "-", repl = " ")
    sbData$SUB_CLASS_DESCRIPTION <- str_trim(sbData$SUB_CLASS_DESCRIPTION)
    sbData$COLOR_FAMILY <- str_to_title(str_trim(sbData$COLOR_FAMILY))
    sbData$MATERIAL_DESCRIPTION <- str_to_title(sbData$MATERIAL_DESCRIPTION)
    sbData$combined <- paste0(sbData$SUB_CLASS_DESCRIPTION, "-", sbData$COLOR_FAMILY, "-", sbData$MATERIAL_DESCRIPTION, "-", sbData$heel)
    columns_selected <- c('combined', m)
    sbData <- subset(sbData, select = colnames(sbData) %in% columns_selected)
    dots <- sapply(columns_selected[1], . %>% {as.formula(paste0('~', . ))})
    sbData<- sbData %>% group_by_(.dots = dots) %>% summarise_(Measure = interp(~sum(var, na.rm = TRUE), var = as.name(m)))
    sunburst(sbData, percent = TRUE,
             colors = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#46ACC8"))
  })
  
  
  # "function(d){ p <- cat('%',round((d.value/this*100), 2)) return  p + 'of items were in this category'}"
  natShoeFilter <- reactive({
    dateStart <- min(natData$TRANSACTION_DATE)
    dateEnd <- max(natData$TRANSACTION_DATE)
    m <- input$natMeasure
    columns_selected <- c("TRANSACTION_DATE", "STYLE_SHORT_NAME", "HEEL_HEIGHT", "MATERIAL_DESCRIPTION", "COLOR_FAMILY", m)
    tempData <- subset(natData, select = colnames(natData) %in% columns_selected)
    tempData
  })
  qFilter <- reactive({
    genData <- natData
    s <- input$seasonSelect
    m <- input$natMeasure
    columns_selected <- c("TRANSACTION_DATE", "STYLE_SHORT_NAME", "SUB_CLASS_DESCRIPTION", "HEEL_HEIGHT", "MATERIAL_DESCRIPTION", "COLOR_FAMILY", m)
    if (s == "winter"){
      months_selected <- c("1", "2", "3")
    } else if(s == "spring"){
      months_selected <- c("4", "5", "6")
    } else if(s == "summer"){
      months_selected <- c("7", "8", "9")
    } else if(s == "fall"){
      months_selected <- c("10", "11", "12")
    }
    genData$year <- year(as.POSIXlt(genData$TRANSACTION_DATE))
    genData$month <- month(as.POSIXlt(genData$TRANSACTION_DATE))
    genData <- genData %>% subset(genData$year == input$yearSelect)
    genData <- genData %>% filter(genData$month %in% months_selected)
    genData$heel <- ifelse(grepl("LOW", genData$HEEL_HEIGHT), "Low",
                           ifelse(grepl("MED", genData$HEEL_HEIGHT), "Medium",
                                  ifelse(grepl("HIGH", genData$HEEL_HEIGHT), "High", "Other")))
    genData
  })
  ## render a bar plot showing sales- static
  output$nShoePlot<- renderPlotly({
    dat <- qFilter()
    m <- input$natMeasure
    columns_selected <- c("TRANSACTION_DATE", m)
    dat$TRANSACTION_DATE <- floor_date(dat$TRANSACTION_DATE, "week")
    dots <- sapply(columns_selected[1], . %>% {as.formula(paste0('~', . ))})
    dat <- dat %>% group_by_(.dots = dots) %>% summarise_(Measure = interp(~sum(var, na.rm = TRUE), var = as.name(m)))
    dat$Measure <- ceiling(dat$Measure)
    gg <- dat %>% plot_ly(x = TRANSACTION_DATE, y = Measure, type = "bar", hoverinfo = "text", text = textOutput('xval')) %>% config(displayModeBar = F, scrollZoom=F, clickZoom = F)
     gg
  })
  
  output$xval<-renderPrint({
      #pull top items, to be selected from on click
      eventdata <- event_data("plotly_hover", source = "source")
      mon <- floor_date(as.Date(eventdata$x, origin ='1970-02-01'), unit = 'week')
      m <- input$natMeasure
      columns_selected <- c("TRANSACTION_DATE", "STYLE_SHORT_NAME", m)
      dots <- sapply(columns_selected[1:2], . %>% {as.formula(paste0('~', .))})
      nData <- natData %>% group_by_(.dots = dots) %>% summarise_(Measure = interp(~sum(var, na.rm = TRUE), var = as.name(m)))
      nData$Measure <- ceiling(nData$Measure)
      nData <- nData%>%arrange(TRANSACTION_DATE, -Measure)
      nData <- nData[!duplicated(nData$TRANSACTION_DATE), ]
       str1 <- HTML(as.character(nData[(as.character(nData$TRANSACTION_DATE) == as.character(mon)), ]))
       str2 <-HTML("The Top Selling Item for ", format(as.POSIXct(mon), format = "%B '%y"), "is the ",
           as.character(nData[(as.character(nData$TRANSACTION_DATE) == as.character(mon)), 2]))
       paste( str1, str2, sep = "/n")
  })
  imageFilter<-reactive({
    nData<-qFilter()
    m <- input$natMeasure
    columns_selected <- c("STYLE_SHORT_NAME", m)
    dots <- sapply(columns_selected[1], . %>% {as.formula(paste0('~', .))})
    nData <- nData %>% group_by_(.dots = dots) %>% summarise_(Measure = interp(~sum(var, na.rm = TRUE), var = as.name(m)))
    nData$Measure <- ceiling(nData$Measure)
    scrollImage <- head(arrange(nData, desc(Measure)), n=5)
    scrollImage
  })
  
  
  #==========carousel picture output================
  
 
  
  
  output$pic1 <- renderUI({
    scrollImage<-imageFilter()
    url_prep <- URLencode(paste(scrollImage[1,1],"Naturalizer", "shoes"))
    url_to_scrape  <- paste0("http://www.bing.com/images/search?q=", url_prep)
    shoe_image <- read_html(url_to_scrape)
    s_image <- shoe_image %>% html_nodes("img") %>% html_attr("src")
    str1 <- str_trim(s_image[4])
    url_prep <- URLencode(paste(scrollImage[2,1],"Naturalizer", "shoes"))
    url_to_scrape  <- paste0("http://www.bing.com/images/search?q=", url_prep)
    shoe_image <- read_html(url_to_scrape)
    s_image <- shoe_image %>% html_nodes("img") %>% html_attr("src")
    str2 <- str_trim(s_image[4])
    url_prep <- URLencode(paste(scrollImage[3,1],"Naturalizer", "shoes"))
    url_to_scrape  <- paste0("http://www.bing.com/images/search?q=", url_prep)
    shoe_image <- read_html(url_to_scrape)
    s_image <- shoe_image %>% html_nodes("img") %>% html_attr("src")
    str3 <- str_trim(s_image[4])
    HTML(paste0('<img height = 102px, width = 138px, onmouseover = paste(scrollImage[1,1]) src="', str1,'"</img>',
                '<img height = 102px, width = 138px src="', str2,'"</img>', 
                '<img height = 102px, width = 138px src="', str3,'"</img>'))
  })
  output$pic2 <- renderUI({
    scrollImage<-imageFilter()
    url_prep <- URLencode(paste(scrollImage[4,1],"Naturalizer", "shoes"))
    url_to_scrape  <- paste0("http://www.bing.com/images/search?q=", url_prep)
    shoe_image <- read_html(url_to_scrape)
    s_image <- shoe_image %>% html_nodes("img") %>% html_attr("src")
    str1 <- str_trim(s_image[3])
    url_prep <- URLencode(paste(scrollImage[5,1],"Naturalizer", "shoes"))
    url_to_scrape  <- paste0("http://www.bing.com/images/search?q=", url_prep)
    shoe_image <- read_html(url_to_scrape)
    s_image <- shoe_image %>% html_nodes("img") %>% html_attr("src")
    str2 <- str_trim(s_image[3])
    url_prep <- URLencode(paste(scrollImage[6,1],"Naturalizer", "shoes"))
    url_to_scrape  <- paste0("http://www.bing.com/images/search?q=", url_prep)
    shoe_image <- read_html(url_to_scrape)
    s_image <- shoe_image %>% html_nodes("img") %>% html_attr("src")
    str3 <- str_trim(s_image[4])
    HTML(paste0('<img height = 102px, width = 138px,  src="', str1,'"</img>',
                '<img height = 102px, width = 138px src="', str2,'"</img>', 
                '<img height = 102px, width = 138px src="', str3,'"</img>'))
  })
  output$ops2<- renderUI({
    g <- input$bdown
    tempData <- subset(summaryData, select = g)
    tempData <- unique(tempData[1])
    selectInput('ops2',  "", choices = c( 'All',  tempData), width = '100%', selected = "A")
  })
  


  output$natSummary <- renderDataTable({
    bd <- input$bdown
    if (input$ops2 == "All"){
      columns_selected <- c("Brand", "Time.Period","Dollars", "Units","Price")
      totalSum <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
      totalSum <- subset( totalSum, totalSum$Brand %in% c("Naturalizer"))
      totalSum <- totalSum %>% mutate(Time.Period = format( Time.Period, "%Y"))%>% group_by (Time.Period) %>% 
        summarise( Units = sum(Units), Dollars = sum(Dollars), Price = mean(Price))
      totalSum$Dollar.Growth <- (paste0( "%", round(((totalSum$Dollars - lag(totalSum$Dollars))/lag(totalSum$Dollars) *100), 2)))
      totalSum$Unit.Growth <- (paste0( "%", round(((totalSum$Units - lag(totalSum$Units))/lag(totalSum$Units) *100), 2)))
    }
    else{
      g <- input$ops2
      columns_selected <- c("Brand", "Time.Period", "Dollars", "Units", "Price",  bd)
      totalSum <- subset(shoeData, select = colnames(shoeData) %in% columns_selected)
      colnames(totalSum)[colnames(totalSum) == bd]<- "Breakdown"
      totalSum <- subset( totalSum, totalSum$Brand %in% c("Naturalizer"))
      totalSum <- subset(totalSum, totalSum$Breakdown ==g)
      totalSum <- totalSum %>% mutate(Time.Period = format( Time.Period, "%Y"))%>% group_by (Time.Period) %>% 
      summarise( Units = sum(Units), Dollars = sum(Dollars), Price = mean (Price))
      totalSum$Dollar.Growth <- (paste0( "%", round(((totalSum$Dollars - lag(totalSum$Dollars))/lag(totalSum$Dollars) *100), 2)))
      totalSum$Unit.Growth <- (paste0( "%", round(((totalSum$Units - lag(totalSum$Units))/lag(totalSum$Units) *100), 2)))
    }
    totalSum$Dollars <- paste0( "$", comma(round(totalSum$Dollars/1000000, 2)), " million")
    totalSum$Units <- comma(totalSum$Units)
    totalSum <- t(totalSum)
    colnames(totalSum) <- totalSum[1, ]
    totalSum <- totalSum[-1, ]
    totalSum
  }, options= list(pageLength=1, filter=0, info=0, paging=FALSE, lengthChange= FALSE, ordering=FALSE), selection= 'none', rownames=TRUE)


  
  output$rfmPlot4 <- renderPlot({
    colors <- c(rep("#A2A475", 10), rep("#02401B", 3))
     p <- ggplot(data = rfmData, aes(x = rfmData$Score)) + geom_histogram(bins = 12, binwidth = 1, fill = colors) +
       theme_fivethirtyeight()+ scale_y_continuous(labels = comma) + scale_x_continuous(breaks = unique(rfmData$Score)) +  xlab("Score") + ylab("Count") + ggtitle("Distribution of Scores")
     p
  })
  output$rfmPlot3 <- renderPlot({
    colors <- c(rep("#A2A475", 6), rep("#02401B", 4), rep("#A2A475", 3))
    p <- ggplot(data = rfmData, aes(x = rfmData$Score)) + geom_histogram(bins = 12, binwidth = 1, fill = colors) +
      theme_fivethirtyeight()+ scale_y_continuous(labels = comma) + scale_x_continuous(breaks = unique(rfmData$Score)) +  xlab("Score") + ylab("Count") + ggtitle("Distribution of Scores")
    p
  })
  output$rfmPlot2 <- renderPlot({
    colors <- c(rep("#A2A475", 3), rep("#02401B", 3), rep("#A2A475", 7))
    p <- ggplot(data = rfmData, aes(x = rfmData$Score)) + geom_histogram(bins = 12, binwidth = 1, fill = colors) +
      theme_fivethirtyeight()+ scale_y_continuous(labels = comma) + scale_x_continuous(breaks = unique(rfmData$Score)) +  xlab("Score") + ylab("Count")+ ggtitle("Distribution of Scores")
    
    p
  })
  output$rfmPlot1 <- renderPlot({
    colors <- c(rep("#02401B", 3), rep("#A2A475", 10))
    p <- ggplot(data = rfmData, aes(x = rfmData$Score)) + geom_histogram(bins = 12, binwidth = 1, fill = colors) +
      theme_fivethirtyeight()+ scale_y_continuous(labels = comma) + scale_x_continuous(breaks = unique(rfmData$Score)) +  xlab("Score") + ylab("Count") + ggtitle("Distribution of Scores")
    p
  })
  
  

  

  rfmData$group <- ifelse(rfmData$Score %in% c(3,4,5), "1",
                          ifelse(rfmData$Score %in% c(6,7,8), "2",
                                 ifelse(rfmData$Score %in% c(9,10,11, 12), "3",
                                        ifelse(rfmData$Score %in% c(13,14,15), "4", ""))))
  
  
  
  output$rfmDT <- renderTable({
    rdat <- rfmData
    rdat$Recency_in_Days <-rdat$Recency_in_Days*-1
    levels <- c(0, 180, 360, 720, 1800, Inf)
    labels <- c("past 6 months", "past year", "1-2 years", " 2-5 years", "over 5 years")
    rdat$Recency <- cut(rdat$Recency_in_Days, levels, labels)
   
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    rdat <- rdat %>% group_by(group)%>% summarise( Num.In.Category = n(), Last.Purchase = (Mode(Recency)), Avg.Orders = round(mean(Frequency), 2),
                                                     Total.Purchased = paste0("$", round(mean(Monetary), 2)), Num.In.Category = n(), Emailable = sum(Emailable)) %>% mutate ( Percent.Emailable = paste0( "%", round( 100*Emailable/Num.In.Category, 2)))
    rdat <- rdat[-1,]
    rdat <- cbind( c("3-5", "6-8", "9-12", "13-15"), rdat)
    colnames(rdat)[1] <- "Score"
    rdat <- rdat [-2]
    rdat <- rdat[-6]
    rdat <-rdat[-6]
    rdat[2] <- comma(rdat[2])
    colnames(rdat) <- c( "Score", "Number of People in Category", "Time of Last Purchase", "Number of Purchases", "Total Lifetime Dollars Purchased")
    rdat
})
  
  
  qFrame <- reactive({
    qData <- rfmData %>% group_by(group) %>% summarize(Recency_Score = round(mean(Recency_in_DaysQ), 2), Frequency_Score = round(mean(FrequencyQ), 2), Monetary_Score = round(mean(MonetaryQ), 2), Arr = mode(perm))
    qData <- qData[-1,]
  })
  output$s1 <- renderText({
    paste("Group 1 is our least active customers.\n They on average have spent less than $50 on our products,
           and have not shopped with us for several years.\n 
          Their average RFM score breakdown is (1.66, 1.01. 1.61)"
           )
  })
  output$s2 <- renderText({
    paste("Group 2 are less active customers.\n They on average have spent about $100 on our products,
           and have shopped with us most recently 2-5 years ago. \n 
          Their average RFM score breakdown is (3.18, 1.14. 2.60)"
    )
  })
  output$s3 <- renderText({
    paste("Group 3 fairly active customers.\n Their last purchase was several years ago, 
          but they have shopped with us several times and spent several hundred dollars.\n
           Their average RFM score breakdown is (3.45, 2.63. 4.17)"
    )
  })
  output$s4 <- renderText({
    "Group 4 is our most active customers.\n Their last purchase was in the last 6 months, and they 
           they have shopped with us a dozen times on average, spending $819 with us over their lifetime.\n
    Their average RFM score breakdown is (4.45, 4.55. 4.92)"
  })
  
  wFrame <- reactive ({
    wData <- rfmData
    wData$Recency_in_Days <- ifelse(wData$Recency_in_Days >=-180, 1, 0)
    wData$ords <- ifelse(wData$Frequency >5, 1, 0)
    wData$spend <- ifelse(wData$Monetary >=300, 1, 0)
    wData <- wData %>% group_by(group) %>% summarise( num = n(),  Num.Recent = sum(Recency_in_Days), Num.Ords = sum(ords), Num.Spend = sum(spend))
    wData <- wData[-1,]
    wData$Num.Recent <- round(wData$Num.Recent*100/wData$num)
    wData$Num.Ords <- round(wData$Num.Ords*100/wData$num)
    wData$Num.Spend <- round(wData$Num.Spend*100/wData$num)
    as.data.frame(wData)
  })
  
  output$w1 <-renderPlot({
    wData <- wFrame()
    n <- as.numeric(wData[1,3])
    parts <- c( "Did not"  = 100- n)
     a <-waffle::waffle(parts, rows =  5, size = 0.1, colors = c("#972D15"), title = "Percent Who Purchased in Last 6 Months")
     
     m <- as.numeric(wData[1,4])
     parts1 <- c( "Did not"  = 100- m)
     b <-waffle::waffle(parts1, rows =  5, size = 0.1, colors = c("#972D15"), title = "Percent Who Have Made Over 5 Orders")
     
     o <- as.numeric(wData[1,5])
     parts2 <- c( "Less"  = 100- o)
     c <-waffle::waffle(parts2, rows =  5, size = 0.1, colors = c("#972D15"), title = "Percent With Lifetime Spend > $300")
    
      waffle::iron(a, b, c)
})
  
  output$w2 <-renderPlot({
    wData <- wFrame()
    n <- as.numeric(wData[2,3])
    parts <- c("Purchased in last 6 months"  = n, "Did not"  = 100- n)
    a <-waffle::waffle(parts, rows =  5, size = 0.1, colors = c("#81A88D", "#972D15"), title = "Percent Who Purchased in Last 6 Months")
    
    m <- as.numeric(wData[2,4])
    parts1 <- c( "Did not"  = 100- m)
    b <-waffle::waffle(parts1, rows =  5, size = 0.1, colors = c("#972D15"), title = "Percent Who Have Made Over 5 Orders")
    
    o <- as.numeric(wData[2,5])
    parts2 <- c("Lifetime Spend >300"  = o, "Less"  = 100- o)
    c <-waffle::waffle(parts2, rows =  5, size = 0.1, colors = c("#81A88D", "#972D15"), title = "Percent With Lifetime Spend > $300")
    
    waffle::iron(a, b, c)
  })
  
  output$w3 <-renderPlot({
    wData <- wFrame()
    n <- as.numeric(wData[3,3])
    parts <- c("Purchased in last 6 months"  = n, "Did not"  = 100- n)
    a <-waffle::waffle(parts, rows =  5, size = 0.1, colors = c("#81A88D", "#972D15"), title = "Percent Who Purchased in Last 6 Months")
    
    m <- as.numeric(wData[3,4])
    parts1 <- c("Ordered more than 5 times"  = m, "Did not"  = 100- m)
    b <-waffle::waffle(parts1, rows =  5, size = 0.1, colors = c("#81A88D", "#972D15"), title = "Percent Who Have Made Over 5 Orders")
    
    o <- as.numeric(wData[3,5])
    parts2 <- c("Lifetime Spend >300"  = o, "Less"  = 100- o)
    c <-waffle::waffle(parts2, rows =  5, size = 0.1, colors = c("#81A88D", "#972D15"), title = "Percent With Lifetime Spend > $300")
    
    waffle::iron(a, b, c)
  })
  
  output$w4 <-renderPlot({
    wData <- wFrame()
    n <- as.numeric(wData[4,3])
    parts <- c("Purchased in last 6 months"  = 100-n, "Did not"  = n)
    a <-waffle::waffle(parts, rows =  5, size = 0.1, colors = c("#81A88D", "#972D15"), title = "Percent Who Purchased in Last 6 Months")
    
    m <- as.numeric(wData[4,4])
    parts1 <- c("Ordered more than 5 times"  = m, "Did not"  = 100- m)
    b <-waffle::waffle(parts1, rows =  5, size = 0.1, colors = c("#81A88D", "#972D15"), title = "Percent Who Have Made Over 5 Orders")
    
    o <- as.numeric(wData[4,5])
    parts2 <- c("Lifetime Spend >300"  = o, "Less"  = 100- o)
    c <-waffle::waffle(parts2, rows =  5, size = 0.1, colors = c("#81A88D", "#972D15"), title = "Percent With Lifetime Spend > $300")
    
    waffle::iron(a, b, c)
  })
  

  
})
