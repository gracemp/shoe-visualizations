# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("carouselPanel.R")
source("carouselPanel2.R")
library(shiny)

shinyUI(
  fluidPage(theme="styles.css",
            tags$head(tags$script(src= "particles.js")),
            navbarPage("Shoe Timeline",
                       tabPanel("TREND", value="TREND",
        
                                 tags$div(id = 'selectionTop',
                                                   conditionalPanel(condition = "input.showhide",
                                                   dateRangeInput("daterange", "", start = minDate, end = maxDate , min = minDate, max = maxDate, format = "mm/yyyy",
                                                                  startview = "month", separator = " to "),
                                                   selectInput('measure', label = "", choices = c("Dollars","Units")))
                                                  
                                            ),
                                tags$div(id = 'selectionCheck',
                                wellPanel( id = 'check',
                                           checkboxInput("showhide", label = "", TRUE,width = '200px'),
                                           paste("hide"))),
                                fluidPage(id = "trend_panel",

                                          wellPanel(h2("Industry Trends"),
                                                    h6("click to break down"),
                                                      streamgraphOutput(outputId = "mainPlot"),
                                                    id = "streamPanel"),
                                          
                                          wellPanel(
                                            fluidRow(
                                              column(3,
                                            h3("Market Summary",
                                               bsButton("qM", label = "", icon = icon("question"), style = 'info', size = "extra-small")),
                                            bsPopover(id = "qM", title = "Sales for this time period by category for the industry as a whole and Caleres wholesale brands.", placement = 'right', 
                                                      content = "", trigger = 'click')),
                                            column(2, offset = 4,
                                            selectInput('drilldown', label = "Select Drilldown:", choices = c("Footwear Type" = "Footwear.Type", "Footwear Style" = "Footwear.Style", "Class" = "Class"), width = "100%")),
                                            column(2, 
                                            uiOutput('hops', style = "test-style"))),
                                            fluidRow(
                                              column(4, offset= 2,
                                                     h6("Total Sales")
                                              ),
                                              column(4, offset = 2,
                                                     h6("Caleres Brand Sales")
                                              )),
                                            fluidRow(
                                              column(6,
                                            dataTableOutput("totalSales")), 
                                            column(6,
                                            dataTableOutput("cSales"))),  id = 'mktSumPanel'),
                                          
                                          wellPanel(id = "growPanel",
                                                    fluidRow(
                                                      column(6,
                                                   h3("Fastest Growing Brands",
                                                      bsButton("q1", label = "", icon = icon("question"), style = 'info', size = "extra-small"))
                                                   ),
                                                   bsPopover("q1", title = "All selects top 5 fastest growing brands for this period who had a market share of over 0.1% on average. Select a brand to view it and its competitors. Graph shows sales over time.",  content = "", trigger = 'click'),
                                                   column(4, offset = 2,
                                                          selectInput("growb", "Select Brand", choices = c( "All", "Naturalizer", "Dr. Scholl's", "Ryka", "Via Spiga"))
                                                          )),
                                                   carouselPanel(auto.advance =F, plotOutput("Tops1", height = '450px'), plotOutput("Tops2", height = '450px'), 
                                                                 plotOutput("Tops3", height = '450px'), plotOutput("Tops4", height = '450px'),
                                                                 plotOutput("Tops5", height = '450px'))

                                          ),
                                          
                                          wellPanel(
                                            h3("Top Brands",
                                               bsButton("q2", label = "", icon = icon("question"), style = 'info', size = "extra-small")),
                                            bsPopover(id = "q2", title = "Top brands for the selected time period by sales in units or dollars. Excludes high-performance shoes", placement = 'right', 
                                                      content = "", trigger = 'click'),
                                            DT::dataTableOutput('leftDT'),
                                            id = 'top_panel1'),
                                         
                                           wellPanel(
                                            h3("Top Items", bsButton("q3", label = "", icon = icon("question"), style = 'info', size = "extra-small")),
                                            DT::dataTableOutput('rightDT'), target = 'cell',id = 'top_panel2'),
                                          bsPopover(id ="q3", title = "Top items for the selected time period by sales in units or dollars.Excludes high-performance shoes. Click for images.", placement = 'right', 
                                                    content= "", trigger= 'click'),
                                          wellPanel(id = "basePanel",
                                                    htmlOutput('condPanel')),
                                          
                                          wellPanel(id = 'bPanel',
                                                    textInput( 'bChoice', "Enter Brand", value = 'Naturalizer'),
                                                    actionButton(inputId = 'applyB', label = "", icon = "GO"),
                                                    textOutput('eText'),
                                                    plotOutput('stackPlot', height = "200px")),
                                          
                                       
                                          
                                          
                                          
                                          wellPanel(id = "bubblePanel",
                                                    fluidRow(
                                                      column(6,
                                                    h3("Caleres Brands and Competitors")),
                                                    column(4, offset = 2,
                                                       selectInput("bubbleb", "Select Group", choices = c("Lifestyle", "Fashion")))),
                                                    plotOutput('bubblePlot'))
                                          
                                          
                                          
                                )
                       ),
                       tabPanel("NATURALIZER", value="NATURALIZER",
                                fluidPage(id = "nat_panel",
                                          tags$img(src = "nat_logo.png", style = "float: right;"),
                                          
                                          fluidRow(
                                            column(5, offset = 1,
                                                   selectInput(inputId = 'natMeasure', label = "Measure", choices = c( "By Dollars" = "DOLLARS", "By Units"= "UNITS"))
                                            ),
                                            column(5, offset = 1,
                                                   fluidRow(
                                                     column(6,
                                                            selectInput(inputId = 'yearSelect', label = "Select Year", choices = c("2012", "2013", 
                                                                                                                                   "2014", "2015", "2016"))
                                                     ),
                                                     column(5,
                                                            selectInput(inputId = 'seasonSelect', label = " Select Season", choices = c("Winter (Q1)" = "winter",
                                                                                                                                        "Spring (Q2)" = "spring", 
                                                                                                                                        "Summer (Q3)" = "summer",
                                                                                                                                        "Fall (Q4)"   = "fall" )))))),
                                          
                                          wellPanel(
                                            h4("Product Breakdown",
                                            bsButton("qBurst", label = "", icon = icon("question"), style = 'info', size = "extra-small")),
                                              sunburstOutput('burst', height="600px"),
                                            wellPanel( id = "textburst",
                                            paste("Hover to see the relative breakdowns of Naturalizer stock by subclass, color, material, and heel height.
                                                  Each ring is a pie chart, adding one more breakdown of the product. See the legend above for what information
                                                  is being shown.")),
                                            id = 'burstPanel'),
                                          bsPopover(id = "qBurst", title = "Choose a time frame. Flow of shoe components is subclass->color->material->heel height.", placement = 'right', 
                                                    content = "", trigger = 'click'),
                                          
                                          
                                          wellPanel(
                                            h4("Sales and Top Items per Quarter",
                                            bsButton("nShoeq", label = "", icon = icon("question"), style = 'info', size = "extra-small")),
                                            plotly::plotlyOutput('nShoePlot'),
                                            bsPopover(id = 'nShoeq', title = "Graph of total Naturalizer sales for the quarter selected.Click on the plot to get top shoe of the month.", 
                                                      placement= "right", content = "", trigger = 'click'),
                                            wellPanel(
                                              h5("Top Shoes of the Quarter"),
                                              carouselPanel2(auto.advance =F, htmlOutput('pic1'), htmlOutput('pic2'))
                                            ),
                                            id = 'natPlotPanel'),
                                          
                                          
                                          
                                          
                                          wellPanel( id = 'summaryPanel',
                                                     h5("Naturalizer Summary",
                                                        bsButton("natq", label = "", icon = icon("question"), style = 'info', size = "extra-small")),
                                                     bsPopover(id = 'natq', title = "Total sales for each year, with cumulative growth metrics. Drilldown for metrics in specific categories.", 
                                                               placement= "right", content = "", trigger = 'click'),
                                                     fluidRow(
                                                       column(6,
                                                              selectInput('bdown', label = "Select Drilldown:", choices = c("Footwear Type" = "Footwear.Type", "Footwear Style"= "Footwear.Style"), width= '100%')
                                                       ),
                                                       column(6,
                                                              uiOutput('ops2')
                                                              )
                                                     ),
                                                     fluidRow(
                                                     dataTableOutput('natSummary')
                                                     )     
                                          ),
                                          
                                          wellPanel(id = "personas",
                                                    h4("Profiles"),
                                                    h5('This analysis mathematically segmented our customer base based on 3 key characteristics:
                                                        Recency (how long ago was their last purchase?), Frequency ( how many times have they purchased?) 
                                                       and Monetary (How much do they spend?)'),
                                                    paste("These customers were given a score between 1-5 for each category, with 1 being the least active 
                                                       and 5 being the most active in each of these categories. Breakdown of our customers based on this aggregate scale (3-15) below. "),
          
                                                    wellPanel( id= "general",
                                                               tableOutput("rfmDT")
                                                               ),

                                                    carouselPanel(
                                                                  wellPanel(column(4, offset = 4, h4("Score: 13-15")),column(6,plotOutput("w4", width = '100%')), column(6,textOutput('s4'), plotOutput("rfmPlot4", height = "300px"))),
                                                                  wellPanel(column(4, offset = 4, h4("Score: 9-12")),column(6,plotOutput("w3", width = '100%')), column(6,textOutput('s3'), plotOutput("rfmPlot3",  height = "300px"))),
                                                                  wellPanel(column(4, offset = 4, h4("Score: 6-8")),column(6,plotOutput("w2", width = '100%')), column(6,textOutput('s2'), plotOutput("rfmPlot2",  height = "300px"))),
                                                                  wellPanel(column(4, offset = 4, h4("Score: 3-5")),column(6,plotOutput("w1", width = '100%')), column(6,textOutput('s1'), plotOutput("rfmPlot1",  height = "300px")))
                                                                  )
                                                               
                                                  
                                        
                                                  
                                 
                                          )
                                ),
                                tags$script("particlesJS.load('trend_panel', 'particlesjs-config.json')"),
                                tags$script("particlesJS.load('nat_panel', 'particlesjs-config.json')")

                                
                       )
                       
                       
                       
            )
  )
)

