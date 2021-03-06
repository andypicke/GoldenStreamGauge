#---------------------------------------------
#
# Description: A Shiny web app to explore streamflow along Clear Creek
# in Golden,CO, and related weather and snowpack data.s
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Author: Andy Pickering
#
#---------------------------------------------

# Load Libraries
library(shiny)
library(leaflet)
library(waterData)
library(lubridate)
library(dplyr)
library(plotly)

#=================================================
# Define UI for application 
#=================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Clear Creek Stream Flow"),
  
  # Sidebar with a date range input
  sidebarLayout(
    sidebarPanel(
      dateInput("startdate",label = "Startdate",val='2018-01-01'),
      dateInput("enddate",label = "Enddate",value=Sys.Date(),max = Sys.Date())
    ),
    
    # Set up mainPanel and tabs
    mainPanel(
      tabsetPanel(
        tabPanel('Time-series',plotlyOutput("tsPlot",width = '100%',height = 800)),
        tabPanel('Yearly Comparison',plotlyOutput("streamflow")),
        tabPanel('Map of Stations',leafletOutput("map",width = '100%')),
        tabPanel('About',h4("This app visualizes streamflow conditions along Clear Creek in Golden CO, as well as related snowpack and weather conditions, for a date range selected"),
                 h4("The main figure shows 4 plots. Note they are interactive so you can pan, zoom, select etc.. "),
                 h5("(1) A timeseries of streamflow on Clear Creek from USGS stations at Golden, and from further upstream at Lawson."),
                 h5("(2) Snow water equivalent (ie snowpack) at the Loveland Basin snotel site."),
                 h5("(3) Precipitation at the snotel site."),
                 h5("(4) Average temperature at the snotel site."),
                 h4("Snotel data (snow water equivalent, precipitation, and temperature) is from a station at Loveland Basin (near Clear Creek source) and obtained using the 'snotelr' R package. Streamflow data is from USGS stream gauges along Clear Creek and obtained using the 'waterData' package. The map tab shows the location of each station. Source code is available on github at: "),
                 a(href="https://github.com/andypicke/GoldenStreamGauge","GoldenStreamGauge"))
      )
    ) #mainPanel
  )#sidebarLayout
)#fluidPage





#=================================================
# Define server logic
#=================================================
server <- function(input, output) {
  
  #---------------------------------------------
  # Read in snotel data (pre-downloaded to save time)
  #---------------------------------------------
  
  snotel_dat <- reactive({
    readRDS('data/LB_snotel.rds') 
  })
  
  #---------------------------------------------
  # Download stream gauge data
  #---------------------------------------------
  stat_code <- '00003' # code for daily mean
  var_code  <- '00060' # code for streamflow (discharge)
  
  stream_dat_golden <- reactive({
    importDVs(staid = '06719505',code = var_code, stat=stat_code,sdate = input$startdate, edate = input$enddate) %>% 
      mutate(year=year(dates)) %>%
      mutate(month=month(dates)) %>% 
      mutate(yday=yday(dates))%>% 
      mutate(name='Golden')
  })
  
  stream_dat_lawson <- reactive({
    importDVs(staid = '06716500',code = var_code, stat=stat_code,sdate = input$startdate, edate = input$enddate) %>% 
      mutate(year=year(dates)) %>%
      mutate(month=month(dates)) %>% 
      mutate(yday=yday(dates)) %>% 
      mutate(name='Lawson')
  })
  
  stream_dat_both <- reactive({
    bind_rows(stream_dat_golden(),stream_dat_lawson())
  })
  
  # Make main figure with 4 panels
  output$tsPlot <- renderPlotly({
    
    # Streamflow
    p1 <- stream_dat_both() %>% 
      plot_ly(x=~dates, y=~val) %>% 
      add_lines(data=stream_dat_both() %>% filter(name=='Golden'), name='Golden') %>% 
      add_lines(data=stream_dat_both() %>% filter(name=='Lawson'), name='Lawson') %>% 
      layout(xaxis=list(title="Date"),
             yaxis=list(title="Streamflow [ft^3/s]"))
    
    # Snowpack
    p2 <- snotel_dat() %>% 
      filter(date>=min(stream_dat_both()$dates)) %>% 
      plot_ly(x=~date,y=~snow_water_equivalent) %>% 
      add_lines(name='SWE',fill="tozeroy",color=I("Blue")) %>% 
      layout(xaxis=list(title="Date"),
             yaxis=list(title="Snow Water Equiv. [mm]"))
    
    # Precipitation
    p3 <- snotel_dat() %>% 
      filter(date>=min(stream_dat_both()$dates)) %>% 
      plot_ly(x=~date,y=~precipitation) %>% 
      add_bars(name='Precip', color=I("Black")) %>% 
      layout(xaxis=list(title="Date"),
             yaxis=list(title="Precipitation [mm]"))
    
    # Temperature
    p4 <- snotel_dat() %>% 
      filter(date>=min(stream_dat_both()$dates)) %>% 
      plot_ly(x=~date, y=~temperature_mean*(9/5)+32) %>% # Conver to fahrenheit
      add_lines(name='Temp',color=I("Black")) %>% 
      layout(xaxis=list(title="Date"),
             yaxis=list(title="Mean Temp. [F]")) 
    
    p<- subplot(p1,p2,p3,p4,nrows = 4, shareX = TRUE, titleY = TRUE,margin = 0.05) %>% hide_legend()
    
    p
    
  }) # renderPlotly
  
  
  
  #---------------------------------------------
  # Plot comparing streamflow between years
  #---------------------------------------------
  
  output$streamflow <- renderPlotly({
    stream_dat_golden() %>% 
      plot_ly(x=~yday, y=~val) %>% 
      add_lines(color=~as.factor(year)) %>% 
      layout(xaxis=list(title="Yearday"),
             yaxis=list(title="Streamflow [ft^3/s]"),
             title="Streamflow at Golden During Different Years")
  }) # renderPlotly
  
  
  #---------------------------------------------
  # Plot map of station locations using leaflet
  #---------------------------------------------
  
  m<-leaflet() %>%
    addProviderTiles(providers$OpenTopoMap) %>% 
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=-105.9,  lat=39.67,  popup="Loveland Basin Snotel Site") %>% 
    addMarkers(lng=-105.235,lat=39.753, popup="USGS Stream Gauge: Golden")  %>% 
    addMarkers(lng=-105.62, lat=39.75,  popup="USGS Stream Gauge: Lawson")
  
  output$map <- renderLeaflet(
    m
  )
  
  
  #---------------------------------------------
} # END Server function
#---------------------------------------------
#---------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

