#---------------------------------------------
#
# Description: A Shiny web app to explore streamflow along Clear Creek
# in Golden,CO, and related weather and snowpack data.
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
library(DT)


#---------------------------------------------
# Read in snotel data (pre-downloaded to save time)
#---------------------------------------------

snotel_dat <- readRDS('data/LB_snotel.rds') 


#=================================================
# Define UI for application 
#=================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Clear Creek Stream Flow"),
  
  # Sidebar with a date range input
  sidebarLayout(
    sidebarPanel(
      dateInput("startdate",
                label = "Startdate",
                val=lubridate::ymd(paste(lubridate::year(Sys.Date())-2,"-01-01"))),
      dateInput("enddate",
                label = "Enddate",
                value=Sys.Date(),
                max = Sys.Date()),
      h5(paste('Most recent snotel data included:',as.character(max(snotel_dat$date))))
    ),
    
    # Set up mainPanel and tabs
    mainPanel(
      tabsetPanel(
        tabPanel('About',h3("Use this app to explore streamflow conditions on Clear Creek in Golden CO, as well as related snowpack and weather conditions from a snotel station near its source."),
                 h4("The time-series figure has 4 panels (*Note they are interactive so you can pan, zoom, select etc.. "),
                 h5("(1) A timeseries of streamflow on Clear Creek from USGS stations at Golden"),
                 h5("(2) Snow water equivalent (ie snowpack) at the Loveland Basin snotel site."),
                 h5("(3) Precipitation at the snotel site."),
                 h5("(4) Average temperature at the snotel site."),
                 h4("You can also view stream gauge data and camera at the",
                    a(href="https://waterdata.usgs.gov/monitoring-location/06719505/#parameterCode=00060&period=P7D&compare=true","USGS website")),
                 h4("Check City of Golden Clear Creek",
                    a(href="https://www.visitgolden.com/plan-your-visit/creek-info/","status")),
                 h5("Snotel data (snow water equivalent, precipitation, and temperature) is from a station at Loveland Basin (near Clear Creek source) and obtained using the 'snotelr' R package. Streamflow data is from USGS stream gauges along Clear Creek and obtained using the 'waterData' package. The map tab shows the location of each station."),
                 h4("Source code for this Shiny app is available on github at: ",
                    a(href="https://github.com/andypicke/GoldenStreamGauge","GoldenStreamGauge"))),
        tabPanel('Map of Stations',leafletOutput("map",width = '100%')),
        tabPanel('Time-series',plotlyOutput("tsPlot",width = '100%',height = 800)),
        #tabPanel('Yearly Comparison',plotlyOutput("yearly_comp"),width = '100%',height = 800),#plotlyOutput("sf_plot"),plotlyOutput("swe_plot")),
        tabPanel('Yearly Comparison',plotlyOutput("sf_plot"),plotlyOutput("swe_plot")),
        tabPanel('Data Table',DTOutput("dat_table") )
      )
    ) #mainPanel
  )#sidebarLayout
)#fluidPage





#=================================================
# Define server logic
#=================================================
server <- function(input, output) {
  
  
  #---------------------------------------------
  # Download stream gauge data
  #---------------------------------------------
  stat_code <- '00003' # code for daily mean
  var_code  <- '00060' # code for streamflow (discharge)
  
  # Golden stream gauge
  stream_dat_golden <- reactive({
    waterData::importDVs(staid = '06719505',
                         code = var_code,
                         stat=stat_code,
                         sdate = input$startdate,
                         edate = input$enddate) %>% 
      select(-staid) %>% 
      mutate(year=year(dates),
             month=month(dates),
             yday=yday(dates),
             name='Golden')
  })
  
  
  # combine stream gauge and snotel data into a single dataframe
  dat_comb <-  reactive({
    dplyr::left_join(stream_dat_golden(),select(snotel_dat,-c(year,yday)),
                     by=c("dates"="date"))
  })
  
  # Make main timeseries figure with 4 panels
  output$tsPlot <- renderPlotly({
    
    # Streamflow
    p1 <- dat_comb() %>% 
      plot_ly(x=~dates, y=~val) %>% 
      add_lines(data=dat_comb() %>% filter(name=='Golden'), name='Golden') %>% 
      add_lines(x=lubridate::ymd("2021-06-08"),y=range(dat_comb()$val,na.rm = TRUE),line=list(color="red",dash='dash'),name='Golden Creek Closed') %>% 
      add_lines(x=lubridate::ymd("2022-06-14"),y=range(dat_comb()$val,na.rm = TRUE),line=list(color="red",dash='dash'),name='Golden Creek Closed') %>% 
      add_lines(x=lubridate::ymd("2023-06-01"),y=range(dat_comb()$val,na.rm = TRUE),line=list(color="red",dash='dash'),name='Golden Creek Closed') %>% 
      add_lines(x=lubridate::ymd("2021-06-18"),y=range(dat_comb()$val,na.rm = TRUE),line=list(color="green",dash='dash'),name='Opened') %>% 
      add_lines(x=lubridate::ymd("2023-07-04"),y=range(dat_comb()$val,na.rm = TRUE),line=list(color="green",dash='dash'),name='Opened') %>% 
      layout(xaxis=list(title="Date"),
             yaxis=list(title="Streamflow [ft^3/s]"))
    
    # Snowpack
    p2 <- dat_comb() %>% 
      plot_ly(x=~dates,y=~snow_water_equivalent) %>% 
      add_lines(name='SWE',fill="tozeroy",color=I("Blue")) %>% 
      layout(xaxis=list(title="Date"),
             yaxis=list(title="Snow Water Equiv. [mm]"))
    
    # Precipitation
    p3 <- dat_comb() %>% 
      plot_ly(x=~dates,y=~precipitation) %>% 
      add_bars(name='Precip', color=I("Black")) %>% 
      layout(xaxis=list(title="Date"),
             yaxis=list(title="Precipitation [mm]"))
    
    # Temperature
    p4 <- dat_comb() %>% 
      plot_ly(type='scatter',x=~dates, y=~temperature_mean*(9/5)+32,name='Temp',color=I("Black")) %>% # Convert to fahrenheit
      layout(xaxis=list(title="Date"),
             yaxis=list(title="Mean Temp. [F]")) 
    
    p<- subplot(p1,p2,p3,p4,nrows = 4, shareX = TRUE, titleY = TRUE,margin = 0.05) %>% hide_legend()
    
    p
    
  }) # renderPlotly
  
  
  
  #---------------------------------------------
  # Plot comparing streamflow between years
  #---------------------------------------------
  
  #output$yearly_comp <- renderPlotly({
  
  output$sf_plot <- renderPlotly({
    
    sf <- dat_comb() %>% 
      filter(name=='Golden') %>% 
      plot_ly(x=~yday, y=~val) %>% 
      add_lines(color=~as.factor(year)) %>% 
      layout(xaxis=list(title="Yearday"),
             yaxis=list(title="Streamflow [ft^3/s]"),
             title="Streamflow at Golden During Different Years")
  }) # renderPlotly
  
  
  #---------------------------------------------
  # Plot comparing snowpack (SWE) between years
  #---------------------------------------------
  
  output$swe_plot <- renderPlotly({
    swe <- dat_comb() %>% 
      plot_ly(x=~yday,y=~snow_water_equivalent) %>% 
      add_lines(color=~as.factor(year)) %>% 
      layout(xaxis=list(title="Yearday"),
             yaxis=list(title="SWE"),
             title="Snowpack at Loveland During Different Years")
    
    #yearly_comp <- subplot(sf,swe,nrows = 2, shareX = TRUE, shareY = FALSE, titleY = TRUE,margin = 0.05)# %>% hide_legend()
    
  }) # renderPlotly
  
  
  
  
  #---------------------------------------------
  # Plot map of station locations using leaflet
  #---------------------------------------------
  
  m<-leaflet() %>%
    #    addProviderTiles(providers$OpenTopoMap) %>% 
    addTiles() %>%
    addMarkers(lng=-105.9,  lat=39.67,  popup="Loveland Basin Snotel Site") %>% 
    addMarkers(lng=-105.235,lat=39.753, popup="USGS Stream Gauge: Golden")
  
  output$map <- renderLeaflet(
    m
  )
  
  # data table
  output$dat_table <- renderDT({
    dat_comb() %>% 
      select(-c(name,qualcode,year,month,yday,temperature_min,temperature_max)) %>% 
      dplyr::rename(swe=snow_water_equivalent,
                    temp_mean = temperature_mean,
                    streamflow = val) %>% 
      datatable(rownames = FALSE,
                extensions = c("Responsive","Buttons"),
                options = list(
                  buttons = c("excel","csv","pdf"),
                  dom = "Bftip")
      )
  },
  server = FALSE)
  
  #---------------------------------------------
} # END Server function
#---------------------------------------------
#---------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

