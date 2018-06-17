#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load Libraries
library(shiny)
library(waterData)
library(snotelr)
library(lubridate)
library(ggplot2)
theme_set(theme_gray(base_size = 18))
library(dplyr)



#=================================================
# Define UI for application 
#=================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Stream Gauge Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateInput("startdate",label = "startdate",val='2018-01-01'),
      dateInput("enddate",label = "enddate",value=Sys.Date(),max = Sys.Date())
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("tsPlot",width = '100%')
    )
  )
)





#=================================================
# Define server logic
#=================================================
server <- function(input, output) {
  
  
  #---------------------------------------------
  # Download snotel data
  #---------------------------------------------
  
#  snotel <- snotelr::download_snotel(site_id = 602,internal = TRUE)
#  snotel_dat <- snotel[[1]] %>% 
#    select(-c(network,state,site_name,description,start,end,latitude,longitude,elev,county,site_id)) %>% #mutate(date = as.Date(date))
 snotel_dat <- readRDS('data/LB_snotel.rds') 
  
  
  #---------------------------------------------
  # Download stream gauge data
  #---------------------------------------------
  stat_code <- '00003' # daily mean
  var_code <- '00060' # streamflow (discharge)
  
  stream_dat <- reactive({
    importDVs(staid = '06719505',code = var_code, stat=stat_code,sdate = input$startdate, edate = input$enddate) %>% 
      mutate(year=year(dates)) %>%
      mutate(month=month(dates)) %>% 
      mutate(yday=yday(dates))
  })
  

  #---------------------------------------------
  # Join snotel and stream gauge data so we can plot them together easily
  #---------------------------------------------

  dat_comb <- reactive({
   inner_join(stream_dat(),snotel_dat,by=c("dates"="date"))
  })
  
    
  #---------------------------------------------
  # Plot stream flow
  #---------------------------------------------
  
  output$tsPlot <- renderPlot({
    p1 <- dat_comb() %>% ggplot(aes(dates,val))+
      geom_line(size=2)+
      ylab('Streamflow')+
      ggtitle("Timeseries at Loveland Basin Snotel and Clear Creek Stream Gauge",subtitle =  "Clear Creek Golden Station")

    p2 <- dat_comb() %>% ggplot(aes(dates,snow_water_equivalent))+
      geom_col()+
      ylab('Snow Water Equivalent')#+
      ggtitle("Timeseries of SWE",subtitle =  "Loveland Basin Snotel Site")
  
      p3 <- dat_comb() %>% ggplot(aes(dates,temperature_mean*(9/5)+32))+
        geom_line(size=2)+
        ylab('Mean Temperature [F]')##+
#      ggtitle("Timeseries of Av Temp",subtitle =  "Loveland Basin Snotel Site")
      
      p4 <- dat_comb() %>% ggplot(aes(dates,precipitation))+
        geom_col()+
        ylab('Precipitation')##+
      #      ggtitle("Timeseries of Av Temp",subtitle =  "Loveland Basin Snotel Site")
      
    gridExtra::grid.arrange(p1,p2,p4,p3,nrow=4)
      },height=800)
  
  
  
  
  #---------------------------------------------
} # END Server function
#---------------------------------------------
#---------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

