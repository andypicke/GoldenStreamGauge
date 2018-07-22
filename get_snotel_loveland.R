#
# Download snotel data for loveland station; used in shiny app. 
# I pre-download to speed up response time of app.
#
#
#

library(snotelr)
library(dplyr)
dat_loveland <- snotelr::download_snotel(site_id = 602,internal = TRUE)
df_loveland <- dat_loveland[[1]] %>% 
  select(-c(network,state,site_name,description,start,end,latitude,longitude,elev,county,site_id))
df_loveland$date <- as.Date(df_loveland$date)
saveRDS(df_loveland,file='data/LB_snotel.rds')


