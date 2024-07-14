#
# Download snotel data for loveland station; used in shiny app.
# I pre-download to speed up response time of app.
#
# 2019/01/13 - Updated for new version of snotelr package
#

library(here)
library(snotelr)
library(dplyr)
library(purrr)


# make into function so we can use *safely*
get_snotel <- function() {
  dat_loveland <- snotelr::snotel_download(site_id = 602, internal = TRUE)
  return(dat_loveland)
}

get_snotel_safe <- safely(get_snotel)

err <- FALSE
while (err == FALSE) {
  out <- get_snotel_safe()
  print(out$error)
  err <- is.null(out$error)
}
dat_loveland <- out$result

df_loveland <- dat_loveland |>
  dplyr::select(-c(network, state, site_name, description, start, end, latitude, longitude, elev, county, site_id))

df_loveland$date <- as.Date(df_loveland$date)

df_loveland <- df_loveland |>
  mutate(
    year = lubridate::year(date),
    yday = lubridate::yday(date)
  )

saveRDS(df_loveland, 
        file = here("data","LB_snotel.rds")
        )

# df %>% ggplot(aes(yday,snow_water_equivalent,group=year))+geom_line(aes(col=as.factor(year)))+xlim(0,100)
