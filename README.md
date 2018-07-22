# GoldenStreamGauge
[GoldenStreamGauge](https://andypicke.shinyapps.io/GoldenStreamGauge/) is an app built with R Shiny and hosted on shinyapps.io . It visualizes stream flow and weather conditions along Clear Creek in Golden, CO. This repository contains the code for the app.

* The initial version of the app was described in a [Blog Post](https://andypicke.github.io/GoldenStreamGauge/) on my website.

## Using the app

The app is simple to use. On the left sidebar, you can select a start and end date to examine. The main tab ('time-series') shows timeseries of:
* a) Streamflow at 2 stream gauges along Clear Creek
* b) Snow-water equivalent at the Loveland basin snotel site.
* c) Precipiation at the Loveland basin snotel site.
* d) Mean temperature at the Loveland basin snotel site.

The second tab shows a map (made with leaflet) of the locations of the snotel site and the two stream gauge stations.

## Data Sources

### Weather Data
Weather data is from the Loveland basin snotel site.

### Stream Data
Stream flow data is from two USGS stream gauge stations along Clear Creek.s
