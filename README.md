# GoldenStreamGauge
This repository contains the code for [GoldenStreamGauge](https://andypicke.shinyapps.io/GoldenStreamGauge/), an app built with R Shiny and hosted on shinyapps.io . It visualizes stream flow in Clear Creek near Golden, as well as snowpack and weather conditions from a snotel station at Loveland Pass. 

* The initial version of the app was described in a [Blog Post](https://andypicke.github.io/GoldenStreamGauge/) on my website.

# App Structure

On the left sidebar, you can select a start and end date to examine. The default range is January 1 of two years ago. The date range selected is applied to all sections of the app.

There are 4 tab panels:

1)  *Time-series* shows timeseries of:
* a) Streamflow measured at Clear Creek in Golden
* b) Snow-water equivalent at the Loveland basin snotel site.
* c) Precipiation at the Loveland basin snotel site.
* d) Mean temperature at the Loveland basin snotel site.

2) *About*

3) *Map of Stations*

The second tab shows a map (made with leaflet) of the locations of the snotel site and the two stream gauge stations.

4) *Yearly Comparison*

Plots a timeseries of stream flow and snowpack for each separate year on top of each other, allowing for comparison of different years. Data is plotted versus yearday instead of date to provide a common x-axis.

5) *Data Table

Displays data in an interactive DT table that can filtered and sorted. Also provides links to download data. Note the data displayed is for the date range chosen in the left input.

# Data Sources

### Weather Data
Weather data is from the Loveland basin snotel site.

### Stream Data
Stream flow data is from two USGS stream gauge stations along Clear Creek.s
