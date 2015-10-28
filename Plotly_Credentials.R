install.packages("Rtools")
library(Rtools) 
library(devtools) 
devtools::install_github("ropensci/plotly")
library(plotly)
Sys.setenv("plotly_username"="JeffreyUslan")
Sys.setenv("plotly_api_key"="4txr51h2pd")
 
ggplotly(CCF_plot)
 
 