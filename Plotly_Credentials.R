 devtools::install_github("ropensci/plotly")
 Sys.setenv("plotly_username"="JeffreyUslan")
 Sys.setenv("plotly_api_key"="4txr51h2pd")
 library(plotly)
 ggplotly(CCF_plot)
 