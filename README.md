# Historical-MV-Death-Rate
This is a Shiny App that visualizes historical demographic differences in motor vehicle death rates. 

This app can be accessed at: https://monicaaa.shinyapps.io/historical-mv-death-rate-/

Description
------
This project visualizes long-term demographic differences in motor vehicle death rates for non-Hispanic blacks and whites in the US from 1934-2014. A similar graph was published in Monica King's dissertation: https://repository.upenn.edu/edissertations/2396/
 
Data source: 1934-1998 data came from the CDC HIST290 tables (https://www.cdc.gov/nchs/nvss/mortality/hist290.htm); 1999-2014 data were queried from CDC WONDER

Monica King cleaned, wrangled, and aggregated the data. 

Requirements
-------
R (preferably R Studio)

Dependencies
-------
- shiny
- ggplot2
- magrittr
- dplyr
- rsconnect


Using the App
-------
1) Open app.r in R Studio
2) Click "Run App" or run the code "runApp(<<APP_DIRECTORY>>)"
