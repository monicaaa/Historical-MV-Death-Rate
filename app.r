# use install.packages("PackageName") to install all packages below
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(rsconnect)

# Define UI logic ----

ui <- fluidPage(
  
  titlePanel("Historical Demographic Trends"),
  
  fluidPage(
    
    # Defines sidebar panel selections
    sidebarPanel(
      
      # Makes slider range for years
      sliderInput("slider", label = h3("Year Range"), min = 1934, 
                  max = 2014, value = c(1934, 2014), sep = ""),

  
      # Makes radio buttons for sex
      radioButtons("radio", label = h3("Select Sex"),
                   choices = list("Men" = "Male", "Women" = "Female"), 
                   selected = "Male"),
      
      # Makes checkboxes for age groups
      checkboxGroupInput("age", "Age Groups:",  
                  c("Adjusted" = "adjusted",
                    "Ages 5-14" = "age05.14",
                    "Ages 15-24" = "age15.24",
                    "Ages 25-44" = "age25.44",
                    "Ages 45-64" = "age45.64",
                    "Ages 65-84" = "age65.84"),
                  selected = c("adjusted")),
      
      # Makes checkbox for whether the line is smoothed
      checkboxInput("smooth", "3-Year Moving Average", TRUE)
    
    ),
    
    # Defines main panel layouts
    mainPanel(
      h3(textOutput("selected_var")), # graph title based on sex selection, defined in output
      p("Positive = black population experience higher MV death rates compared to white population",
        style = "font-family: 'times'; font-si16pt"),
      plotOutput("mv_plot"), #graph defined below
      h6("Source: Historical CDC Vital Statistics Data"),
      h6("Analysis and app built by Monica M. King")
      
    )
)

)
# reads in cleaned data from AWS
mv_data <- read.csv(url("https://s3.amazonaws.com/shiny-app-data/absolute_differences_historical_bw.csv")) %>%
  mutate(bw_asdr_perdiff = (bw_asdr-1)*100, # defined var for % difference instead of ratio 
         bw_asdr_perdiff.s = (bw_asdr.s-1)*100)

# sets the break of the x-axis label based on the selected year range
# @param: min_value is an integer
# @param: max_value is an integer
# @return is an integer

def_break_length <- function(min_value, max_value){
  total_range <- max_value-min_value
  if (total_range > 40){
    return(10)
  } else if (total_range %in% (15:40)){
    return(5)
  } else{
    return(1)
  }
}

# Define server logic
server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste0("Black-White Differences in Motor Vehicle Death Rates for ", input$radio, "s, 1934-2014")
  })

  output$mv_plot <- renderPlot({mv_data %>% 
    filter(age.cat %in% input$age,
           gender == input$radio,
           year %in% (input$slider[1]:input$slider[2])) %>%
      ggplot(aes(x= year, 
                 y= if(input$smooth){ # whether y is smoothed or not
                   bw_asdr_perdiff.s
                 }else{
                   bw_asdr_perdiff
                 }, 
                 group = age.cat, colour = age.cat)) +
      scale_colour_manual(name = "Age Groups",
                          values = c("#000000", "#e41a1c", "#377eb8", "#4daf4a",
                                     "#984ea3", "#ff7f00"),
                          labels = c("Age-adjusted", "Ages 5-14", "Ages 15-24",
                                     "Ages 25-44", "Ages 45-64", "Ages 65-84")) +
      geom_hline(yintercept=1, color = "dark grey") +
      geom_line() +
      theme(legend.position="bottom") +
      theme_bw()  +
      theme(text = element_text(size=16))+ 
      theme(legend.position="right") +
      theme(axis.title.x = element_blank()) + # Remove x-axis label
      ylab("Black-White % Difference") +      
      scale_x_continuous(breaks = seq(1930, 2015, def_break_length(input$slider[1], input$slider[2]))) +
      scale_y_continuous(breaks = seq(-80, 80, 10))}
)
  
}

shinyApp(ui, server)

