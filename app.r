# install.packages("shiny")
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Black-White Differences in Motor Vehicle Death Rates, 1934-2014"),
  
  fluidPage(
    
    sidebarPanel(
      # Copy the line below to make a slider range 
      sliderInput("slider", label = h3("Year Range"), min = 1934, 
                  max = 2014, value = c(1934, 2014)),

  
      # Copy the line below to make a set of radio buttons
      radioButtons("radio", label = h3("Select Sex"),
                   choices = list("Men" = "Male", "Women" = "Female"), 
                   selected = "Male"),
      
      checkboxGroupInput("age", "Age Groups:",  
                  c("Adjusted" = "adjusted",
                    "Ages 5-14" = "age05.14",
                    "Ages 15-24" = "age15.24",
                    "Ages 25-44" = "age25.44",
                    "Ages 45-64" = "age45.64",
                    "Ages 65-84" = "age65.84"),
                  selected = c("adjusted"))
    
    ),
    
    mainPanel(
      plotOutput("mv_plot", click = "plot_click"),
      verbatimTextOutput("info")
    )
    )
)

# reads in cleaned data from AWS
mv_data <- read.csv(url("https://s3.amazonaws.com/shiny-app-data/absolute_differences_historical_bw.csv"))

# function to define the break of x axis label for graph depending on selected year range
# @param: min_value is the lower value in the sliders
# @param: max_value is the upper value in the sliders

def_break_length <- function(min_value, max_value){
  total_range <- max_value-min_value
  if (total_range > 30){
    return(10)
  } else if (total_range %in% (15:30)){
    return(5)
  } else{
    return(1)
  }
}

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

  output$mv_plot <- renderPlot({mv_data %>% 
    filter(age.cat %in% input$age,
           gender == input$radio,
           year %in% (input$slider[1]:input$slider[2])) %>%
      ggplot(aes(x=year, y=bw_asdr.s, group = age.cat, colour = age.cat)) +
#      scale_colour_manual(values = c("#000000", "#e41a1c", "#377eb8", "#4daf4a",
  #                                   "#984ea3", "#ff7f00")) +
      geom_hline(yintercept=1, color = "grey") +
      geom_line() +
      theme(legend.position="bottom") +
      theme_bw()  +
      theme(text = element_text(size=16))+ 
      theme(legend.position="bottom") +
      theme(legend.title=element_blank()) +
      theme(axis.title.x = element_blank()) +   # Remove x-axis label
      ylab(paste0("Black-White Ratio in Motor Vehicle Death Rate for ", input$radio)) +      
      scale_x_continuous(breaks = seq(1940, 2015, def_break_length(input$slider[1], input$slider[2])))   }
)
  
}
shinyApp(ui, server)

