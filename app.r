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
                   choices = list("Male" = "Male", "Female" = "Female"), 
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

mv_data <- read.csv(url("https://s3.amazonaws.com/shiny-app-data/absolute_differences_historical_bw.csv"))

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
      ylab("Black-White Ratio in Motor Vehicle Death Rate") +      
      scale_x_continuous(breaks = seq(1940, 2010, 10))   }
)
  
  # output$info <- renderPrint({
  #   # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
  #   # were a base graphics plot, we'd need those.
  #   nearPoints(mv_data, coordinfo = input$plot_click, addDist = TRUE)
  # })
  
}
shinyApp(ui, server)

