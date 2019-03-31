library(shiny)

shinyUI(fluidPage(
  
  h2("Road Accidents in Toronto"),
  
  sidebarLayout(
               sidebarPanel(
                 sliderInput("acc_date", label = "Date of accident",
                             min = as.Date("2007-01-01"), max = as.Date("2017-12-01"),
                             value = c(as.Date("2007-01-01"), as.Date("2017-12-01")),
                             timeFormat = "%b %Y"),
                 
                 checkboxGroupInput("fatal", label = "Accident Fatality",
                                    choices = c("Fatal", "Non-Fatal Injury"),
                                    selected = c("Fatal", "Non-Fatal Injury")),
                 
                 selectInput("auto_type", label = "Other Vehicles Involved", multiple = T,
                             choices = c("Pedestrian", "Bicycle", "Motorcycle", "Truck", "Emergency Vehicle"),
                             selected = c("Pedestrian")),
                 
                 selectInput("precip", label = "Weather Condition",
                             choices = c("Clear", "Precipitated"), multiple = T, 
                             selected = c("Clear")),
                 
                 sliderInput("acc_time", label = "Hour of accident",
                             min = 0, max = 24, step = 1,
                             value = c(0, 24)),

                 sliderInput("population", label = "2016 Population of Neighborhood",
                             min = 6000, max = 70000, step = 1000,
                             value = c(6000, 70000)),
                 
                 checkboxInput("pop_label", label = "Overlay 2016 Population",
                               value = F))
               ,

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Interactive Map", 
                         leafletOutput("acc_map"), 
                         dataTableOutput("acc_data"))
    ,
    tabPanel("Frequency Table",
             plotOutput("acc_plot"),
             dataTableOutput("acc_data2"))
  )
  )
)
))
