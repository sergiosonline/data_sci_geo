library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  h2("Road Accidents in Toronto"),
  
  tabsetPanel(
    tabPanel("Interactive Map",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("acc_date", label = "Date of accident",
                             min = as.Date("2007-01-01"), max = as.Date("2017-12-01"),
                             value = c(as.Date("2007-01-01"), as.Date("2017-12-01")),
                             timeFormat = "%b %Y"),
                 
                 checkboxGroupInput("fatal", label = "Accident Fatality",
                                    choices = c("Fatal", "Non-Fatal Injury"),
                                    selected = c("Fatal", "Non-Fatal Injury")),
                 
                 selectInput("auto_type", label = "Vehicles Involved", multiple = T,
                             choices = c("Automobile", "Bicycle", "Motorcycle", "Pedestrian"),
                             selected = c("Automobile")),
                 
                 selectInput("precip", label = "Weather Condition",
                             choices = c("Clear", "Rain", "Snow"), multiple = T, 
                             selected = c("Clear")),
                 
                 sliderInput("acc_time", label = "Hour of accident",
                             min = 0, max = 24, step = 1,
                             value = c(0, 24)),
                 
                 checkboxInput("pop_label", label = "Overlay 2016 Population",
                               value = T)),
               
               mainPanel(leafletOutput("acc_map"),
                         dataTableOutput("acc_data"))
             )),
    
    tabPanel("Frequency Chart",
             sidebarLayout(
               sidebarPanel(),
               mainPanel()
             ))
  )
)
)
