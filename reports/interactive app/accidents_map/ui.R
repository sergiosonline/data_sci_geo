library(shiny)

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
                 
                 checkboxGroupInput("auto_type", label = "Vehicles Involved",
                                    choices = c("Automobile", "Bicycle", "Pedestrian"),
                                    selected = c("Automobile", "Bicycle", "Pedestrian")),
                 
                 selectInput("precip", label = "Weather Condition",
                             choices = c("Rain", "Snow"), multiple = T, 
                             selected = c("Rain", "Snow")),
                 
                 sliderInput("acc_time", label = "Hour of accident",
                             min = 0, max = 24, step = 0.5,
                             value = c(0, 24)),
                 
                 checkboxInput("pop_label", label = "Overlay 2016 Population",
                               value = T)),
               
               mainPanel(leafletOutput("acc_map"))
             )),
    
    tabPanel("Frequency Chart",
             sidebarLayout(
               sidebarPanel(),
               mainPanel()
             ))
  )
)
)
