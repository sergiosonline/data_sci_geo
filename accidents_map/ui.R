#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  h2("Road Accidents in Toronto in 2007-2017"),
  
  tabsetPanel(
    tabPanel("Interactive Map",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("acc_date", label = "Date of accident",
                             min = as.Date("2007-01-01"), max = as.Date("2017-12-01"),
                             value = c(as.Date("2007-01-01"), as.Date("2017-12-01")),
                             timeFormat = "%b %Y"),
                 
                 checkboxGroupInput("fatal", label = "Accident Fatality",
                                    choices = c("Fatal", "Non-fatal"),
                                    selected = c("Fatal", "Non-fatal")),
                 
                 checkboxGroupInput("auto_type", label = "Vehicles Involved",
                                    choices = c("Automobile", "Bicycle", "Pedestrian"),
                                    selected = c("Automobile", "Bicycle", "Pedestrian")),
                 
                 checkboxGroupInput("precip", label = "Weather Condition",
                                    choices = c("Rain", "Snow", "All Conditions"),
                                    selected = "All Conditions"),
                 
                 checkboxInput("pop_label", label = "Show 2016 Population",
                               value = T)),
               mainPanel()
             )),
    tabPanel("Frequency Chart", "")
  )
)
)
