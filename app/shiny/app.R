#install.packages( "maps", dependencies = TRUE) #run this to install R package maps
################################- warning this will update existing packages if already installed


#*save the following code in a file named app.R *
library(shiny)
library(maps)
library(tidyverse)

##Section 1 ____________________________________________________
#load your data or create a data table as follows:
#countyData = read.table(
#  text = "State County
# Delaware Kent
# Delaware 'New Castle'
# Delaware Sussex
# 'Rhode Island' Bristol
# 'Rhode Island' Kent
# 'Rhode Island' Newport
# 'Rhode Island' Providence
# 'Rhode Island' Washington",
#  header = TRUE, stringsAsFactors = FALSE)


data <- readr::read_csv(
  "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv", 
  na = c("", " "))
data

data <- data %>%
  mutate(
    date = lubridate::dmy(dateRep),
  ) %>%
  separate(
    col = date, 
    into = c("year", "month", "day"), remove = FALSE) %>%
  mutate(
    month = as.integer(month)
  )

convert_months <- tibble(
  month_name = c(
    "january",
    "february",
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december"),
  month_number = c(1:12))

data <- left_join(data, convert_months, by = c("month" = "month_number"))



##Section 2 ____________________________________________________
#set up the user interface
ui = shinyUI(
  fluidPage( #allows layout to fill browser window
    titlePanel("Reactive select input boxes"),
    #adds a title to page and browser tab
    #-use "title = 'tab name'" to name browser tab
    sidebarPanel( #designates location of following items
      htmlOutput("continent_selector"),#add selectinput boxs
      htmlOutput("country_selector")# from objects created in server
    ),
    
    mainPanel(
      plotOutput("plot1") #put plot item in main area
    )
    
  ) )


##Section 3 ____________________________________________________
#server controls what is displayed by the user interface
server = shinyServer(function(input, output) {
  #creates logic behind ui outputs ** pay attention to letter case in names
  
  output$continent_selector = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "continent", #name of input
                label = "Continent:", #label displayed in ui
                choices = as.character(unique(data$continentExp)),
                # calls unique values from the State column in the previously created table
                selected = "Europe") #default choice (not required)
  })
  output$country_selector = renderUI({#creates County select box object called in ui
    
    data_available = data %>%
      dplyr::filter(continentExp == input$continent)
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "country", #name of input
                label = "Country:", #label displayed in ui
                choices = unique(data_available$countriesAndTerritories), #calls list of available counties
                selected = "Netherlands")
  })
  
  output$plot1 = renderPlot({ #creates a the plot to go in the mainPanel
    
  cases_plot <- data %>%
      dplyr::filter(
        continentExp == input$continent,
        countriesAndTerritories == input$country
      ) %>%
      ggplot(
        aes(
          x = date,
          y = cases_weekly
        )
      ) +
      geom_point() +
      geom_line(colour = "purple", size = 1) +
      ggtitle(
        paste("COVID-19 cases for", 
              input$country))
    #+
     # toolboxr::rotate_axis_labels("x", angle = 90)

    casualties_plot <-  data %>%
        dplyr::filter(
          continentExp == input$continent,
          countriesAndTerritories == input$country
        ) %>%
        ggplot(
          aes(
            x = date,
            y = deaths_weekly
          )
        ) +
        geom_point() +
        geom_line(colour = "darkred", size = 1) +
        ggtitle(
          paste("COVID-19 related deaths for", 
                input$country))
      
  
      cowplot::plot_grid(
        cases_plot,
        casualties_plot,
        ncol = 1)
        
  })
  

})#close the shinyServer

##Section 4____________________________________________________
shinyApp(ui = ui, server = server) #need this if combining ui and server into one file.



