#######################################################################################################
## COVID-19 App, based on data from the ECDC: see: 'https://www.ecdc.europa.eu/en/publications-data
## App author: Marc A.T. Teunis, PhD, 2021 (CC-BY/NC)
#######################################################################################################

#--------------------------------------------------------------------------------------

## load helper function and data loading script
source(
  "helpers.R"
)

#--------------------------------------------------------------------------------------

#set up the user interface
ui = shinyUI(
  fluidPage( #allows layout to fill browser window
    titlePanel("COVID-19 cases and anti-SARS-CoV2 vaccinations"),
    #adds a title to page and browser tab
    #-use "title = 'tab name'" to name browser tab
    sidebarPanel( #designates location of following items
      htmlOutput("continent_selector"),#add selectinput boxs
      htmlOutput("country_selector"),
      verbatimTextOutput("info")# from objects created in server
    ),
    
    
    mainPanel(
      plotOutput("plot1", height = 650) #put plot item in main area
    )
    
  ) )

# ---------------------------------------------------------------------------------------

#server controls what is displayed by the user interface
server = shinyServer(function(input, output) {
  #creates logic behind ui outputs ** pay attention to letter case in names
  
  output$continent_selector = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "continent", #name of input
                label = "Continent:", #label displayed in ui
                choices = as.character(unique(data$continent)),
                # calls unique values from the State column in the previously created table
                selected = "Europe") #default choice (not required)
  })
  output$country_selector = renderUI({#creates County select box object called in ui
    
    data_available = data %>%
      dplyr::filter(continent == input$continent)
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "country", #name of input
                label = "Country:", #label displayed in ui
                choices = unique(data_available$country), #calls list of available counties
                selected = "Netherlands")
  })
  
  
  output$info = renderText(paste(
"The data is downloaded from the 
European Center for Disease Control.
For a full description of the data and 
the datasets, see: 
'https://www.ecdc.europa.eu/en/publications-data' 

Legend to figures. 
A) Weekly reported cases
B) Weekly reported deaths
C) Weekly vaccinations for each vaccine, 
the red dotted line is the total per week
D) Cumulative vaccinations

Vaccine supplier and target group:
COM = Comirnaty – Pfizer/BioNTech;
MOD = mRNA-1273 – Moderna,
CN = BBIBV-CorV – CNBG,
SIN = Coronavac – Sinovac,
SPU = Sputnik V - Gamaleya Research Institute,
AZ = AZD1222 – AstraZeneca,
UNK = UNKNOWN,

App author: Marc A.T. Teunis, 2021 [CC-BY/NC]")
)
  
  
  
  output$plot1 = renderPlot({ #creates the plot to go in the mainPanel
    
    cases_plot <- data %>%
      dplyr::filter(
        continent == input$continent,
        country == input$country,
        indicator == "cases") %>%
      ggplot(
        aes(
          x = date,
          y = weekly_count
        )
      ) +
      geom_point(colour = "purple", size = 3) +
      scale_x_date(breaks="month", date_labels = "%b-%y") +
      geom_line(colour = "purple", size = 2) +
      ggtitle(
        paste("COVID-19 cases for", 
              input$country)) +
      ylab("Total count per week") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  

    casualties_plot <-  data %>%
        dplyr::filter(
          continent == input$continent,
          country == input$country,
          indicator == "deaths") %>%
        ggplot(
          aes(
            x = date,
            y = weekly_count
          )
        ) +
        geom_point(colour = "darkred", size = 3) +
        scale_x_date(breaks="month", date_labels = "%b-%y") +
        geom_line(colour = "darkred", size = 2) +
        ggtitle(
          paste("COVID-19 related deaths for", 
                input$country)) +
        ylab("Total count per week")  +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    data_vaccination_total <- data_vac %>%
      dplyr::filter(
        country == input$country) %>%
      group_by(date) %>%
      summarise(total = sum(number_doses_received))
    
    
    vaccination_plot <- data_vac %>%
      dplyr::filter(
       # continent == input$continent,
        country == input$country) %>%
      group_by(vaccine, target_group, country, date) %>%
      summarise(total = sum(number_doses_received)) %>%
      ggplot(
        aes(
          x = date,
          y = total
        )
      ) +
      geom_point(aes(colour = vaccine, shape = target_group), size = 3) +
      scale_x_date(breaks="month", date_labels = "%b-%y") +
      geom_line(aes(colour = vaccine, shape = target_group), size = 2) +
      ggtitle(
        paste("anti-SARS-CoV2 vaccinations", 
              input$country)) +
      ylab("Total count per week") +
      geom_line(
        data = data_vaccination_total, 
        colour = "darkred", 
        size = 2, 
        linetype = "dashed") +
   #   scale_y_log10() +
      theme_bw()
    
      
    ## cumulative numbers for vaccination
    
    ind <- !is.na(data_vac$number_doses_received)
    
    data_vac_no_na <- data_vac[ind, ]
    
    vaccination_plot_cumsum <- data_vac_no_na %>%
      dplyr::filter(
        country %in% input$country) %>%
      group_by(country, date) %>%
      summarize(total = sum(number_doses_received, na.rm = TRUE)) %>%
      mutate(total_cummulative = cumsum(total)) %>%
      ggplot(
        aes(
          x = date,
          y = total_cummulative
        )
      ) +
      geom_point(colour = "darkgreen", size = 3) +
      scale_x_date(breaks="month", date_labels = "%b-%y") +
      geom_line(colour = "darkgreen", size = 2) +
      ggtitle(
        paste("Cumulative anti-SARS-CoV2 vaccinations", 
              input$country)) +
      ylab("Cumulative count")
  #    scale_y_log10() +
      theme_bw()
    
      
      ## plot all in panel
      cowplot::plot_grid(
        cases_plot,
        casualties_plot,
        vaccination_plot,
        vaccination_plot_cumsum,
        ncol = 2, labels = c("A", "B", "C", "D"))
        
  })
  

})#close the shinyServer

#----------------------------------------------------------------------------------------

## run shiny app from single app.R script
shinyApp(ui = ui, server = server) #need this if combining ui and server into one file.
