# line plot of meat consumption over time by type of meat 

library(tidyverse)
library(shiny)
library(shinythemes)
library(lubridate)
library(plotly)



# load data (source: https://www.kaggle.com/datasets/ulrikthygepedersen/meat-consumption)
meat <- read.csv("Pathway/meat_consumption.csv",
                 header = T,
                 sep = ",",
                 stringsAsFactors = F)

# clean data
meat <- meat %>% 
  dplyr::select(-indicator,-frequency) %>% # remove unnecessary variables
  mutate(across(where(is.character), as.factor), #change character to factor variables
         location_c = as.character(location), # keep location as a character variable
         subject = fct_collapse(subject,
                                beef = c("BEEF"),
                                pig = c("PIG"),
                                sheep = c("SHEEP"),
                                poultry = c("POULTRY"))) %>% 
  group_by(location, time, measure) %>% 
  mutate(overall_value = sum(value)) %>% # create new variable that includes the total consumption of all meat types at each time point
  ungroup() %>% 
  mutate(value = ifelse(measure == "KG_CAP", round(value, 2), log(value)), # transform meat consumption measures
         overall_value = ifelse(measure == "KG_CAP", round(overall_value, 2), log(overall_value)))

# create new continent variables
meat <- meat %>% mutate(continents = fct_collapse(location,
                                                  North_America = c("CAN", "USA"),
                                                  South_America = c("MEX", "ARG", "BRA", 
                                                                    "CHL","PRY","PER"),
                                                  Asia = c("JPN", "KOR", "CHN", "COL", 
                                                           "IND", "IDN", "IRN","ISR", 
                                                           "KAZ","MYS", "PAK", "PHL",
                                                           "SAU","THA","VNM", "TUR"),
                                                  Oceania = c("AUS","NZL"),
                                                  Europe = c("UKR", "NOR", "CHE", "GBR", 
                                                             "EU27", "RUS"),
                                                  Africa = c("EGY","ETH", "NGA", "ZAF"),
                                                  Other = c("WLD","OECD", "BRICS")))


# time variables
meat <- meat %>%
  mutate(time_date = make_date(time, 1, 1), # create a new date variable
         time_y = time_date - as.Date('1990-01-01'), # create underlying time variable >30years
         time_y_2000 = time_date - as.Date('2000-01-01'),
         time_y_2010 = time_date - as.Date('2010-01-01'),
         time_y = round(time_length(time_y, "years"), digits = 0),
         time_y_2000 = round(time_length(time_y_2000, "years"), digits = 0),
         time_y_2010 = round(time_length(time_y_2010, "years"), digits = 0))




# Define UI
ui <- fluidPage(
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel( 
      
      HTML("<h5>Type of Meat</h3>"),
      # Input values
      selectInput("subject", "Type:",
                  choices = list("Beef meat" = "beef", "Pork meat" = "pig", 
                                 "Sheep meat" = "sheep", "Poultry meat" = "poultry"),
                  selected = "beef")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: linegraph ----
      plotlyOutput(outputId = "plot2")
      
    )
  )
)

# Define server logic for Shiny app
server <- function(input, output) {
  # Render the plot based on the selected inputs
  output$plot2 <- renderPlotly({
    # Filter the data based on the selected inputs
    filtered_data2 <- meat %>%
      filter(subject == input$subject)
    
    # Generate ggplot object for the filtered data
    filtered_data2 %>% 
      filter(measure == "KG_CAP" & continents != "Other" & time <=2023) %>%
      group_by(continents, time) %>% 
      filter(!duplicated(location)) %>% 
      mutate(overall_val_cont = mean(overall_value)) %>% 
      ungroup() %>% 
      mutate(continents = fct_reorder(continents, overall_val_cont)) %>% 
      mutate(continents = fct_rev(continents)) %>% 
      ggplot(aes(x = time_y, y = value, group = location))+
      geom_line(aes(group = location), color = "grey",linewidth = 0.8, alpha = 0.4)+theme_classic()+
      geom_smooth(aes(group = continents, fill = continents),
                  formula =  y ~ x, method = "lm", size = 0.8, color = "black")+
      labs(y = "Kg / Capita", x = "Time since 1990 (Years)",
           fill = "Continents",  
           title = paste("Trend in global meat consumption (kg/ capita)", "\nfrom 1990 to 2023"))+ 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold")) -> p
    plot_obj <- ggplotly(p)
    
    
    # Render the ggplot object
    print(plot_obj)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

