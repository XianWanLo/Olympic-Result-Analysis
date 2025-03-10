#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(sf)
library(dplyr)
library(shiny)
library(DT)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(ggiraph)
library(plotly)
library(countrycode)  
library(rnaturalearth)  
library(rnaturalearthdata)
library(shinydashboard)

# Load data from the dataset
athlete_df <- read.csv("athlete_events.csv")
noc_df <- read.csv("noc_regions.csv")
olympics_df <- merge(athlete_df, noc_df, by = "NOC", all.x = TRUE)

# Load country coordinates
world_data <- ne_countries(scale = "medium", returnclass = "sf")

# Main Dashboard tab
overall_tab <- tabPanel(
  
  title = 'Main Dashboard',
  div(class = "black-tab",
    
    # Tab title
    fluidRow(
           tags$h1("Overall Data Overview", style = "text-align: center; margin-bottom: 20px; margin-top: 20px;"),
    ),
    
    # Filter
    fluidRow(
      column(width = 12, 
             div(class = "filter",
                 # Select input for Olympics season filter
                 selectInput("selected_season", "Select Season:",
                             choices = c("All", "Summer", "Winter"),
                             selected = "All"),
                 
                 # Select inputs for gender filter
                 selectInput("selected_gender", "Gender:", 
                             choices = c("Both", "M", "F"), 
                             selected = "Both"),
                 
                 # Select inputs for sport filter
                 selectInput("selected_sport", "Select Sport:",
                             choices = c("All", sort(unique(olympics_df$Sport))), 
                             selected = "All"),
                 
                 # Select inputs for region filter
                 selectInput("selected_region", "Select Region:",
                             choices = c("All", sort(unique(olympics_df$region))), 
                             selected = "All"),
                 
                 # Select inputs for year filter
                 div(class = "slider",
                 sliderInput("selected_year_range", "Select Year Range:",
                             min = min(olympics_df$Year), 
                             max = max(olympics_df$Year),
                             value = c(min(olympics_df$Year), max(olympics_df$Year)),
                             step = 2)
                 )
              )
      )
    ),
    
    # Medal Map
    fluidRow(
      column(width = 12,
             div(class = "rowTitle",
                 tags$h3("Olympics Medal Map (click for details)", style = "color: orange; text-align: left;")
             )
      )
    ),
    fluidRow(
      column(width = 12,
             leafletOutput("medalMap"),
             style = "margin-bottom: 10px;"
      )
    ),
    
    # Olympic dashboard 
    fluidRow(
      column(width = 12, 
         div(class = "rowTitle",
             tags$h3("Olympic Dashboard", style = "color: orange; text-align: left;")
         )
      )
    ),
    
    # Shows medal count
    fluidRow(
      column(width = 12, 
             class = "medal",
             align = "center",
             valueBoxOutput("total_medal_count", width = 3),
             valueBoxOutput("gold_medal_count", width = 3),
             valueBoxOutput("silver_medal_count", width = 3),
             valueBoxOutput("bronze_medal_count", width = 3),
      )
    ),
    
    # Shows medal count
    fluidRow(
      
      # Athlete gender trend
      column(width = 5,  
             plotlyOutput("genderTrendPlot")
      ),
      
      # Top 20 Medalists
      column(width = 7,  
                 plotlyOutput("topMedalistPlot")
             )
      ),
  
    fluidRow(
    column(width = 5, 
           # Button to switch to "Athlete Data Overview" tab
           actionButton("switch_tab", "Click to view Athletes' Physical Data"),
           align = "center",
           style = "margin-bottom: 10px;margin-top: 10px;"
      )
    ),

    fluidRow(
      # Total sport events count
      column(width = 6,  
             plotlyOutput("totalEventsPlot"),
             style = "margin-bottom: 10px;"
      ),
      # Top 10 sport events
      column(width = 6,  
             plotlyOutput("topEventsPlot"),
             style = "margin-bottom: 10px;"
      ),
    )
  )
)


# Athlete Data Overview tab
athlete_tab <- tabPanel(
                            
  title = 'Athlete Data Overview',
  div(class = "black-tab",
    
      fluidRow(
       
        # Tab title
        column(width = 9,  
               tags$h1("Athlete Data Overview", style = "text-align: center; margin-bottom: 20px; margin-top: 20px;")
        ),
        
        # filter
        column(width = 3,  
               div(class = "filter",
                   # Select input for Olympics athlete type
                   selectInput("selected_athletes", "Athlete Type:",
                               choices = c("All", "Medalist only"),
                               selected = "All"),
                   style = "margin-bottom: 10px; margin-top: 10px;",
               )
        )
      ),
      
      # Average Age
      fluidRow(
        column(width = 12,
           div(class = "rowTitle",
               tags$h3("Olympic Athletes' Age Distribution", style = "color: orange; text-align: left;")
           ),
        ),
        
        column(width = 12,
            plotlyOutput("avgAgePlot"),
            align = "center"
        )
      ),
      
      # Average Physical data
      fluidRow(
        column(width = 12,
          div(class = "rowTitle",
              tags$h3("Olympic Athletes' Physical Data", style = "color: orange; text-align: left;")
          )
        )
      ),
      
      fluidRow(
        column(width = 6,  
               plotlyOutput("avgHeightPlot")
        ),
        
        column(width = 6,  
               plotlyOutput("avgWeightPlot")
        ),
      ),
      
      # Athlete Gender Distribution
      fluidRow(
        column(width = 12,
          div(class = "rowTitle",
              tags$h3("Olympic Athletes' Gender Distribution", style = "color: orange; text-align: left;")
          ),
        )
      ),
      
      fluidRow(
        # Display avgAgePlot and parHeightPlot side by side
        column(width = 12,  # Left column for the avgAgePlot
               plotOutput("SexDistributionPieChart")
        )
      ),

  )
)

# Olympic result by sports tab
result_by_sport_tab<- tabPanel(
  
  fluidRow(
    column(width = 12,
               tags$h3("Olympic Result by Sport", style = "text-align: center;")
           )
  ),
  title = 'Result By Sport',
  DTOutput("CompleteEventsPlot")

)

# Olympic result by medalist tab
result_by_medalist_tab<- tabPanel(
  
  fluidRow(
    column(width = 12,
           tags$h3("Olympic Result by Medalist", style = "text-align: center;")
    )
  ),
  title = 'Result By Medalist',
  DTOutput("CompleteMedalistPlot")
  
)

# UI for the Shiny app
ui <- navbarPage(
  id='mypage',
  title='Olympic Data Analysis from 1896 to 2016',
  
  # Custom style for layout
  tags$style(HTML("
    .black-tab {
      background-color: black;
      color: white;
    
    .slider {
      .irs-bar,
      .irs-bar-edge,
      .irs-single,
      .irs-handle{
        background-color: orange;
        border-color: orange;
      }
    }
    .filter {
      display: flex;
      flex-direction: row;
      justify-content: space-around;
    }
    .rowTitle{
      background-color: #444; 
      padding: 10px; 
      border-radius: 5px; 
      margin-bottom: 10px;
    }
    .medal {
      color:orange;
      justify-content: space-around;
      margin-bottom: 20px;
    }
  ")),
  
  overall_tab,
  athlete_tab,
  result_by_sport_tab,
  result_by_medalist_tab
)

# Define server logic
server <- function(input, output, session) {
  
  # Dark themed for plot
  custom_theme <- theme(
    plot.title = element_text(size = 16, color = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.grid = element_line(color = "gray"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
  )
  
  # Data filter accoring to user input
  filtered_data <- reactive({
    df <- olympics_df
    selected_region <- trimws(input$selected_region,c("both"))
    
    if (input$selected_gender != "Both") {
      df <- df[df$Sex == input$selected_gender, ]
    }
    
    if (input$selected_season != "All") {
      df <- df[df$Season == input$selected_season, ]
    }
    
    if (input$selected_sport != "All") {
      df <- df[df$Sport == input$selected_sport, ]
    }
    
    if (selected_region != "All") {
      df <- df[df$region == selected_region, ]
    }
    
    df <- df[df$Year >=input$selected_year_range[1] & df$Year <= input$selected_year_range[2], ]
    df
  })
  
  # ----------------------- Total Medal Counts -------------------------------
  
  output$total_medal_count <- renderValueBox({
    data <- filtered_data()
    total_medals <- sum(!is.na(data$Medal))
    valueBox(
      total_medals, "Total Medals", icon = icon("trophy")
    )
  })
  
  output$gold_medal_count <- renderValueBox({
    data <- filtered_data()
    gold_medals <- sum(data$Medal == "Gold", na.rm = TRUE)
    valueBox(
      gold_medals, "Gold Medals", icon = icon("medal")
    )
  })
  
  output$silver_medal_count <- renderValueBox({
    data <- filtered_data()
    silver_medals <- sum(data$Medal == "Silver", na.rm = TRUE)
    valueBox(
      silver_medals, "Silver Medals", icon = icon("medal")
    )
  })
  
  output$bronze_medal_count <- renderValueBox({
    data <- filtered_data()
    bronze_medals <- sum(data$Medal == "Bronze", na.rm = TRUE)
    valueBox(
      bronze_medals, "Bronze Medals", icon = icon("medal")
    )
  })
  
  # ----------------------- Athletes Count  --------------------------------
  output$genderTrendPlot <- renderPlotly({
    
    data <- filtered_data()
    
    # Process Data
    gender_counts <- data %>%
      filter(!is.na(Sex)) %>%
      group_by(Year, Sex) %>%
      summarise(AthleteCount = n(),.groups ="keep") 
    
    # Athlete count plot
    p <- ggplot(gender_counts, aes(x = Year, y = AthleteCount, color = Sex)) +
      geom_line(size = 1.0) +
      geom_point(size = 1.5) +
      labs(title = "Total Number of Athletes",
           x = "Year", y = "Number of Athletes") +
      custom_theme +
      scale_color_manual(values = c("M" = "orange", "F" = "yellow"))# Custom color 
      
    ggplotly(p)
  })
  
  # ----------------------- Total Event Count --------------------------------
  output$totalEventsPlot <- renderPlotly({
    
    data <- filtered_data()
    
    # Process Data
    total_events_per_year <- data %>%
      filter(!is.na(Event)) %>%
      group_by(Year, Season) %>%
      summarise(EventCount = n(),.groups ="keep") 
    
    # Total Event Count Plot
    p <- ggplot(total_events_per_year, aes(x = Year, y = EventCount, color = Season)) +
      geom_line(size = 1.0) +
      geom_point(size = 1.5) +
      labs(title = "Total Number of Sport Events",
           x = "Year", y = "Total Events") +
      custom_theme +
      scale_color_manual(values = c("Summer" = "orange", "Winter" = "yellow"))  # Custom color
    
    ggplotly(p) 
  })
  
  # ----------------------- Top 10 Result by Sport Plot --------------------------
  
  output$topEventsPlot <- renderPlotly({
    
    data <- filtered_data()
    
    # Process Data
    event_medals <- data %>%
      filter(!is.na(Medal)) %>%
      group_by(Sport, Medal) %>%
      summarise(MedalCount = n(),.groups ="keep")
    
    # Get the top 10 sports with the most medals
    top_sports <- event_medals %>%
      group_by(Sport) %>%
      summarise(TotalMedals = sum(MedalCount),.groups ="keep") %>%
      arrange(desc(TotalMedals)) %>%
      head(10) %>%
      pull(Sport)
    
    # Filter the original data
    top_event_medals <- event_medals %>%
      filter(Sport %in% top_sports) %>%
      arrange(desc(MedalCount)) 
    
    top_event_medals$Medal <- factor(top_event_medals$Medal, levels = c("Bronze", "Silver","Gold"))
    
    # Result by Sport Plot
    p <- ggplot(top_event_medals, aes(x = Sport, y = MedalCount, fill = Medal)) +
      geom_bar(stat = "identity", position = "stack", width = 0.5) +
      labs(title = paste("Top 10 Sports Events with Most Medals (click for more)"),
           x = "Sports Events", y = "Medal Count") +
      custom_theme +
      scale_fill_brewer(palette = "Oranges")
    
    # Register plot click
    result_plot <- ggplotly(p, source = "topEvents") %>%
      event_register("plotly_click")
    
    result_plot

  })
  
  # ----------------------- Complete Result by Sport Plot --------------------------
  
  output$CompleteEventsPlot <- renderDT({
    
    data <- filtered_data()
    
    # Data table plot
    event_medals <- data %>%
      filter(!is.na(Medal)) %>%
      group_by(Sport) %>%
      summarise(Total = n(),
                Gold = sum(Medal == "Gold", na.rm = TRUE),
                Silver = sum(Medal == "Silver", na.rm = TRUE),
                Bronze = sum(Medal == "Bronze", na.rm = TRUE),.groups ="keep")  %>%
      arrange(desc(Total)) %>%
      select(Sport, Gold,Silver,Bronze, Total)
  })
  
  # ----------------------------------- Athlete Sex Distribution ---------------------------------
  output$SexDistributionPieChart <- renderPlot({
    
    data <- filtered_data()
    if (input$selected_athletes == "Medalist only") {
      data <- data[!is.na(data$Medal), ]
    }
    
    # Process Data
    gender_distribution <- data %>%
      filter(!is.na(Sex)) %>%
      group_by(Sex) %>%
      summarise(Count = n())
    
    # Calculate percentage for pie chart annotation
    gender_distribution <- gender_distribution %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    # Gender Distribution Plot
    p <- ggplot(gender_distribution, aes(x = "", y = Count, fill = Sex)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start=0) +
      custom_theme +
      scale_fill_brewer(palette = "Oranges") + 
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 5)  # Adding percentage labels
    p
  }, bg="transparent")
  
  # ----------------------- Average Age --------------------------------
  output$avgAgePlot <- renderPlotly({
    
    data <- filtered_data()
    if (input$selected_athletes == "Medalist only") {
      data <- data[!is.na(data$Medal), ]
    }
    
    # Process Data
    avg_age <- data %>%
      filter(!is.na(Sex)) %>%
      group_by(Year, Sex) %>%
      summarise(AverageAge = mean(Age, na.rm = TRUE),.groups ="keep")
    
    # Average age plot
    p <- ggplot(avg_age, aes(x = Year, y = AverageAge, color = Sex)) +
      geom_line() +
      geom_point() +
      labs(title = "Average Age",
           x = "Year", y = "Average Age") +
      custom_theme+
      scale_color_manual(values = c("M" = "orange", "F" = "yellow"))
    
    # Convert ggplot to an interactive plotly graph
    ggplotly(p)
  })
  
  # ----------------------- Average Height --------------------------------
  output$avgHeightPlot <- renderPlotly({
    
    data <- filtered_data()
    if (input$selected_athletes == "Medalist only") {
      data <- data[!is.na(data$Medal), ]
    }
    
    # Process Data
    avg_height <- data %>%
      group_by(Year, Sex) %>%
      summarise(AverageHeight = mean(Height, na.rm = TRUE),.groups ="keep")
    
    # Average Height plot
    p <- ggplot(avg_height, aes(x = Year, y = AverageHeight, color = Sex)) +
      geom_line() +
      geom_point() +
      labs(title = "Average Height",
           x = "Year", y = "Average Height") +
      custom_theme +
      scale_color_manual(values = c("M" = "orange", "F" = "yellow"))
    
    ggplotly(p)
  })
  
  # ----------------------- Average Weight --------------------------------
  output$avgWeightPlot <- renderPlotly({
    
    data <- filtered_data()
    if (input$selected_athletes == "Medalist only") {
      data <- data[!is.na(data$Medal), ]
    }
    
    # Process Data
    avg_weight <- data %>%
      group_by(Year, Sex) %>%
      summarise(AverageWeight = mean(Weight, na.rm = TRUE),.groups ="keep")
    
    # Average Weight plot
    p <- ggplot(avg_weight, aes(x = Year, y = AverageWeight, color = Sex)) +
      geom_line() +
      geom_point() +
      labs(title = "Average Weight",
           x = "Year", y = "Average Weight") +
      custom_theme +
      scale_color_manual(values = c("M" = "orange", "F" = "yellow"))
    
    ggplotly(p)
  })
  
  
  # ----------------------- Top 20 medalist --------------------------------
  output$topMedalistPlot <- renderPlotly({
    
    data <- filtered_data()
    
    # Process Data
    participant_medals <- data %>%
      filter(!is.na(Medal)) %>%
      group_by(Name,Medal) %>%
      summarise(MedalCount = n(),.groups ="keep")%>%
      arrange(desc(MedalCount))
    
    # Get the top 20 sports with the most medals
    top_medallist <- participant_medals %>%
      group_by(Name) %>%
      summarise(TotalMedals = sum(MedalCount),.groups ="keep") %>%
      arrange(desc(TotalMedals)) %>%
      head(20) %>%
      pull(Name)
    
    # Filter the original data
    top_medallist_medals <- participant_medals %>%
      filter(Name %in% top_medallist) %>%
      arrange(desc(MedalCount)) 
    
    #set the order
    top_medallist_medals$Name <- factor(top_medallist_medals$Name, levels = unique(top_medallist_medals$Name))
    top_medallist_medals$Medal <- factor(top_medallist_medals$Medal, levels = c("Bronze", "Silver","Gold"))
    
    # Top medalist plot
    p <- ggplot(top_medallist_medals) +
      geom_col(aes(x = MedalCount, y = Name, fill = Medal), position = "stack", width = 0.7) +
      labs(title = paste("Top 20 Medallist (click for more)"),
           x = "Participants", y = "Medal Count") +
      custom_theme +
      scale_fill_brewer(palette = "Oranges")
    
    result_plot <- ggplotly(p, source = "topMedalists") %>%
      event_register("plotly_click")  # Register click event
    
    result_plot
  })
  
  # ----------------------- Complete Result by Medalist Plot --------------------------
  
  output$CompleteMedalistPlot <- renderDT({
    
    data <- filtered_data()
    
    # Data table plot
    event_medals <- data %>%
      filter(!is.na(Medal)) %>%
      group_by(Name) %>%
      summarise(Total = n(),
                Gold = sum(Medal == "Gold", na.rm = TRUE),
                Silver = sum(Medal == "Silver", na.rm = TRUE),
                Bronze = sum(Medal == "Bronze", na.rm = TRUE),.groups ="keep")  %>%
      arrange(desc(Total)) %>%
      select(Name,Gold,Silver,Bronze,Total)
  })
  
# -----------------------Medal Map ---------------
  output$medalMap <- renderLeaflet({
    data <- filtered_data()
    
    # Process Data
    medal_counts <- data %>%
      filter(NOC!="IOA") %>%
      group_by(region) %>%
      summarise(Gold = sum(Medal == "Gold", na.rm = TRUE),
                Silver = sum(Medal == "Silver", na.rm = TRUE),
                Bronze = sum(Medal == "Bronze", na.rm = TRUE),.groups ="keep")
    
    # Rename data column & Calculate total medal
    country_medals <- as.data.frame(medal_counts)
    colnames(country_medals) <- c("Region", "Gold", "Silver", "Bronze")
    country_medals$Total <-  country_medals$Gold + country_medals$Silver + country_medals$Bronze
    
    # Use countrycode to get the ISO3 code of countries
    country_medals$iso3 <- countrycode(country_medals$Region, origin = "country.name", destination = "iso3c")
    
    # Merge with world_data to get the country coordinates
    map_data <- merge(world_data, country_medals, by.x = "iso_a3", by.y = "iso3", all.x = TRUE)
    
    # Define a specific color bin for better visual experience
    mybins <- c(0, 5, 10, 50, 100, 300, 500, 1000, 5000, Inf)
    pal <- colorBin(palette = "YlOrRd", domain = map_data$Total, bins = mybins, na.color = "transparent")
    
    # Medal map with leaflet 
    # Dark-themed map background from https://carto.com/basemaps
    leaflet(map_data) %>%
      addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png") %>%
      addPolygons(
        fillColor = ~pal(Total),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.8,
        popup = ~paste(Region, 
                       "<br>Gold: ", Gold, 
                       "<br>Silver: ", Silver, 
                       "<br>Bronze: ", Bronze, 
                       "<br>Total: ",Total, " medals",
                       "<br><a href='javascript:' onclick='Shiny.setInputValue(\"selected_region\",\"",Region, "\")'>Click to View Country's Result</a>")
      ) %>%
      addLegend(
        pal = pal,
        values = ~Total,
        title = "Total Medal Count",
        position = "bottomright",
      ) %>%
      setView(lng = 0, lat = 0, zoom = 2)  # Center map
  })
  
  # Listen to click event in topEventsPlot
  observeEvent(event_data("plotly_click", source = "topEvents"), {
    clicked_event <- event_data("plotly_click", source = "topEvents")
    if (!is.null(clicked_event)) {
      updateNavbarPage(session, "mypage", selected = "Result By Sport")
    }
  })
  
  # Listen to click event in topMedalistPlot
  observeEvent(event_data("plotly_click", source = "topMedalists"), {
    clicked_event <- event_data("plotly_click", source = "topMedalists")
    if (!is.null(clicked_event)) {
      updateNavbarPage(session, "mypage", selected = "Result By Medalist")
    }
  })
  
  # Listen to actionButton
  observeEvent(input$switch_tab, {
    updateTabsetPanel(session, inputId = "mypage", selected = "Athlete Data Overview")
  })
  
  # Listen to click event in map
  observeEvent(input$selected_region, {
    selected_region <- trimws(input$selected_region,c("both"))
    updateSelectInput(session, "selected_region", selected = selected_region)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)