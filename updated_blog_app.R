library(tidyverse)
library(ggplot2)
library(shiny)
library(png)
library(grid)
library(readxl)
library(plotly)
library(rjson)

custom_colours <- c("#000000", "#FEB627", "#7E7E7E", "#27B9FE", "#FFFFFF")
seifa_colours <- c("#FEB627", "#27B9FE", "mediumseagreen", "indianred", "mediumslateblue")
tick_font <- list(size = 15)

# Load the data
#data <- read_xlsx("data.xlsx")
data <- read_csv("data_new.csv") %>% 
  filter(Year < 2020)

# Load denominators (population and SEIFA data)
gov_regions <- read_excel("gov_regions_retouched.xlsx") %>%
  rename(postcode = Postcode) %>%
  distinct(postcode, .keep_all = TRUE)
postcode_denominators <- read_csv("postcode_denominators.csv") %>%
  filter(age == 0) %>% 
  mutate(postcode = as.character(postcode)) %>%
  left_join(gov_regions, by = "postcode")
SEIFA_2016 <- read_csv("seifa_2016.csv") %>% mutate(postcode = as.character(postcode))
seifa_population_2016 <- postcode_denominators %>%
  left_join(SEIFA_2016, on = "postcode") %>% 
  rename(`Relative Socio-Economic Disadvantage` = SEIFA_disadvantage,
         `Education and Occupation` = SEIFA_education_occupation,
         `Economic Resources` = SEIFA_economic,
         `Relative Socio-Economic Advantage and Disadvantage` = SEIFA_advantage_disadvantage)
region_denoms <- postcode_denominators %>%
  filter(!is.na(`SA Government Region`)) %>% 
  group_by(`SA Government Region`) %>% 
  summarise(Population = sum(adjusted_population))
denominators <- read_csv("my_denominators.csv") %>% filter(age == 0)
yearly_denominators <- denominators %>% 
  group_by(year) %>% summarise(population = sum(adjusted_population)) %>% rename(Year = year)
atsi_populations <- denominators %>% 
  group_by(year, Cultural.Background) %>% 
  summarise(Population = sum(adjusted_population)) %>% 
  rename(Year = year,
         `Cultural Background` = Cultural.Background)
# Load birth data and safe sleeping risk data
births <- read_csv("perinatal_temp.csv") %>% select(c("year","total"))
safe_sleeping_risks <- read_csv("safe_sleeping_risks_Feb2021.csv", col_names = 
                                  c("case_number", "Unsafe bedding", "Parental smoking", 
                                    "Not in an approved bed", "Not placed on back", "Bed sharing", "Not breastfed"),
                                col_types = "nffffff", skip = 1) %>% 
  filter(!is.na(case_number))
# Filter SUDI deaths (Sudden and Unexpected Death in Infancy)
sudi_all <- filter(data, SUDI == "Yes")
# Extract safe sleeping risk SUDIs
sudi_sleeping <- data %>% inner_join(safe_sleeping_risks, on = case_number)
# Count deaths by year
yearly_deaths <- sudi_all %>% 
  group_by(Year) %>% summarise(Deaths = n())
atsi_deaths <- sudi_sleeping %>% 
  group_by(Year, cultural_background) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  complete(Year, cultural_background, fill=list(Deaths=0)) %>% 
  rename(`Cultural Background` = cultural_background)
# Map data
map_df_json <- fromJSON(file = "gov_regions/SAGovtRegions_GDA2020.json")
rates_regions <- sudi_sleeping %>% 
  rename(`SA Government Region` = region) %>% 
  group_by(`SA Government Region`) %>%
  summarise(`Number of deaths` = n()) %>% 
  left_join(region_denoms, by = "SA Government Region") %>% 
  mutate(`Death rate` = `Number of deaths`/Population*10000)

indexes = c("Relative Socio-Economic Disadvantage",
            "Education and Occupation",
            "Economic Resources",
            "Relative Socio-Economic Advantage and Disadvantage")
##----------------------------------------------------------------------------------------------------

# UI function
ui <- (fluidPage(
  # Page title
  #titlePanel("Sleep-related infant deaths in South Australia, 2005-2019"),
  
  # Define the sidebar
  fluidRow(
    
    column(10,
      style = "background-color: #F0F0F0;",
      #align = "center",
      offset = 1,
      
      br(),
      
      div(align = "center",
        strong("Sleep-related infant deaths in South Australia, 2005-2019",
          style = "text-align:center; font-size: 18px")
        ),
      br(),
  
      # Dropdown menu for selecting which figure to display
      selectInput(inputId = "option", label = "Select figure:",
                  choices = c("Figure 1: Safe sleeping risks over time",
                              "Figure 2: Prevalence of safe sleeping risks",
                              "Figure 3: Sleep-related deaths by socio-economic factors",
                              "Figure 4: Sleep-related deaths by government region"
                              ),
                  selectize = FALSE),
      
      # Conditional panel that appears when yearly safe sleeping deaths figure is selected
      conditionalPanel(condition = "input.option == 'Figure 1: Safe sleeping risks over time'",
                       selectInput("ssrisk_option",
                                   multiple = T,
                                   label = "Risk factor:",
                                   choices = c("Unsafe bedding", "Parental smoking", 
                                               "Not in an approved bed", "Not placed on back", 
                                               "Bed sharing", "Not breastfed",
                                               "Total"),
                                   selected = c("Total"))),
      
      # Conditional panel that appears when SEIFA figure is selected
      conditionalPanel(condition = "input.option == 'Figure 3: Sleep-related deaths by socio-economic factors'",
                       selectInput("SEIFA_option",
                                   label = "Socio-economic index (SEIFA):",
                                   choices = indexes)),
      
      # Conditional panel that appears when regions figure is selected
      conditionalPanel(condition = "input.option == 'Figure 4: Sleep-related deaths by government region'",
                       selectInput("Region_measure",
                                   label = "Measure:",
                                   choices = c("Death rate", "Number of deaths")))
    )
  ),
  
  fluidRow(
    
    column(10,
        align = "center",
        offset = 1,
        
        br(),
        
        div(
          style = "max-width: 1200px; height: 80vh",
          plotlyOutput("figure",
                       width = "100%",
                       height = "100%")
        )
           
    )
    
  )
  ))

##----------------------------------------------------------------------------------------------------

# Server function
server <- (function(input, output) {
  
  # Fill in the spot we created for a plot
  output$figure <- renderPlotly({
    option <- input$option
    
    # Figure one code
    if (option == "Figure 1: Safe sleeping risks over time"){
      ssrisk_option <- input$ssrisk_option
      risk_colours <- c("#FEB627", "#27B9FE", "mediumseagreen", "indianred", "mediumslateblue", "darkblue", "black")
      # Link colours to factors
      line_colours <- setNames(risk_colours, 
                               c("Unsafe bedding", "Parental smoking", 
                                 "Not in an approved bed", "Not placed on back", 
                                 "Bed sharing", "Not breastfed",
                                 "Total"))
      # Convert data to long form so we can categorise by risk
      df <- pivot_longer(data = safe_sleeping_risks,
                         cols = !case_number,
                         names_to = "Risk", 
                         values_to = "present") %>%
        left_join(select(sudi_all, c("case_number", "Year")), on = Year) %>% 
        filter(present == "Yes") %>% 
        group_by(Year, Risk) %>% summarise(n = n()) %>% 
        ungroup() %>% 
        complete(Year, Risk, fill = list(n = 0))
      
      # Yearly deaths
      totals <- sudi_all %>% 
        left_join(select(sudi_all, c("case_number", "Year")), on = Year) %>% 
        group_by(Year) %>% summarise(n = n()) %>% mutate(Risk = "Total") %>% 
        .[, c("Year", "Risk", "n")]
      
      df <- bind_rows(df, totals) %>% filter(Risk %in% ssrisk_option) %>% 
        left_join(yearly_denominators, on = "Year") %>% 
        mutate(rate = n/population*20000)
      
      # Plot the data
      plot_ly(data = df,
              type = "scatter",
              mode = "lines",
              color = ~Risk,
              colors = line_colours,
              x = ~Year,
              y = ~n,
              text = ~n,
              hovertemplate = paste0("Number of deaths: ", "%{text}", "<extra></extra>")
              #height = 500
      ) %>% 
        layout(#width = 600,
          autosize = T,
          legend = list(x = 0.65, y = 0.95, bgcolor = "rgba(0, 0, 0, 0)"),
          
          yaxis = list(title = "Number of deaths\ninvolving risk factor",
                       titlefont = list(size = 18),
                       #range = c(0, 13),
                       ticklen = 6,
                       tickcolor = "white",
                       tickfont = tick_font),
          xaxis = list(title = "Year\n",
                       titlefont = list(size = 18),
                       range = c(2005, 2019),
                       dtick = 2,
                       tick0 = 2005,
                       ticklen = 6,
                       tickcolor = "white",
                       tickfont = tick_font
          ))
    }
    
    # Figure two code
    else if (option == "Figure 2: Prevalence of safe sleeping risks"){
      # Transform sleeping risk data to long form
      long_df <- gather(safe_sleeping_risks, risk, present, `Unsafe bedding`:`Not breastfed`, factor_key = TRUE)
      # Calculate proportion of deaths involving each risk factor
      props <- long_df %>% 
        group_by(risk) %>% 
        count(present) %>% 
        mutate(prop = n/sum(n)) %>% 
        arrange(prop)
      # Plot the data
      plot_ly(data = filter(props, present == "Yes"),
              type = "bar",
              color = I(custom_colours[2]),
              x = ~reorder(risk, prop),
              y = ~prop,
              text = ~n,
              hovertemplate = paste0("Number of deaths: ", "%{text}", "<extra></extra>")) %>% 
        layout(#width = 600,
          #height = 500,
          autosize = T,
          yaxis = list(title = "Percentage of deaths\ninvolving factor",
                       tickformat = "%", # Convert props to percentage
                       range = c(0,0.8),
                       titlefont = list(size = 18),
                       ticklen = 6,
                       tickcolor = "white",
                       tickfont = tick_font),
          xaxis = list(title = "Risk factor",
                       titlefont = list(size = 18),
                       ticklen = 6,
                       tickcolor = "white",
                       tickfont = list(size = 12)))
    }
    
    # Figure three code
    else if (option == "Figure 3: Sleep-related deaths by socio-economic factors"){
      # Get user selection
      SEIFA_option <- input$SEIFA_option
      # Copy data and rename columns to proper SEIFA index names
      fig3_data <- sudi_sleeping %>% rename(`Relative Socio-Economic Disadvantage` = SEIFA_disadvantage,
                          `Education and Occupation` = SEIFA_education_occupation,
                          `Economic Resources` = SEIFA_economic,
                          `Relative Socio-Economic Advantage and Disadvantage` = SEIFA_advantage_disadvantage)
      
      dfs <- list()
      
      # Generate data frames with populations of infants in each SEIFA quintile within each index
      for (i in indexes){
        infant_populations <- filter(seifa_population_2016, age == 0) %>% 
          filter(!is.na(.data[[i]])) %>% 
          group_by_at(i) %>% 
          summarise(population = sum(adjusted_population))
        # Rate of infant death by quintile
        dfs[[i]] <- assign(paste0(i,"_df"), fig3_data %>% filter(!is.na(.data[[i]])) %>%  
          group_by_at(i) %>% summarise(n = n()) %>% 
          left_join(infant_populations) %>%
          mutate(rate = n/population*10000,
                 average_population = population/15))
      }
      
      plot_ly(data = dfs[[SEIFA_option]],
              type = "bar",
              marker = list(color = seifa_colours),
              x = ~dfs[[SEIFA_option]][[SEIFA_option]],
              y = ~rate,
              text = ~paste0("Deaths since 2005: ", n, 
                             "\n", "Average infant population: ", round(average_population,0)),
              hovertemplate = paste0("%{text}", "<extra></extra>")) %>% 
        layout(
               autosize = T,
               yaxis = list(title = "Sleep-related deaths\nper 10,000 infants",
                            titlefont = list(size = 18),
                            ticklen = 6,
                            tickcolor = "white",
                            tickfont = tick_font),
               xaxis = list(title = paste0(SEIFA_option, "\n"),
                            titlefont = list(size = 18),
                            ticklen = 6,
                            tickcolor = "white",
                            tickfont = tick_font,
                            ticktext = list("1\n(most disadvantaged)", "2", "3", "4", "5\n(least disadvantaged)"), 
                            tickvals = list(1, 2, 3, 4, 5)))
    }
    
    
    else if (option == "Figure 4: Sleep-related deaths by government region"){
      map_colours <- c("#FFFFFF", "#FEB627")
      i <- input$Region_measure
      legend_text <- ifelse(i == "Death rate", "Death rate\nper 10,000\ninfants\n \n ", "Number \nof deaths \n \n ")
      pal <- colorRampPalette(map_colours)
      colourscale <- pal(100)
      plot_ly() %>% 
        add_trace(type="choroplethmapbox",
                  geojson=map_df_json,
                  locations=rates_regions$`SA Government Region`,
                  z=round(rates_regions[[i]],2),
                  colors=colourscale,
                  #zmax=34,
                  #zmin=18,
                  featureidkey="properties.region",
                  marker=list(opacity=0.75),
                  text=rates_regions$`SA Government Region`,
                  hovertemplate=paste0(i, ": %{z}", "\n",
                                       "Infant population: ", round(rates_regions$Population/15, 0),
                                       "<extra>%{text}</extra>")) %>% 
        colorbar(title = legend_text,
                 x=1, y=1,
                 len=1) %>% 
        layout(mapbox=list(style="carto-positron",
                           zoom=4.5,
                           center=list(lon=134.5, lat=-33)))
    }
  
  })
  
  
  # Downloadable .xlsx of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "data.xlsx",
    content = function(file) {
      writexl::write_xlsx(data, file)
    }, 
  )
})

##----------------------------------------------------------------------------------------------------

shinyApp(ui, server)

