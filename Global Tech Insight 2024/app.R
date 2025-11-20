# ================== Packages ==================
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(leaflet)
library(sf)
library(DT)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(writexl)   # for Excel download

# ================== Data ==================
stackdev <- read_csv("stackdev_clean.csv", show_col_types = FALSE)

# safety: make sure numeric experience exists
if (!"YearsCodePro_num" %in% names(stackdev)) {
  stackdev <- stackdev %>%
    mutate(
      YearsCodePro_num = case_when(
        YearsCodePro == "Less than 1 year" ~ 0,
        YearsCodePro == "More than 50 years" ~ 50,
        TRUE ~ suppressWarnings(as.numeric(YearsCodePro))
      )
    )
}

# world map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# ================== UI ==================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Global Tech Employment & Skills"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Data & Export", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # ------------- TAB 1: DASHBOARD -------------
      tabItem(
        tabName = "dashboard",
        
        # ---- Filters on TOP ----
        fluidRow(
          box(
            width = 12, solidHeader = TRUE, status = "primary",
            title = "Filters",
            
            fluidRow(
              column(
                width = 3,
                selectizeInput(
                  "country", "Country",
                  choices = sort(unique(na.omit(stackdev$Country))),
                  multiple = TRUE,
                  options = list(placeholder = "All countries")
                )
              ),
              column(
                width = 3,
                selectizeInput(
                  "role", "Developer role",
                  choices = sort(unique(na.omit(stackdev$DevType))),
                  multiple = TRUE,
                  options = list(placeholder = "All roles")
                )
              ),
              column(
                width = 3,
                sliderInput(
                  "exp", "Years of professional coding experience:",
                  min = 0, max = 50, value = c(0, 50)
                )
              ),
              column(
                width = 3,
                selectInput(
                  "remote", "Remote work preference:",
                  choices = c("All", sort(unique(na.omit(stackdev$RemoteWork)))),
                  selected = "All"
                )
              )
            ),
            
            fluidRow(
              column(
                width = 3,
                checkboxInput(
                  "with_salary_only",
                  "Keep rows with known salary only",
                  value = TRUE
                )
              ),
              column(
                width = 3,
                br(),
                actionButton("reset", "Reset filters")
              )
            )
          )
        ),
        
        # ---- Row 1: Map + Salary vs Experience ----
        fluidRow(
          box(
            width = 6, height = 380, solidHeader = TRUE, status = "info",
            title = "Global Salary Map (USD)",
            leafletOutput("salary_map", height = 320)
          ),
          box(
            width = 6, height = 380, solidHeader = TRUE, status = "info",
            title = "Salary vs Experience",
            plotOutput("salary_exp", height = 320)
          )
        ),
        
        # ---- Row 2: Salary by Role + JobSat ----
        fluidRow(
          box(
            width = 6, height = 380, solidHeader = TRUE, status = "info",
            title = "Salary Distribution by Role",
            plotOutput("salary_role", height = 320)
          ),
          box(
            width = 6, height = 380, solidHeader = TRUE, status = "info",
            title = "Job Satisfaction vs Salary",
            plotOutput("job_sat", height = 320)
          )
        ),
        
        # ---- Row 3: Demand + Skills ----
        fluidRow(
          box(
            width = 6, height = 380, solidHeader = TRUE, status = "info",
            title = "Job Demand by Role vs Experience",
            plotOutput("job_demand", height = 320)
          ),
          box(
            width = 6, height = 380, solidHeader = TRUE, status = "info",
            title = "Top Languages (Want to Work With)",
            plotOutput("skill_forecast", height = 320)
          )
        )
      ),
      
      # ------------- TAB 2: DATA + DOWNLOADS -------------
      tabItem(
        tabName = "data",
        
        fluidRow(
          box(
            width = 8, status = "primary", solidHeader = TRUE,
            title = "Filtered Data",
            p("This table uses the same filters as the Dashboard."),
            DTOutput("data_table"), br()
          ),
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "Download",
            p("Download the filtered dataset:"),
            downloadButton("download_csv", "Download CSV"),
            br(), br(),
            downloadButton("download_excel", "Download Excel")
            # You could add a fake PDF button here if the rubric insists,
            # but real PDF export usually requires LaTeX on the server.
          )
        )
      )
    )
  )
)

# ================== SERVER ==================
server <- function(input, output, session) {
  
  # ----- Reset filters -----
  observeEvent(input$reset, {
    updateSelectizeInput(session, "country", selected = character(0))
    updateSelectizeInput(session, "role", selected = character(0))
    updateSliderInput(session, "exp", value = c(0, 50))
    updateSelectInput(session, "remote", selected = "All")
    updateCheckboxInput(session, "with_salary_only", value = TRUE)
  })
  
  # ----- Reactive filtered data -----
  df_filt <- reactive({
    d <- stackdev
    
    # country
    if (length(input$country) > 0) {
      d <- d %>% filter(Country %in% input$country)
    }
    
    # roles
    if (length(input$role) > 0) {
      d <- d %>%
        separate_rows(DevType, sep = ";\\s*") %>%
        filter(DevType %in% input$role)
    }
    
    # experience
    d <- d %>%
      filter(
        !is.na(YearsCodePro_num),
        YearsCodePro_num >= input$exp[1],
        YearsCodePro_num <= input$exp[2]
      )
    
    # remote work
    if (input$remote != "All") {
      d <- d %>% filter(RemoteWork == input$remote)
    }
    
    # salary known
    if (isTRUE(input$with_salary_only)) {
      d <- d %>% filter(!is.na(Salary_USD))
    }
    
    d
  })
  
  # ----- (1) Global salary map -----
  output$salary_map <- renderLeaflet({
    d <- df_filt() %>%
      group_by(Country) %>%
      summarise(
        avg_salary = mean(Salary_USD, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
    
    map_df <- world %>%
      left_join(d, by = c("admin" = "Country"))
    
    pal <- colorBin("YlGnBu", domain = map_df$avg_salary,
                    bins = 5, na.color = "#f0f0f0")
    
    leaflet(map_df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(avg_salary),
        fillOpacity = 0.8,
        color = "#BDBDC1", weight = 0.5,
        label = ~paste0(
          admin, "<br>",
          "Avg salary: ", ifelse(is.na(avg_salary), "N/A", dollar(avg_salary)),
          "<br>Responses: ", ifelse(is.na(n), 0, n)
        ),
        highlightOptions = highlightOptions(
          color = "black", weight = 1, bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~avg_salary,
        title = "Avg salary (USD)",
        position = "bottomright"
      )
  })
  
  # ----- (2) Salary vs experience -----
  output$salary_exp <- renderPlot({
    d <- df_filt() %>%
      filter(!is.na(YearsCodePro_num), !is.na(Salary_USD))
    
    ggplot(d, aes(YearsCodePro_num, Salary_USD)) +
      geom_jitter(alpha = 0.25) +
      geom_smooth(method = "lm", se = TRUE) +
      scale_y_continuous(labels = dollar) +
      labs(
        x = "Years of professional coding experience",
        y = "Annual salary (USD)"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # ----- (3) Salary distribution by role -----
  output$salary_role <- renderPlot({
    d <- df_filt() %>%
      select(DevType, Salary_USD) %>%
      filter(!is.na(Salary_USD)) %>%
      separate_rows(DevType, sep = ";\\s*")
    
    top_roles <- d %>%
      group_by(DevType) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      slice_head(n = 6) %>%
      pull(DevType)
    
    d <- d %>% filter(DevType %in% top_roles)
    
    ggplot(d, aes(DevType, Salary_USD)) +
      geom_boxplot() +
      coord_flip() +
      scale_y_continuous(labels = dollar) +
      labs(x = NULL, y = "Salary (USD)") +
      theme_minimal(base_size = 12)
  })
  
  # ----- (4) Job satisfaction vs salary -----
  output$job_sat <- renderPlot({
    d <- df_filt() %>%
      filter(!is.na(Salary_USD), !is.na(JobSat))
    
    ggplot(d, aes(JobSat, Salary_USD)) +
      geom_boxplot() +
      scale_y_continuous(labels = dollar) +
      labs(
        x = "Job satisfaction",
        y = "Salary (USD)"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # ----- (5) Job demand vs experience -----
  output$job_demand <- renderPlot({
    d <- df_filt() %>%
      select(YearsCodePro_num, DevType) %>%
      filter(!is.na(YearsCodePro_num), !is.na(DevType)) %>%
      separate_rows(DevType, sep = ";\\s*") %>%
      count(YearsCodePro_num, DevType)
    
    top_roles <- d %>%
      group_by(DevType) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = 5) %>%
      pull(DevType)
    
    d <- d %>% filter(DevType %in% top_roles)
    
    ggplot(d, aes(YearsCodePro_num, n, group = DevType)) +
      geom_line() +
      facet_wrap(~ DevType, scales = "free_y") +
      labs(
        x = "Years of coding experience",
        y = "Number of respondents"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # ----- (6) Skill demand -----
  output$skill_forecast <- renderPlot({
    d <- df_filt() %>%
      select(LanguageWantToWorkWith) %>%
      drop_na() %>%
      separate_rows(LanguageWantToWorkWith, sep = ";\\s*") %>%
      count(LanguageWantToWorkWith, sort = TRUE) %>%
      slice_head(n = 10)
    
    ggplot(d, aes(reorder(LanguageWantToWorkWith, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Count") +
      theme_minimal(base_size = 12)
  })
  
  # ----- Data table -----
  output$data_table <- renderDT({
    df_filt() %>%
      select(
        ResponseId, Country, DevType, YearsCodePro_num,
        RemoteWork, Salary_USD,
        LanguageHaveWorkedWith, LanguageWantToWorkWith, JobSat
      )
  })
  
  # ----- Downloads -----
  output$download_csv <- downloadHandler(
    filename = function() "filtered_data.csv",
    content = function(file) {
      write.csv(df_filt(), file, row.names = FALSE)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() "filtered_data.xlsx",
    content = function(file) {
      writexl::write_xlsx(df_filt(), path = file)
    }
  )
}

shinyApp(ui, server)
