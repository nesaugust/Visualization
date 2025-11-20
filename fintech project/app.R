# ------------------------------------------------------------
# Credit Scoring Dashboard (EN) – mobile-friendly + fixes
# ------------------------------------------------------------

library(shiny)
library(readxl)
library(dplyr)
library(plotly)
library(bslib)
library(scales)
library(DT)

fmt_money <- function(x) ifelse(is.na(x), "-", paste0("$", formatC(x, format = "f", digits = 0, big.mark = ",")))
fmt_pct   <- function(x) paste0(round(x, 1), "%")
safe_pull <- function(df, col) if (col %in% names(df)) df[[col]] else NA

ui <- fluidPage(
  theme = bs_theme(version = 5, base_font = font_google("Inter"),
                   primary = "#0F3D69", bg = "#F7F9FC", fg = "#1B263B"),
  tags$head(tags$style(HTML("
    .card {background:#fff;border-radius:18px;padding:16px;
           box-shadow:0 6px 14px rgba(0,0,0,0.06);margin-bottom:20px;}
    .title-bar {background:#0F3D69;color:#fff;font-weight:700;
                text-align:center;padding:10px;border-radius:12px;margin-bottom:12px;}
    .center {text-align:center;}
    .muted{color:#6c757d;}
    #toprow {display:flex;gap:16px;flex-wrap:wrap;}
    #leftcol, #rightcol {display:flex;flex-direction:column;}
    #leftcol .card, #rightcol .card {flex:1;}
    @media (min-width: 992px){ #leftcol{flex:0 0 60%;} #rightcol{flex:0 0 38%;} }
    @media (max-width: 991.98px){ #toprow {flex-direction:column;} }
    /* Score cards inside User Profile */
    .stats {display:flex;gap:12px;flex-wrap:wrap;}
    .stat {
  flex: 1 1 30%;
  background: #F2F5FA;
  border-radius: 14px;
  padding: 12px 14px;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;     /* centers horizontally */
  text-align: center;      /* centers text inside */
}
    .stat-label {font-size:.9rem;color:#4e5d72;margin-bottom:4px;}
    .stat-value {font-size:1.4rem;font-weight:700;color:#0F2239;}
  "))),
  sidebarLayout(
    sidebarPanel(width = 3,
                 div(class="title-bar","⚙️ Settings"),
                 div(class="center muted",
                     HTML("Reading file: <code>scored_dataset_randomforest_thresholded_after.xlsx</code>")
                 ),
                 hr(), uiOutput("row_picker")
    ),
    mainPanel(width = 9,
              # ---- HEADER ----
              div(class="center",
                  div(style="background:#0F3D69;color:white;font-weight:700;
                 font-size:1.6rem;padding:14px 0;border-radius:12px;
                 margin-top:20px;max-width:500px;margin-left:auto;margin-right:auto;",
                      "Hi, Customer!"
                  ),
                  h3(style="margin:10px 0 8px 0;color:#1B263B;","Credit Scoring Dashboard"),
                  tags$hr(style="margin-top:6px;margin-bottom:12px;border-top:1px solid #c8d1dc;")
              ),
              
              # ---- Top two boxes ----
              div(id="toprow",
                  div(id="leftcol",
                      div(class="card",
                          div(class="title-bar","Customer Profile"),
                          uiOutput("profile_stats")
                      )
                  ),
                  div(id="rightcol",
                      div(class="card",
                          div(class="title-bar","Credit Scoring"),
                          plotlyOutput("gauge", height = "125px"),
                          uiOutput("score_block")
                      )
                  )
              ),
              
              # ---- Drivers ----
              div(class="card",
                  div(class="title-bar","What Influences Your Score"),
                  plotlyOutput("drivers_plot", height="200px")
              ),
              
              # ---- AI Explanation ----
              div(class="card",
                  div(class="title-bar","AI Explanation"),
                  uiOutput("insight"),
                  div(class="muted center",
                      style = "font-size:0.8rem; margin-top:6px; color:#7a8899;",
                      "Note: Explanations are informational and intended to help you understand the outcome."
                  )
              ),
              
              # ---- Data preview ----
              div(class="card",
                  div(class="title-bar","Data (preview)"),
                  DTOutput("tbl")
              )
    )
  )
)

server <- function(input, output, session){
  
  data_raw <- reactive({
    path <- "scored_dataset_randomforest_thresholded_after.xlsx"
    validate(need(file.exists(path), paste0("File not found: ", path)))
    read_excel(path)
  })
  
  output$row_picker <- renderUI({
    df <- data_raw(); n <- nrow(df)
    tagList(
      sliderInput("row", "Select record", min = 1, max = n, value = 1, step = 1, width = "100%"),
      div(class="center muted", paste("Total records:", n))
    )
  })
  
  r <- reactive({
    df <- data_raw(); req(input$row)
    df[input$row, , drop = FALSE]
  })
  
  # ----- USER PROFILE as 3 score cards -----
  output$profile_stats <- renderUI({
    df <- r()
    dti_val <- suppressWarnings(as.numeric(safe_pull(df, "total_dti")))
    if (!is.na(dti_val) && dti_val <= 1) dti_pct <- dti_val*100 else dti_pct <- dti_val
    
    age   <- suppressWarnings(as.integer(safe_pull(df, "AGE")))
    income<- fmt_money(suppressWarnings(as.numeric(safe_pull(df, "annual_inc"))))
    util  <- ifelse(is.na(dti_pct), "-", fmt_pct(dti_pct))
    
    tags$div(class="stats",
             div(class="stat",
                 div(class="stat-label","Age"),
                 div(class="stat-value", age)
             ),
             div(class="stat",
                 div(class="stat-label","Annual Income"),
                 div(class="stat-value", income)
             ),
             div(class="stat",
                 div(class="stat-label","Utilization"),
                 div(class="stat-value", util)
             )
    )
  })
  
  # ----- GAUGE -----
  # helper: color by category
  cat_color <- function(cat) {
    if (is.na(cat)) return("#1B263B")
    lc <- tolower(cat)
    if (grepl("poor|low", lc))    return("#D9534F")
    if (grepl("good", lc))        return("#2A9D8F")
    if (grepl("fair|standard", lc)) return("#1B263B")
    "#1B263B"
  }
  
  # ---- GAUGE (smaller + no cutoff + colored number) ----
  output$gauge <- renderPlotly({
    df    <- r()
    score <- suppressWarnings(as.numeric(safe_pull(df, "Score")))
    catg  <- as.character(safe_pull(df, "Risk_Category"))
    col   <- cat_color(catg)
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = score,
      number = list(font = list(size = 33, color = col)),
      domain = list(x = c(0, 1), y = c(0, 0.80)),
      gauge = list(
        shape = "angular",
        axis  = list(range = c(300, 850), tickfont = list(size = 9)),
        bar   = list(thickness = 0.18),
        steps = list(
          list(range = c(300, 580), color = "#e76f51"),
          list(range = c(580, 700), color = "#e9c46a"),
          list(range = c(700, 850), color = "#2a9d8f")
        )
      )
    ) |>
      layout(
        margin   = list(l = 20, r = 20, t = 8, b = 0),
        autosize = TRUE
      )
  })
  
  # ---- CATEGORY TEXT (colored) + centered probability ----
  output$score_block <- renderUI({
    df   <- r()
    catg <- as.character(safe_pull(df, "Risk_Category"))
    pd   <- suppressWarnings(as.numeric(safe_pull(df, "PD")))
    prob_good <- ifelse(is.na(pd), NA, 1 - pd)
    col  <- cat_color(catg)
    
    tagList(
      tags$h3(style = paste0("text-align:center; color:", col, "; font-weight:700;"),
              ifelse(is.na(catg), "", toupper(catg))),
      tags$p(class = "muted center",
             sprintf("Probability: %s", ifelse(is.na(prob_good), "-", round(prob_good, 2))))
    )
  })
  
  
  # ----- DRIVERS (with BNPL Payment Status) -----
  output$drivers_plot <- renderPlotly({
    df_all <- data_raw(); df <- r()
    emp_raw  <- suppressWarnings(as.numeric(df$EMPLOYMENT_YEARS))
    cir_raw  <- suppressWarnings(as.numeric(df$credit_to_income_ratio))
    bnpl_raw <- suppressWarnings(as.numeric(df$bnpl_default))  # 0 = on time, 1 = late
    
    # robust scaling for CIR
    p5  <- suppressWarnings(quantile(df_all$credit_to_income_ratio, 0.05, na.rm = TRUE))
    p95 <- suppressWarnings(quantile(df_all$credit_to_income_ratio, 0.95, na.rm = TRUE))
    cir_norm <- ifelse(is.na(cir_raw) | is.na(p5) | is.na(p95) | p95 <= p5, 0.5,
                       pmin(pmax((cir_raw - p5) / (p95 - p5), 0), 1))
    
    emp_norm  <- ifelse(is.na(emp_raw), 0.5, pmin(emp_raw/10, 1))  # cap 10 yrs
    bnpl_norm <- ifelse(is.na(bnpl_raw), 0.0, ifelse(bnpl_raw == 1, 1, 0))
    
    y  <- c("Employment Years","Credit to Income Ratio","BNPL Payment Status")
    x  <- c(emp_norm, cir_norm, bnpl_norm)
    tx <- c(
      ifelse(is.na(emp_raw), "—", paste0(emp_raw, " year")),
      ifelse(is.na(cir_raw), "—", paste0(round(cir_raw, 2), "×")),
      ifelse(is.na(bnpl_raw), "—",
             ifelse(bnpl_raw == 1, "Late Payment", "On Time"))
    )
    
    plot_ly(
      x = x, y = y, type = "bar", orientation = "h",
      text = tx, textposition = "outside",
      textfont = list(size = 11),
      cliponaxis = FALSE,
      marker = list(color = "#457B9D")
    ) |>
      layout(
        xaxis = list(title = "Relative level",
                     range = c(0, 1.05),
                     tickfont = list(size=10)),
        yaxis = list(title = "", tickfont = list(size=12), automargin = TRUE),
        margin = list(l=110, r=90, t=10, b=10),
        autosize = TRUE
      )
  })
  
  
  # ----- AI EXPLANATION -----
  output$insight <- renderUI({
    df <- r()
    catg  <- tolower(as.character(safe_pull(df, "Risk_Category")))
    dti   <- suppressWarnings(as.numeric(safe_pull(df, "total_dti"))); if (!is.na(dti) && dti <= 1) dti <- dti*100
    cir   <- suppressWarnings(as.numeric(safe_pull(df, "credit_to_income_ratio")))
    empy  <- suppressWarnings(as.numeric(safe_pull(df, "EMPLOYMENT_YEARS")))
    bnpl  <- suppressWarnings(as.numeric(safe_pull(df, "bnpl_default")))
    
    msgs <- c(
      if (!is.na(catg) && grepl("good", catg)) "Your profile indicates healthy financial stability."
      else if (!is.na(catg) && grepl("fair", catg)) "Your profile shows opportunities to improve toward a higher credit tier."
      else "Your profile is currently in a higher-risk tier. Addressing the items below can help improve your score."
    )
    if (!is.na(dti) && dti >= 40) msgs <- c(msgs, "Your <strong>Debt-to-Income (DTI)</strong> is relatively high; reducing monthly obligations can help.")
    if (!is.na(cir)) {
      q75 <- suppressWarnings(quantile(safe_pull(data_raw(), "credit_to_income_ratio"), 0.75, na.rm=TRUE))
      if (!is.na(q75) && cir >= q75) msgs <- c(msgs, "<strong>Credit to Income Ratio</strong> is high compared with most applicants.")
    }
    if (!is.na(bnpl) && bnpl == 1) msgs <- c(msgs, "There is a history of <strong>BNPL default</strong>; consistent on-time payments will strengthen your profile.")
    if (!is.na(empy) && empy >= 3) msgs <- c(msgs, "<strong>Employment tenure</strong> contributes positively to your score.")
    
    tagList(tags$ul(lapply(msgs, function(m) tags$li(HTML(m)))))
  })
  
  output$tbl <- renderDT({
    datatable(data_raw(), options = list(pageLength = 5, scrollX = TRUE))
  })
}

shinyApp(ui, server)
