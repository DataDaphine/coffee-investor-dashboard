library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(forcats)
library(shinyWidgets)
library(tidyr)


# Data loader

load_rda_first_object <- function(path) {
  env <- new.env(parent = emptyenv())
  nm <- load(path, envir = env)
  
  if (length(nm) == 0) {
    stop("No objects found in the .rda file.")
  }
  
  obj <- env[[nm[1]]]
  
  if (!is.data.frame(obj)) {
    stop("The first object in the .rda file is not a data frame.")
  }
  
  obj
}

normalize_coffee_data <- function(df) {
  names(df) <- tolower(names(df))
  names(df) <- gsub("[[:space:][:punct:]]+", "_", names(df))
  names(df) <- gsub("_+", "_", names(df))
  names(df) <- gsub("^_|_$", "", names(df))
  
  numeric_candidates <- c(
    "total_cup_points", "aroma", "flavor", "aftertaste", "acidity",
    "body", "balance", "uniformity", "clean_cup", "moisture",
    "sweetness", "num_defects"
  )
  
  for (col in intersect(numeric_candidates, names(df))) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  
  char_candidates <- c("country_of_origin", "specialty", "quality")
  for (col in intersect(char_candidates, names(df))) {
    df[[col]] <- as.character(df[[col]])
  }
  
  if (!"country_of_origin" %in% names(df)) {
    stop("Expected column 'country_of_origin' was not found.")
  }
  
  if (!"total_cup_points" %in% names(df)) {
    stop("Expected column 'total_cup_points' was not found.")
  }
  
  if (!"specialty" %in% names(df)) {
    df$specialty <- ifelse(df$total_cup_points >= 80, "Specialty", "Not Specialty")
  }
  
  if (!"quality" %in% names(df)) {
    df$quality <- case_when(
      df$total_cup_points >= 84 ~ "High",
      df$total_cup_points >= 80 ~ "Medium",
      TRUE ~ "Low"
    )
  }
  
  df$country_of_origin <- ifelse(
    is.na(df$country_of_origin) | df$country_of_origin == "",
    "Unknown",
    df$country_of_origin
  )
  
  df$specialty <- ifelse(
    is.na(df$specialty) | df$specialty == "",
    "Unknown",
    df$specialty
  )
  
  df$quality <- ifelse(
    is.na(df$quality) | df$quality == "",
    "Unknown",
    df$quality
  )
  
  df
}


# Loading  data

raw_df <- load_rda_first_object("data/coffee_rating_data.rda")
coffee <- normalize_coffee_data(raw_df)

score_cols <- intersect(
  c(
    "aroma", "flavor", "aftertaste", "acidity",
    "body", "balance", "uniformity", "clean_cup", "sweetness"
  ),
  names(coffee)
)

country_choices <- sort(unique(coffee$country_of_origin))
quality_choices <- sort(unique(coffee$quality))
specialty_choices <- sort(unique(coffee$specialty))


# UI

ui <- page_sidebar(
  title = div(
    tags$div("Coffee Ratings Investor Dashboard", style = "font-size: 1.6rem; font-weight: 800;"),
    tags$div("Full-scale market scan for quality and geography concentration", style = "color: #7f8c8d; font-size: 0.95rem;")
  ),
  
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly",
    "card-cap-bg" = "#f8f9fa"
  ),
  
  fillable = FALSE,
  
  sidebar = sidebar(
    width = 300,
    open = TRUE,
    tags$h5("Global Filters", class = "mb-3"),
    sliderInput("score_range", "Total Cup Points", 
                min = floor(min(coffee$total_cup_points, na.rm = TRUE)), 
                max = ceiling(max(coffee$total_cup_points, na.rm = TRUE)), 
                value = c(70, 95), step = 0.5),
    virtualSelectInput("country", "Country of Origin", choices = country_choices, 
                       selected = country_choices, multiple = TRUE, search = TRUE),
    checkboxGroupInput("specialty", "Specialty Status", choices = specialty_choices, selected = specialty_choices, inline = TRUE),
    checkboxGroupInput("quality", "Quality Tier", choices = quality_choices, selected = quality_choices, inline = TRUE),
    hr(),
    numericInput("top_n", "Top countries to display", value = 10),
    sliderInput("min_obs", "Min records per country", min = 1, max = 100, value = 5),
    radioButtons("metric", "Focus metric",
                 choices = c("Avg Score" = "avg_score", "Avg Quality" = "avg_quality")),
    actionBttn("reset", "Reset Analysis", style = "fill", color = "primary", block = TRUE)
  ),
  
  div(class = "mb-4",
      layout_column_wrap(
        width = 1/4,
        fill = FALSE,
        uiOutput("vb_obs"),
        uiOutput("vb_countries"),
        uiOutput("vb_avg_score"),
        uiOutput("vb_specialty_share")
      )
  ),
  
  # 2. Market Map 
  card(
    height = 650, 
    class = "shadow-sm mb-4",
    card_header(strong("I. Market Map: Quality vs. Volume Concentration")),
    card_body(
      plotlyOutput("country_plot", height = "500px")
    ),
    card_footer(class = "text-muted", "Story: Identifies stable quality benchmarks by weighing volume against score.")
  ),
  
  # 3. Sensory Profile 
  card(
    height = 600,
    class = "shadow-sm mb-4",
    card_header(strong("II. Sensory Profile: Attribute Differentiation")),
    card_body(
      plotlyOutput("radar_like_plot", height = "450px")
    ),
    card_footer(class = "text-muted", "Story: Shows the physical taste attributes that define the specialty segment.")
  ),
  
  # 4. Economics Signal 
  card(
    height = 600,
    class = "shadow-sm mb-4",
    card_header(strong("III. Economic Signal: Score Distribution")),
    card_body(
      plotlyOutput("specialty_plot", height = "450px")
    ),
    card_footer(class = "text-muted", "Story: Statistical spread of cup points by market category.")
  ),
  
  # 5. Detail Table
  card(
    class = "shadow-sm mb-5",
    card_header(strong("IV. Country Detail Master Table")),
    card_body(
      DTOutput("summary_table")
    )
  )
)


# Server

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSliderInput(
      session,
      "score_range",
      value = c(
        floor(min(coffee$total_cup_points, na.rm = TRUE)),
        ceiling(max(coffee$total_cup_points, na.rm = TRUE))
      )
    )
    
    updateVirtualSelect(
      session,
      "country",
      selected = country_choices
    )
    
    updateCheckboxGroupInput(
      session,
      "specialty",
      selected = specialty_choices
    )
    
    updateCheckboxGroupInput(
      session,
      "quality",
      selected = quality_choices
    )
    
    updateNumericInput(
      session,
      "top_n",
      value = 10
    )
    
    updateSliderInput(
      session,
      "min_obs",
      value = 5
    )
    
    updateRadioButtons(
      session,
      "metric",
      selected = "avg_score"
    )
  })
  
  filtered <- reactive({
    coffee |>
      filter(
        !is.na(total_cup_points),
        total_cup_points >= input$score_range[1],
        total_cup_points <= input$score_range[2],
        country_of_origin %in% input$country,
        specialty %in% input$specialty,
        quality %in% input$quality
      )
  })
  
  country_summary <- reactive({
    filtered() |>
      group_by(country_of_origin) |>
      summarise(
        n = n(),
        avg_score = mean(total_cup_points, na.rm = TRUE),
        avg_quality = mean(
          case_when(
            quality == "Low" ~ 1,
            quality == "Medium" ~ 2,
            quality == "High" ~ 3,
            TRUE ~ NA_real_
          ),
          na.rm = TRUE
        ),
        avg_defects = if ("num_defects" %in% names(.)) mean(num_defects, na.rm = TRUE) else NA_real_,
        specialty_share = mean(specialty == "Specialty", na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(n >= input$min_obs)
  })
  
  top_countries <- reactive({
    metric_name <- input$metric
    
    country_summary() |>
      arrange(desc(.data[[metric_name]])) |>
      slice_head(n = input$top_n)
  })
  
  sensory_profile <- reactive({
    req(length(score_cols) > 0)
    
    filtered() |>
      mutate(segment = specialty) |>
      group_by(segment) |>
      summarise(
        across(all_of(score_cols), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      pivot_longer(
        cols = all_of(score_cols),
        names_to = "attribute",
        values_to = "value"
      )
  })
  
  output$vb_obs <- renderUI({
    value_box(
      title = "Coffees in scope",
      value = comma(nrow(filtered())),
      showcase = NULL,
      theme = "primary"
    )
  })
  
  output$vb_countries <- renderUI({
    value_box(
      title = "Countries represented",
      value = comma(dplyr::n_distinct(filtered()$country_of_origin)),
      showcase = NULL,
      theme = "secondary"
    )
  })
  
  output$vb_avg_score <- renderUI({
    value_box(
      title = "Average cup score",
      value = round(mean(filtered()$total_cup_points, na.rm = TRUE), 2),
      showcase = NULL,
      theme = "success"
    )
  })
  
  output$vb_specialty_share <- renderUI({
    value_box(
      title = "Specialty share",
      value = percent(mean(filtered()$specialty == "Specialty", na.rm = TRUE), accuracy = 0.1),
      showcase = NULL,
      theme = "warning"
    )
  })
  
  output$country_plot <- renderPlotly({
    req(nrow(top_countries()) > 0)
    
    p <- ggplot(
      top_countries(),
      aes(
        x = n,
        y = avg_score,
        text = paste0(
          "Country: ", country_of_origin,
          "<br>Records: ", comma(n),
          "<br>Avg score: ", round(avg_score, 2),
          "<br>Specialty share: ", percent(specialty_share, accuracy = 0.1)
        )
      )
    ) +
      geom_point(size = 4, alpha = 0.8) +
      geom_text(
        aes(label = country_of_origin),
        nudge_y = 0.15,
        size = 3,
        check_overlap = TRUE
      ) +
      labs(
        x = "Number of rated coffees",
        y = "Average total cup points",
        title = "Country depth versus quality"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$radar_like_plot <- renderPlotly({
    req(nrow(sensory_profile()) > 0)
    
    p <- ggplot(
      sensory_profile(),
      aes(
        x = fct_reorder(attribute, value),
        y = value,
        color = segment,
        group = segment,
        text = paste0(
          "Segment: ", segment,
          "<br>Attribute: ", attribute,
          "<br>Average: ", round(value, 2)
        )
      )
    ) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Average sensory score",
        title = "How sensory attributes change by segment"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$specialty_plot <- renderPlotly({
    req(nrow(filtered()) > 0)
    
    p <- ggplot(
      filtered(),
      aes(
        x = specialty,
        y = total_cup_points,
        fill = specialty,
        text = paste0(
          "Specialty: ", specialty,
          "<br>Total cup points: ", round(total_cup_points, 2),
          "<br>Country: ", country_of_origin
        )
      )
    ) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.2) +
      labs(
        x = NULL,
        y = "Total cup points",
        title = "Distribution of scores by specialty status"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$summary_table <- renderDT({
    tbl <- country_summary() |>
      mutate(
        specialty_share = percent(specialty_share, accuracy = 0.1),
        avg_score = round(avg_score, 2),
        avg_quality = round(avg_quality, 2),
        avg_defects = round(avg_defects, 2)
      ) |>
      arrange(desc(avg_score))
    
    datatable(
      tbl,
      rownames = FALSE,
      filter = "top",
      extensions = c("Buttons"),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}

shinyApp(ui, server)