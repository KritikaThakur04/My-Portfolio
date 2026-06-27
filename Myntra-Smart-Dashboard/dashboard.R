library(shiny)
library(shinydashboard)
library(tidyverse)  #Dplyr, ggplot2
library(DT)
library(plotly)

#Load & Clean
df <- read.csv("D:/book_ch/R/myntra.csv")

df <- df %>%
  filter(rating > 0) %>%
  mutate(
    discount_percent = as.numeric(gsub("%", "", discount_percent)),
    discounted_price = as.numeric(gsub(",", "", discounted_price)),
    marked_price     = as.numeric(gsub(",", "", marked_price))
  ) %>%
  # Remove rows with NA prices — these break slider range and filter()
  filter(!is.na(discounted_price), !is.na(marked_price), !is.na(discount_percent))

# Pre-compute global price bounds (used in slider and reset)
PRICE_MIN <- floor(min(df$discounted_price))
PRICE_MAX <- ceiling(max(df$discounted_price))

#UI
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(title = "Online Retail Insights Dashboard"),

  dashboardSidebar(
    h4("Filters"),

    # FIX: sorted categories for usability
    selectInput("category", "Category",
                choices  = sort(unique(df$product_tag)),
                selected = sort(unique(df$product_tag))[1]),

    # FIX: brand choices now update dynamically when category changes
    # (was showing all brands regardless of category before)
    selectInput("brand", "Brand",
                choices  = c("All"),
                selected = "All"),

    # FIX: step=1, floor/ceil, pre="₹" for clean integer values
    sliderInput("priceRange", "Price Range (₹)",
                min   = PRICE_MIN,
                max   = PRICE_MAX,
                value = c(PRICE_MIN, PRICE_MAX),
                step  = 1,
                pre   = "₹"),

    # NEW: minimum rating filter
    sliderInput("minRating", "Minimum Rating",
                min = 1, max = 5, value = 1, step = 0.1),

    actionButton("reset", "Reset Filters", class = "btn-warning btn-sm"),
    br(), br()
  ),

  dashboardBody(

    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #111; }
        .box { border-radius: 12px; }
      "))
    ),

    # KPI row
    fluidRow(
      valueBoxOutput("avgRating"),
      valueBoxOutput("avgDiscount"),
      valueBoxOutput("totalProducts"),
      valueBoxOutput("avgPrice")
    ),

    # Fallback message
    fluidRow(
      column(12, h4(textOutput("noDataMsg"), style = "color: orange; padding-left: 15px;"))
    ),

    # Insights
    fluidRow(
      box(title = "Insights", width = 12, solidHeader = TRUE, status = "primary",
          htmlOutput("insights"))
    ),

    # Best product recommendation
    fluidRow(
      box(title = "Best Product Recommendation", width = 12, solidHeader = TRUE, status = "success",
          DTOutput("bestProduct"))
    ),

    # Charts row 1
    fluidRow(
      box(title = " Top Categories (All Data)", width = 6,
          plotlyOutput("categoryPlot")),

      box(title = "Price vs Discount", width = 6,
          plotlyOutput("scatterPlot"))
    ),

    # Charts row 2
    fluidRow(
      box(title = "Rating Distribution", width = 6,
          plotlyOutput("ratingPlot")),

      box(title = "Top Brands by Count", width = 6,
          plotlyOutput("brandPlot"))
    ),

    # Products table
    fluidRow(
      box(title = "Top Discounted Products", width = 12,
          DTOutput("productTable"))
    )
  )
)

# Server
server <- function(input, output, session) {

  # FIX: Update brand list AND price range when category changes
  # Previously brand dropdown showed all 3,194 brands regardless of category.
  observeEvent(input$category, {
    brands_in_cat <- df %>%
      filter(product_tag == input$category) %>%
      pull(brand_name) %>%
      unique() %>%
      sort()

    updateSelectInput(session, "brand",
                      choices  = c("All", brands_in_cat),
                      selected = "All")

    # Update price slider to match price range of selected category
    cat_prices <- df %>%
      filter(product_tag == input$category) %>%
      pull(discounted_price)

    cat_min <- floor(min(cat_prices, na.rm = TRUE))
    cat_max <- ceiling(max(cat_prices, na.rm = TRUE))

    updateSliderInput(session, "priceRange",
                      min   = cat_min,
                      max   = cat_max,
                      value = c(cat_min, cat_max))
  })

  # Reset button 
  observeEvent(input$reset, {
    first_cat <- sort(unique(df$product_tag))[1]
    updateSelectInput(session, "category", selected = first_cat)
    updateSelectInput(session, "brand",    selected = "All")
    # FIX: reset uses floor/ceiling consistently with slider definition
    updateSliderInput(session, "priceRange",
                      min   = PRICE_MIN,
                      max   = PRICE_MAX,
                      value = c(PRICE_MIN, PRICE_MAX))
    updateSliderInput(session, "minRating", value = 1)
  })

  # core reactive filter 
  filtered_data <- reactive({

    # FIX: req() prevents crashes while inputs are still initialising
    req(input$priceRange, input$category, input$brand, input$minRating)

    price_low  <- input$priceRange[1]
    price_high <- input$priceRange[2]

    # FIX: guard against inverted range when handles are dragged past each other
    if (is.na(price_low) || is.na(price_high) || price_low > price_high) {
      price_low  <- PRICE_MIN
      price_high <- PRICE_MAX
    }

    data_filtered <- df %>%
      filter(
        product_tag      == input$category,
        discounted_price >= price_low,
        discounted_price <= price_high,
        rating           >= input$minRating
      )

    if (input$brand != "All") {
      data_filtered <- data_filtered %>%
        filter(brand_name == input$brand)
    }

    # Fallback: if combined filters produce 0 rows, show full category
    if (nrow(data_filtered) == 0) {
      return(list(
        data     = df %>% filter(product_tag == input$category),
        fallback = TRUE
      ))
    }

    list(data = data_filtered, fallback = FALSE)
  })

  # Helpers
  safe_mean <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(NA_real_)
    mean(x, na.rm = TRUE)
  }

  #KPI Boxes

  output$avgRating <- renderValueBox({
    val <- safe_mean(filtered_data()$data$rating)
    # FIX: was showing raw "NA" before — now shows "N/A"
    label <- if (is.na(val)) "N/A" else round(val, 2)
    valueBox(label, "Avg Rating", icon = icon("star"), color = "yellow")
  })

  output$avgDiscount <- renderValueBox({
    val <- safe_mean(filtered_data()$data$discount_percent)
    label <- if (is.na(val)) "N/A" else paste0(round(val, 1), "%")
    valueBox(label, "Avg Discount", icon = icon("tags"), color = "green")
  })

  output$totalProducts <- renderValueBox({
    valueBox(
      format(nrow(filtered_data()$data), big.mark = ","),
      "Products", icon = icon("shopping-bag"), color = "blue"
    )
  })

  output$avgPrice <- renderValueBox({
    val <- safe_mean(filtered_data()$data$discounted_price)
    label <- if (is.na(val)) "N/A" else paste0("₹", format(round(val), big.mark = ","))
    valueBox(label, "Avg Price", icon = icon("rupee-sign"), color = "purple")
  })

  # Status message
  output$noDataMsg <- renderText({
    if (filtered_data()$fallback)
      "No products match your filters. Showing full category instead."
    else ""
  })

  # Insights
  output$insights <- renderUI({
    data <- filtered_data()$data

    if (nrow(data) == 0) {
      return(HTML("<i>No data available for the current filters.</i>"))
    }

    best_brand <- data %>%
      group_by(brand_name) %>%
      summarise(r = mean(rating, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(r)) %>%
      slice(1) %>%
      pull(brand_name)

    HTML(paste0(
      "<b>Products shown:</b> ",          format(nrow(data), big.mark = ","), "<br>",
      "<b>Average rating:</b> ",          round(mean(data$rating,           na.rm = TRUE), 2), "<br>",
      "<b>Average discount:</b> ",        round(mean(data$discount_percent, na.rm = TRUE), 1), "%<br>",
      "<b>Price range in view:</b> ₹",   format(min(data$discounted_price, na.rm = TRUE), big.mark = ","),
        " – ₹", format(max(data$discounted_price, na.rm = TRUE), big.mark = ","), "<br>",
      "<b>Best brand (avg rating):</b> ", best_brand
    ))
  })

  # Best product
  output$bestProduct <- renderDT({
    data <- filtered_data()$data
    if (nrow(data) == 0) return(datatable(data.frame(Message = "No products found.")))

    datatable(
      data %>%
        arrange(desc(rating), desc(discount_percent)) %>%
        head(1) %>%
        select(product_name, brand_name, discounted_price, discount_percent, rating),
      options  = list(dom = "t"),   # hide pagination for single-row result
      rownames = FALSE
    )
  })

  # Category plot (global — shows overall landscape)
  output$categoryPlot <- renderPlotly({
    ggplotly(
      df %>%
        count(product_tag) %>%
        slice_max(n, n = 10) %>%
        ggplot(aes(reorder(product_tag, n), n,
                   text = paste0(product_tag, ": ", format(n, big.mark = ",")))) +
        geom_bar(stat = "identity", fill = "#00BCD4") +
        coord_flip() +
        labs(x = NULL, y = "Product Count") +
        theme_minimal(base_size = 11),
      tooltip = "text"
    )
  })

  # Price vs Discount scatter 
  output$scatterPlot <- renderPlotly({
    data <- filtered_data()$data
    if (nrow(data) == 0) return(plotly_empty(type = "scatter", mode = "markers"))

    ggplotly(
      ggplot(data, aes(marked_price, discount_percent,
                       text = paste0(product_name, "<br>₹", discounted_price))) +
        geom_point(alpha = 0.5, color = "#F44336", size = 1.5) +
        labs(x = "Marked Price (₹)", y = "Discount (%)") +
        theme_minimal(base_size = 11),
      tooltip = "text"
    )
  })

  # Rating histogram
  output$ratingPlot <- renderPlotly({
    data <- filtered_data()$data
    if (nrow(data) == 0) return(plotly_empty())

    # FIX: dynamic bins — avoids crash when all products have the same rating
    n_bins <- min(20, max(5, length(unique(data$rating))))

    ggplotly(
      ggplot(data, aes(rating)) +
        geom_histogram(bins = n_bins, fill = "#9C27B0", color = "white", linewidth = 0.2) +
        labs(x = "Rating", y = "Count") +
        theme_minimal(base_size = 11)
    )
  })

  #Top brands bar chart (global)
  output$brandPlot <- renderPlotly({
    ggplotly(
      df %>%
        group_by(brand_name) %>%
        summarise(avg_rating = mean(rating, na.rm = TRUE),
                  count      = n(),
                  .groups    = "drop") %>%
        slice_max(count, n = 10) %>%
        ggplot(aes(reorder(brand_name, count), count,
                   text = paste0(brand_name,
                                 "\nProducts: ", count,
                                 "\nAvg rating: ", round(avg_rating, 2)))) +
        geom_bar(stat = "identity", fill = "#FF9800") +
        coord_flip() +
        labs(x = NULL, y = "Product Count") +
        theme_minimal(base_size = 11),
      tooltip = "text"
    )
  })

  # Products table 
  output$productTable <- renderDT({
    data <- filtered_data()$data
    if (nrow(data) == 0) return(datatable(data.frame(Message = "No products found.")))

    datatable(
      data %>%
        arrange(desc(discount_percent)) %>%
        select(product_name, brand_name, marked_price,
               discounted_price, discount_percent, rating),
      filter   = "top",
      rownames = FALSE,
      options  = list(
        pageLength = 10,
        # FIX: prevent long product names from overflowing cells
        columnDefs = list(list(width = "250px", targets = 0))
      )
    )
  })

}

shinyApp(ui, server)
