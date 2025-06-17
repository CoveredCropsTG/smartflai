library(tidyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(janitor)
library(scales)
library(DBI)
library(RSQLite)

# Establish connection
db_path <- normalizePath("database/SmartflAI_database.sqlite", mustWork = TRUE)
con <- dbConnect(RSQLite::SQLite(), db_path)
onStop(function() {
  dbDisconnect(con)
})

# Load heatmap module
source("modules/Heatmap_Module.R")

ui <- fluidPage(
  titlePanel("SmartFlAI - Greenhouse Whitefly Population Dynamics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("table", "Select Table:", choices = dbListTables(con)),
      radioButtons("display_type", "Display Type:", choices = c("Weekly"="weekly","Cumulative"="cumulative")),
      uiOutput("week_selector"),
      uiOutput("trend_span_ui"),
      checkboxInput("show_map", "Show map", value = FALSE),
      h4("Variance-Mean Ratio (VMR)"),
      verbatimTextOutput("vmr_south"),
      verbatimTextOutput("vmr_north")
      ),
    mainPanel(
      conditionalPanel(
        "input.table == 'GER5'",
        plotOutput("Trial", height="500px")
      ),
      conditionalPanel(
        "input.table != 'GER5'",
        fluidRow(
          column(5, h4("South"), plotOutput("South", height="400px")),
          column(2, uiOutput("table_selector")),
          column(5, h4("North"), plotOutput("North", height="400px"))
        )
      ),
      fluidRow(
        column(6,
           wellPanel(
                h4("South -> Trends over time"),
                radioButtons("south_scope", "Scope", choices = c("Zone average", "Single cell", "Custom group")),
                conditionalPanel("input.south_scope == 'Single cell'", selectInput("south_cell", "Cell:", choices = NULL)),
                conditionalPanel("input.south_scope == 'Custom group'", selectizeInput("south_cells", "Cells:", choices = NULL, multiple = TRUE, options = list(placeholder="Pick 2+ cells"))),
                plotOutput("trend_south", height = "250px")
               )
          ),
        column(6,
           wellPanel(
                h4("North -> Trends over time"), radioButtons("north_scope", "Scope", choices = c("Zone average", "Single cell", "Custom group")),
                conditionalPanel("input.north_scope == 'Single cell'", selectInput("north_cell", "Cell:", choices = NULL)),
                conditionalPanel("input.north_scope == 'Custom group'", selectizeInput("north_cells", "Cells:", choices = NULL, multiple = TRUE, options = list(placeholder="Pick 2+ cells"))),
                plotOutput("trend_north", height = "250px")
           ),
        fluidRow(
            column(12,
                  wellPanel(
                    h4("VMR Trend by Zone"),
                    plotOutput("vmr_trend", height = "300px")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # --------------------------------------------------
  # 1) Retrieving data from DB
  table_names <- reactive({
    dbListTables(con)
  })
  
  # --------------------------------------------------
  # 2) Load & process data
  uploaded_data <- reactive({
    req(input$table)
    dbReadTable(con, input$table) %>%
      clean_names()
  })
  
  processed_data <- reactive({
    df <- uploaded_data()
    req(all(c("date","iso_week","datapoint","count") %in% names(df)))
    df$date <- as.Date(df$date)
    if (input$display_type=="weekly") {
      req(input$week)
      df <- filter(df, iso_week ==input$week)
    } else {
      df <- df %>% group_by(datapoint) %>%
        summarize(count=sum(count,na.rm=TRUE), .groups="drop")
    }
    df
  })
  output$week_selector <- renderUI({
    req(uploaded_data())
    weeks <- unique(uploaded_data()$iso_week)
    selectInput("week","ISO week:", choices = weeks)
  })
  
  output$trend_span_ui <- renderUI({
    req(uploaded_data())
    weeks <- unique(uploaded_data()$iso_week)
    max_n <- length(weeks)
    sliderInput(
      inputId = "trend_span",
      label = "Weeks to plot:",
      min = 1,
      max = max_n,
      value = min(5, max_n),
      step = 1
    )
  })
  
  # --------------------------------------------------
  # 3) Trend weekly summary
  weekly_data <- reactive({
    req(uploaded_data())
    uploaded_data() %>%
      mutate(date = as.Date(date)) %>%
      group_by(iso_week, datapoint) %>%
      summarize(count = sum(count, na.rm=TRUE), .groups="drop")
  })
  
  # --------------------------------------------------
  # 4) Populate selectors for trend analysis
  observe({
    cells_south <- sort(selected_grids()[["South"]]$cell)
    cells_north <- sort(selected_grids()[["North"]]$cell)
    updateSelectInput(session, "south_cell", choices = cells_south)
    updateSelectizeInput(session, "south_cells", choices = cells_south)
    updateSelectInput(session, "north_cell", choices = cells_north)
    updateSelectizeInput(session, "north_cells", choices = cells_north)
  })
  
  
  # --------------------------------------------------
  # 5) Grid definitions
  generate_grid <- function(rows, cols, flip=FALSE) {
    g <- expand.grid(row=rows, col=cols)
    if (flip) g %>% mutate(cell=paste0(tolower(col),row))
    else      g %>% mutate(cell=paste0(tolower(row),col))
  }
  selected_grids <- reactive({
    req(input$table)
    switch(input$table,
           "HAR6" = list(
             South=generate_grid(LETTERS[1:6], 5:1),
             North=generate_grid(LETTERS[7:12], 1:5)
           ),
           "GER2" = list(
             South=generate_grid(LETTERS[1:7], 5:1),
             North=generate_grid(LETTERS[8:14],1:5)
           ),
           "GER5" = list(
             Trial=generate_grid(1:5, LETTERS[1:3], flip=TRUE)
           ),
           # default
           list(
             South=generate_grid(LETTERS[1:5],5:1),
             North=generate_grid(LETTERS[6:10],1:5)
           )
    )
  })
  
  # 6) VMR calculation
  compute_vmr <- function(side_name) {
    df <- processed_data()
    grid_df <- selected_grids()[[side_name]] %>%
      left_join(df, by = c("cell"="datapoint")) %>%
      mutate(count = ifelse(is.na(count), 0, count))
    counts <- grid_df$count
    mu <- mean(counts)
    var <- var(counts)
    vmr <- if (mu == 0) NA else var/mu
    list(mean = mu, variance = var, vmr = vmr)
  }
  
  output$vmr_south <- renderText({
    req(input$table, input$week, processed_data())
    v <- compute_vmr("South")
    sprintf("South -> mean: %.2f, var: %.2f, VMR: %.2f",
            v$mean, v$variance, v$vmr)
  })
  output$vmr_north <- renderText({
    req(input$table, input$week, processed_data())
    v <- compute_vmr("North")
    sprintf("North -> mean: %.2f, var: %.2f, VMR: %.2f",
            v$mean, v$variance, v$vmr)
  })
  
  # 6b) VMR trend
  vmr_over_time <- reactive({
    req(weekly_data())
    df <- weekly_data() 
    zones <- c("South", "North")
    vmr_list <- list()
    
    for (z in zones) {
    zone_cells <- selected_grids()[[z]]$cell
    zone_df <- df %>%
        filter(datapoint %in% zone_cells) %>%
        group_by(iso_week) %>%
        summarize(
          mean = mean(count, na.rm = TRUE),
          var = var(count, na.rm = TRUE),
          .groups = "drop"
          ) %>%
        mutate(
          vmr = ifelse(mean == 0, NA, var/mean),
          zone = z
        )
      vmr_list[[z]] <- zone_df
    }
    
  bind_rows(vmr_list$South, vmr_list$North)
  })
  
  # 7) Map toggle
  observeEvent(input$show_map, {
    if( input$show_map) {
      showModal(modalDialog(
        imageOutput("image_display", width = "125%", height = "auto"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      removeModal()
    }
  })
  
  # --------------------------------------------------
  # 8) Colour scales
  default_colors <- c("white","green","yellow","orange","red","purple")
  default_breaks <- c(0,20,50,100,150,200)
  bsi_colors     <- c("white","red","orange","yellow","green")
  bsi_breaks     <- c(0,25,50,75,100)
  
  # --------------------------------------------------
  # 9) Heatmap plots
    output$North <- renderPlot({
    req(input$table!="GER5", processed_data())
    grid_df <- selected_grids()[["North"]] %>%
      left_join(processed_data(), by=c("cell"="datapoint")) %>%
      mutate(count = ifelse(is.na(count),0,count))
    
    is_bsi <- input$table %in% c("HAR4e","HAR5e")
    cols   <- if (is_bsi) bsi_colors   else default_colors
    brks   <- if (is_bsi) bsi_breaks   else default_breaks
    lbl    <- if (is_bsi) "Black Scale Index %" else "Count"
    
    
    ggplot(grid_df, aes(
      x = factor(col, levels = 1:5), 
      y = factor(row, levels = rev(unique(row))),
      fill = count      
    )) +
      geom_tile(color="black") +
      geom_text(aes(label=ifelse(count>0,count,"")), size = 4) +
      scale_fill_gradientn(
        colors = cols,
        values = scales::rescale(brks),
        limits = c(0, max(grid_df$count,na.rm = TRUE))
      ) +
      theme_minimal() +
      labs(title = paste(input$table, "– North "),
           fill = lbl) +
      theme(
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.position = "right")
  })
  
  #9a - South exception
    output$South <- renderPlot({
    req(input$table!="GER5", processed_data())
    grid_df <- selected_grids()[["South"]] %>%
      left_join(processed_data(), by=c("cell"="datapoint")) %>%
      mutate(count = ifelse(is.na(count),0,count))
    
    is_bsi <- input$table %in% c("HAR4e","HAR5e")
    cols   <- if (is_bsi) bsi_colors   else default_colors
    brks   <- if (is_bsi) bsi_breaks   else default_breaks
    lbl    <- if (is_bsi) "Black Scale Index %" else "Count"
    
    
    ggplot(grid_df, aes(
      x = factor(col, levels = c(5,4,3,2,1)), #reversed order for South
      y = factor(row, levels = rev(unique(row))),
      fill = count      
    )) +
      geom_tile(color="black") +
      geom_text(aes(label=ifelse(count>0,count,"")), size = 4) +
      scale_fill_gradientn(
        colors = cols,
        values = scales::rescale(brks),
        limits = c(0, max(grid_df$count,na.rm = TRUE))
      ) +
      theme_minimal() +
      labs(title = paste(input$table, "– South "),
           fill = lbl) +
      theme(
        axis.title = element_blank(),
        axis.text = element_text(size=12),
        legend.position = "right")
  })
  
  # 9b - GER5 Trial exception
  output$Trial <- renderPlot({
    req(input$table=="GER5", processed_data())
    df <- selected_grids()[["Trial"]] %>%
      left_join(processed_data(), by=c("cell"="datapoint")) %>%
      mutate(count=replace_na(count,0))
    make_heatmap(
      df        = df,
      x_levels  = LETTERS[1:3],
      y_levels  = 1:5,
      title     = "GER5 Trial",
      fill_label= "count",
      cols      = default_colors,
      brks      = default_breaks
    )
  })
  
  # --------------------------------------------------
  # 10) Trend plots
  render_trend <- function(side_name, scope_input, cell_input, cells_input) {
    renderPlot({
      req(weekly_data(), input[[scope_input]], input$trend_span)
      
      df_full <- weekly_data()
      all_wks <- sort(unique(df_full$iso_week))
      recent <- tail(all_wks, input$trend_span)
      df <- df_full %>%
        filter(
          iso_week %in% recent,
          datapoint %in% selected_grids()[[side_name]]$cell)
      
      plot_df <- switch(input[[scope_input]],
                        "Zone average" = df %>%
                          group_by(iso_week) %>%
                          summarize(avg = mean(count), .groups ="drop"),
                        
                        "Single cell" = df %>% 
                          filter(datapoint==input[[cell_input]]),
                        
                        "Custom group" = df %>%
                          filter(datapoint %in% input[[cells_input]]) %>%
                          group_by(iso_week) %>%
                          summarize(avg = mean(count), .groups ="drop")
                        )
      
      p <- if (input[[scope_input]] %in% c("Zone average", "Custom group")) {
        ggplot(plot_df, aes(x = iso_week, y = avg)) +
          ylab("Average Count")
      } else {
        ggplot(plot_df, aes(x = iso_week, y = count, color = datapoint)) +
        ylab("Count")
      }
      
      p +
        geom_line(size = 1) +
        scale_x_continuous(
          breaks = function(x) pretty(x, n = input$trend_span),
          labels = scales::label_number(accuracy = 1)
        ) +
        scale_y_continuous(
          breaks = scales::pretty_breaks(n = 6),
          labels = scales::label_number(accuracy = 1)
        ) +
        labs(
          x = "ISO Week",
          title = paste(input$table, side_name, "trend")
        ) +
        theme_minimal()
      })
  }
  
  output$trend_south <- render_trend(
    side_name = "South",
    scope_input = "south_scope",
    cell_input = "south_cell",
    cells_input = "south_cells"
  )
  
  output$trend_north <- render_trend(
    side_name = "North",
    scope_input = "north_scope",
    cell_input = "north_cell",
    cells_input = "north_cells"
  )
  
  # 10a) Plot VMR trend
  output$vmr_trend <- renderPlot({
   df <- vmr_over_time()
   req(nrow(df) > 0)
    
    ggplot(df, aes(x = iso_week, y = vmr, color = zone, group = zone)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(x = "ISO Week", y = "VMR", title = "Variance-Mean Ratio Trend") +
      scale_x_continuous(breaks = pretty(df$iso_week)) +
      scale_y_continuous(breaks = pretty(df$vmr)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  
  # -------------------------------------------------- 
  # 11) Grid map images
  output$image_display <- renderImage({
    req(input$table)
    img <- switch(input$table,
                  "HAR4" = "www/har4.jpg", 
                  "HAR4-Enc" = "www/har4.jpg", 
                  "HAR5" ="www/har5.jpg",
                  "HAR5-Enc" ="www/har5.jpg",
                  "HAR6" ="www/har6.jpg", 
                  "GER2" ="www/ger2.jpg",
                  "GER5" = "www/ger5.jpg", 
                  NULL)
    if (is.null(img)) return(NULL)
    list(src=img, contentType="image/jpeg", width="125%")
  }, deleteFile=FALSE)
}

shinyApp(ui, server)
