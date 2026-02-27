#' interactive_dashboard UI Function
#'
#' @noRd
mod_interactive_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(

    ## 1st row: method text on left, interactive chart on the right ----
    fluidRow(
      column(
        width = 6,

        # 1 Method picker
        selectInput(
          inputId = ns("select_method"),
          label   = "Select Method:",
          choices = c("Welfare conversion", "Household allocation", "Subnational definition"),
          selected = "Welfare conversion"
        ),

        # 2 Economy picker
        selectInput(
          inputId = ns("select_economy"),
          label   = "Select Economy:",
          choices = NULL,
          selected = NULL
        ),

        # 2b SN-specific controls (poverty line + granular toggle)
        uiOutput(ns("sn_controls")),

        # 3 dynamic text panel
        uiOutput(ns("method_panel")),
        br(),

        # 4 learn‐more button
        actionButton(ns("learn_more"), "Learn more", class = "btn btn-primary"),
        br(),
        br(),
        
        # 5 download data button
        downloadButton(ns("download_data"), "Download data", class = "btn btn-secondary")
      ),

      column(
        width = 6,
        plotly::plotlyOutput(
          outputId = ns("top_chart"),
          height   = "500px"
        )
      )
    ),

    br(),

    ## Bottom section: conditionally rendered ----
    uiOutput(ns("bottom_section_ui"))

  )
}


#' interactive_dashboard Server Functions
#'
#' @param id    module id
#' @param data  a data.table (or data.frame) containing your economy‐level data
#' @noRd
# R/mod_interactive_dashboard.R

mod_interactive_dashboard_server <- function(
    id,
    data_dm,
    data_stb,
    data_sn,
    data_sn_cross,
    dm_metadata,
    stb_metadata,
    sn_metadata,
    method_override = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ─── 0) External method override from Home page ───────────────────────────────────
    if (!is.null(method_override) && is.reactive(method_override)) {
      observeEvent(method_override(), {
        req(method_override())
        updateSelectInput(session, "select_method", selected = method_override())
      }, ignoreInit = TRUE)
    }

    # ─── 1) Reactive dataset based on method ─────────────────────────────────────────
    dataset <- reactive({
      if (input$select_method == "Welfare conversion") {
        data_dm
      } else if (input$select_method == "Household allocation") {
        data_stb
      } else {
        data_sn
      }
    })

    # ─── 2) Whenever the dataset changes (and at startup), repopulate economy picker ──
    observeEvent(dataset(), {
      countries <- sort(unique(dataset()$country_name))
      updateSelectInput(
        session,
        "select_economy",
        choices  = countries,
        selected = countries[1]
      )
    }, ignoreInit = FALSE)

    # ─── 3) Capture the current selections ─────────────────────────────────────────────────
    selected_method  <- reactive(input$select_method)
    selected_economy <- reactive(input$select_economy)
    current_tab      <- reactiveVal("rankings")
    
    # ─── SN-specific controls ─────────────────────────────────────────────────────
    output$sn_controls <- renderUI({
      req(input$select_method)
      if (input$select_method != "Subnational definition") return(NULL)
      tagList(
        selectInput(
          inputId  = ns("sn_poverty_line"),
          label    = "Select Poverty Line:",
          choices  = c("$2.15", "$3.65", "$6.85"),
          selected = "$2.15"
        ),
        checkboxInput(
          inputId = ns("sn_granular"),
          label   = "Show granular rural/urban classifications",
          value   = FALSE
        )
      )
    })
    
    # Log scale toggle for scatter plot (both axes)
    log_scale <- reactiveVal(FALSE)
    
    # Poverty line filter for scatter plot
    poverty_line_filter <- reactive({
      if (is.null(input$poverty_line_filter)) "all" else input$poverty_line_filter
    })
    
    # SN reporting level selection
    sn_reporting_level <- reactive({
      if (is.null(input$sn_reporting_level)) "national" else input$sn_reporting_level
    })
    
    # Cross-country data reactive: returns appropriate data for bottom charts
    cross_country_data <- reactive({
      if (selected_method() == "Subnational definition") {
        data_sn_cross |>
          collapse::fsubset(reporting_level == sn_reporting_level())
      } else {
        dataset()
      }
    })
    
    # Whether the current method is SN (used for title overrides)
    is_sn <- reactive(selected_method() == "Subnational definition")

    # ─── 4) Top chart ─────────────────────────────────────────────────────────────────
    output$top_chart <- plotly::renderPlotly({
      req(selected_economy())
      
      if (selected_method() == "Subnational definition") {
        # Look up the ISO3 code for the selected economy name
        econ_code <- data_sn |>
          collapse::fsubset(country_name == selected_economy()) |>
          collapse::funique(cols = "code")
        econ_code <- econ_code$code[1]
        
        sn_pline <- if (is.null(input$sn_poverty_line)) "$2.15" else input$sn_poverty_line
        show_granular <- if (is.null(input$sn_granular)) FALSE else input$sn_granular
        
        if (show_granular) {
          plot_sn_range_bars(
            data                = data_sn,
            country_code        = econ_code,
            country_name        = selected_economy(),
            selected_poverty_line = sn_pline
          )
        } else {
          plot_sn_dumbbell(
            data                = data_sn,
            country_code        = econ_code,
            country_name        = selected_economy(),
            selected_poverty_line = sn_pline
          )
        }
      } else {
        plot_single_country(
          data           = dataset(),
          select_country = selected_economy(),
          select_method  = selected_method()
        )
      }
    })

    # ─── 5) Left-hand panel: metadata description ─────────────────────────────────────
    output$method_panel <- renderUI({
      econ <- selected_economy()
      meth <- selected_method()
      desc_text <- if (meth == "Welfare conversion") {
        dm_metadata$description
      } else if (meth == "Household allocation") {
        stb_metadata$description
      } else {
        sn_metadata$description
      }
      tagList(
        p(strong("Method:"),   meth),
        p(strong("Economy:"),  econ),
        p(desc_text)
      )
    })

    # ─── 6) Bottom section: dark-blue charts ────────────────────────────────────────
    output$bottom_section_ui <- renderUI({
      req(input$select_method)
      
      # All methods now get the standard bottom section with toggle buttons + charts
      tags$div(
          style = "
            background-color: #003f5c;
            color: #ffffff;
            padding: 20px;
            border-radius: 4px;
          ",
          
          ## Toggle buttons
          fluidRow(
            column(
              width = 4, align = "center",
              actionButton(ns("btn_rankings"), "Differences", class = "btn btn-outline-light")
            ),
            column(
              width = 4, align = "center",
              actionButton(ns("btn_changes"), "Changes", class = "btn btn-outline-light")
            ),
            column(
              width = 4, align = "center",
              actionButton(ns("btn_scatter"), "Scatterplot", class = "btn btn-outline-light")
            )
          ),
          
          br(),
          
          ## Bottom chart area
          fluidRow(
            uiOutput(ns("scatter_controls_ui")),
            uiOutput(ns("bottom_chart_column_ui"))
          )
        )
    })

    # ─── 7) Bottom-chart toggles ──────────────────────────────────────────────────────
    observeEvent(input$btn_rankings,  current_tab("rankings"))
    observeEvent(input$btn_changes,   current_tab("changes"))
    observeEvent(input$btn_scatter,   current_tab("scatter"))

    # Dynamic UI for scatter/rankings controls (left panel)
    output$scatter_controls_ui <- renderUI({
      if (current_tab() %in% c("scatter", "rankings")) {
        column(
          width = 3,
          tags$div(
            style = "background-color: rgba(255, 255, 255, 0.1); padding: 15px; border-radius: 4px;",
            h5("Plot Controls", style = "color: white; margin-bottom: 15px;"),
            
            # Reporting level filter (SN only)
            if (is_sn()) {
              selectInput(
                inputId = ns("sn_reporting_level"),
                label = "Reporting Level:",
                choices = c("National" = "national",
                           "Urban" = "urban",
                           "Rural" = "rural"),
                selected = sn_reporting_level()
              )
            },
            
            # Log scale toggle (scatter only)
            if (current_tab() == "scatter") {
              checkboxInput(
                inputId = ns("log_scale"),
                label = "Log scale (both axes)",
                value = FALSE
              )
            },
            
            # Poverty line filter (both scatter and rankings)
            selectInput(
              inputId = ns("poverty_line_filter"),
              label = "Poverty Line:",
              choices = c("All poverty lines" = "all", 
                         "$2.15 only" = "$2.15",
                         "$3.65 only" = "$3.65",
                         "$6.85 only" = "$6.85"),
              selected = "all"
            ),
            hr(style = "border-color: rgba(255, 255, 255, 0.3);"),
            h5("Statistics", style = "color: white; margin-bottom: 15px;"),
            
            # Show appropriate statistics based on current tab
            if (current_tab() == "scatter") {
              uiOutput(ns("scatter_stats"))
            } else if (current_tab() == "rankings") {
              uiOutput(ns("rankings_stats"))
            }
          )
        )
      } else if (is_sn()) {
        # Show reporting level filter for Changes tab when SN is selected
        column(
          width = 3,
          tags$div(
            style = "background-color: rgba(255, 255, 255, 0.1); padding: 15px; border-radius: 4px;",
            h5("Plot Controls", style = "color: white; margin-bottom: 15px;"),
            selectInput(
              inputId = ns("sn_reporting_level"),
              label = "Reporting Level:",
              choices = c("National" = "national",
                         "Urban" = "urban",
                         "Rural" = "rural"),
              selected = sn_reporting_level()
            )
          )
        )
      } else {
        NULL
      }
    })
    
    # Dynamic UI for bottom chart column width
    output$bottom_chart_column_ui <- renderUI({
      has_controls <- current_tab() %in% c("scatter", "rankings") ||
                      (current_tab() == "changes" && is_sn())
      chart_width <- if (has_controls) 9 else 12
      
      column(
        width = chart_width,
        if (current_tab() %in% c("scatter", "rankings")) {
          plotly::plotlyOutput(
            outputId = ns("bottom_chart_plotly"),
            height   = "500px"
          )
        } else {
          plotOutput(
            outputId = ns("bottom_chart"),
            height   = "500px",
            click    = ns("bottom_chart_click")
          )
        }
      )
    })
    
    # Update log scale reactive values
    observeEvent(input$log_scale, { log_scale(input$log_scale) })

    # Regular ggplot outputs (changes only now)
    output$bottom_chart <- renderPlot({
      if (current_tab() == "changes") {
        sn_title <- if (is_sn()) {
          glue::glue(
            "Difference between the <span style='color:#FF9800;'>**DB**</span> and <span style='color:#4EC2C0;'>**DOU**</span> estimates for the subnational definition"
          )
        } else {
          NULL
        }
        plot_changes(
          data           = cross_country_data(),
          select_country = selected_economy(),
          select_method  = selected_method(),
          title          = sn_title
        )
      }
    })

    # Plotly output for scatter and rankings
    output$bottom_chart_plotly <- plotly::renderPlotly({
      if (current_tab() == "scatter") {
        plot_scatter(
          data                = cross_country_data(),
          select_year         = NULL,
          log_x               = log_scale(),
          log_y               = log_scale(),
          poverty_line_filter = poverty_line_filter(),
          title   = if (is_sn()) "DB vs DOU Poverty Headcount Estimates" else NULL,
          x_label = if (is_sn()) "DB Method Headcount (%)" else NULL,
          y_label = if (is_sn()) "DOU Method Headcount (%)" else NULL
        )
      } else if (current_tab() == "rankings") {
        plot_rankings(
          data                = cross_country_data(),
          select_country      = selected_economy(),
          poverty_line_filter = poverty_line_filter(),
          title   = if (is_sn()) "Difference plot of DB vs DOU poverty headcount" else NULL,
          x_label = if (is_sn()) "Mean headcount across methods (%)" else NULL,
          y_label = if (is_sn()) "Difference (DB \u2212 DOU) (pp)" else NULL
        )
      }
    })
    
    # Render statistics panel
    output$scatter_stats <- renderUI({
      if (current_tab() != "scatter") return(NULL)
      
      # Calculate statistics from the data
      dt <- cross_country_data()
      if (is.null(dt)) return(NULL)
      
      # Filter by poverty line if needed
      if (poverty_line_filter() != "all") {
        dt <- dt |>
          dplyr::filter(poverty_line == poverty_line_filter())
      }
      
      # Filter to data with both values
      dt_complete <- dt |>
        dplyr::filter(!is.na(headcount_default) & !is.na(headcount_estimate))
      
      if (nrow(dt_complete) == 0) return(NULL)
      
      # Calculate statistics
      correlation <- cor(dt_complete$headcount_default, 
                        dt_complete$headcount_estimate,
                        use = "complete.obs")
      
      mean_abs_diff <- mean(abs(dt_complete$headcount_estimate - 
                                  dt_complete$headcount_default),
                           na.rm = TRUE)
      
      within_tolerance <- sum(abs(dt_complete$headcount_estimate - 
                                    dt_complete$headcount_default) <= 3,
                             na.rm = TRUE)
      pct_within <- (within_tolerance / nrow(dt_complete)) * 100
      
      rmse <- sqrt(mean((dt_complete$headcount_estimate - 
                        dt_complete$headcount_default)^2,
                       na.rm = TRUE))
      
      # Render statistics
      tagList(
        tags$div(
          style = "color: white; font-size: 13px;",
          p(
            strong("Correlation:"),
            br(),
            sprintf("%.3f", correlation)
          ),
          p(
            strong("Mean Absolute Difference:"),
            br(),
            sprintf("%.2f pp", mean_abs_diff)
          ),
          p(
            strong("Within ±3pp bands:"),
            br(),
            sprintf("%d/%d (%.1f%%)", within_tolerance, nrow(dt_complete), pct_within)
          ),
          p(
            strong("RMSE:"),
            br(),
            sprintf("%.2f pp", rmse)
          )
        )
      )
    })
    
    # Render rankings statistics panel
    output$rankings_stats <- renderUI({
      if (current_tab() != "rankings") return(NULL)
      
      # Calculate statistics from the data
      dt <- cross_country_data()
      if (is.null(dt)) return(NULL)
      
      # Filter to complete cases
      dt_complete <- dt |>
        dplyr::filter(!is.na(headcount_default) & !is.na(headcount_estimate))
      
      # Filter by poverty line if needed
      if (poverty_line_filter() != "all") {
        dt_complete <- dt_complete |>
          dplyr::filter(poverty_line == poverty_line_filter())
      }
      
      if (nrow(dt_complete) == 0) return(NULL)
      
      # Calculate Bland-Altman statistics
      # Note: diff_pp is PIP minus Alternative
      diff_pp <- dt_complete$headcount_default - dt_complete$headcount_estimate
      abs_diff_pp <- abs(diff_pp)
      
      # Count unique economies, not points
      n_economies <- length(unique(dt_complete$country_name))
      n_points <- nrow(dt_complete)
      
      bias <- mean(diff_pp, na.rm = TRUE)
      sd_diff <- sd(diff_pp, na.rm = TRUE)
      loa_lower <- bias - 1.96 * sd_diff
      loa_upper <- bias + 1.96 * sd_diff
      within_3pp <- sum(abs_diff_pp <= 3, na.rm = TRUE)
      pct_within_3pp <- (within_3pp / n_points) * 100
      
      # Render statistics
      tagList(
        tags$div(
          style = "color: white; font-size: 13px;",
          p(
            strong("Number of economies:"),
            br(),
            sprintf("%d", n_economies)
          ),
          p(
            strong("Mean difference:"),
            br(),
            sprintf("%.2f pp", bias)
          ),
          p(
            strong("Standard Deviation:"),
            br(),
            sprintf("%.2f pp", sd_diff)
          ),
          p(
            strong("Limits of Agreement*:"),
            br(),
            sprintf("[%.2f, %.2f] pp", loa_lower, loa_upper)
          ),
          p(
            strong("Within ±3pp:"),
            br(),
            sprintf("%d/%d (%.1f%%)", within_3pp, n_points, pct_within_3pp)
          ),
          p(
            style = "font-size: 11px; color: #cccccc; margin-top: 10px;",
            "*Limits of Agreement: Mean ± 1.96×SD, indicating the range within which 95% of differences are expected to fall."
          )
        )
      )
    })
    
    # ─── 7) Click handler for Changes chart ────────────────────────────────────────────────────
    observeEvent(input$bottom_chart_click, {
      # Only handle clicks when on Changes tab
      if (current_tab() != "changes") return()
      
      click <- input$bottom_chart_click
      if (is.null(click)) return()
      
      # Get the prepared data used in the plot
      plot_data <- prep_changes(cross_country_data(), selected_economy())
      
      # Now the plot has only poverty_line as column facets (no region rows)
      # Determine which poverty line based on click$panelvar1
      clicked_poverty_line <- click$panelvar1
      
      poverty_lines <- c("$2.15", "$3.65", "$6.85")
      if (is.null(clicked_poverty_line)) {
        # Fallback: use first poverty line
        clicked_poverty_line <- poverty_lines[1]
      }
      
      # Filter data for the clicked poverty line
      facet_data <- plot_data |>
        dplyr::filter(poverty_line == clicked_poverty_line)
      
      # Countries are ordered by fct_reorder(country_name, headcount_default)
      # Get the country order for this specific facet
      country_order <- facet_data |>
        dplyr::arrange(headcount_default) |>
        dplyr::pull(country_name) |>
        unique()
      
      # Round the y-coordinate to get the country index
      clicked_index <- round(click$y)
      
      if (clicked_index < 1 || clicked_index > length(country_order)) return()
      
      # Get the country name (y-axis goes bottom to top)
      clicked_country <- country_order[clicked_index]
      
      # Filter data for the clicked country
      country_data <- plot_data |>
        dplyr::filter(country_name == clicked_country)
      
      if (nrow(country_data) == 0) return()
      
      # Extract country info
      country_code <- unique(country_data$code)
      region <- unique(country_data$region_code)
      
      # Create detailed modal content
      modal_content <- tagList(
        h4(style = "color: #0071bc;", clicked_country),
        p(strong("Country Code: "), country_code),
        p(strong("Region: "), region),
        p(strong("Method: "), selected_method()),
        hr(),
        h5("Poverty Headcount Estimates by Poverty Line:"),
        br(),
        
        # Create a table for each poverty line
        lapply(unique(country_data$poverty_line), function(pline) {
          pline_data <- country_data |>
            dplyr::filter(poverty_line == pline)
          
          tagList(
            tags$div(
              style = "margin-bottom: 20px;",
              h6(strong(paste0("Poverty Line: ", pline))),
              tags$table(
                class = "table table-striped table-sm",
                style = "width: 100%; margin-top: 10px;",
                tags$thead(
                  tags$tr(
                    tags$th("Metric"),
                    tags$th("Value", style = "text-align: right;")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td("Default Methodology"),
                    tags$td(paste0(round(pline_data$headcount_default, 1), "%"),
                           style = "text-align: right;")
                  ),
                  tags$tr(
                    tags$td("Alternative Methodology"),
                    tags$td(paste0(round(pline_data$headcount_estimate, 1), "%"),
                           style = "text-align: right;")
                  ),
                  tags$tr(
                    tags$td(strong("Absolute Difference")),
                    tags$td(
                      strong(paste0(round(pline_data$diff, 1), " pp")),
                      style = "text-align: right; color: #d62728;"
                    )
                  )
                )
              )
            )
          )
        })
      )
      
      # Show modal
      showModal(
        modalDialog(
          title = "Country Details",
          modal_content,
          easyClose = TRUE,
          size = "m",
          footer = modalButton("Close")
        )
      )
    })
    
    # ─── 8) Learn-more modal ──────────────────────────────────────────────────────────
    observeEvent(input$learn_more, {
      md_file <- switch(
        input$select_method,
        "Welfare conversion"     = "dm_full_description.md",
        "Household allocation"   = "stb_full_description.md",
        "Subnational definition" = "sn_full_description.md",
        NULL
      )
      if (is.null(md_file)) {
        shiny::showNotification(
          paste("No full-description available for method:", input$select_method),
          type = "error"
        )
        return()
      }

      md_path <- file.path("data", md_file)
      if (!file.exists(md_path)) {
        shiny::showNotification(
          paste("Markdown file not found:", md_path),
          type = "error"
        )
        return()
      }

      shiny::showModal(
        shiny::modalDialog(
          title     = glue::glue("More on the {tolower(input$select_method)} method"),
          shiny::includeMarkdown(md_path),
          easyClose = TRUE,
          size      = "l"
        )
      )
    })
    
    # ─── 9) Download data handler ──────────────────────────────────────────────────────
    output$download_data <- downloadHandler(
      filename = function() {
        method_name <- tolower(gsub(" ", "_", input$select_method))
        paste0("innovation_hub_", method_name, "_data.csv")
      },
      content = function(file) {
        write.csv(dataset(), file, row.names = FALSE)
      }
    )

  })  # end moduleServer
}    # end mod_interactive_dashboard_server
