#' Reporting-level choices used in multiple SN controls
#' @noRd
SN_REPORTING_LEVELS <- c("National" = "national", "Urban" = "urban", "Rural" = "rural")

#' interactive_dashboard UI Function
#'
#' @description Full UI for the Deep Dives page. Renders:
#'   \enumerate{
#'     \item A dark-navy introductory banner with page title and brief description.
#'     \item A two-column content area: a styled controls card on the left and
#'           a chart card on the right.
#'     \item A lower \dQuote{Additional Analysis} section: distinct blue-gray
#'           background band, introductory text, enhanced tab bar, and a
#'           CSS grid panel (\code{pip-analysis-panel}) supporting 2-column
#'           (controls + chart) and 3-column (controls + chart + stats) layouts.
#'   }
#'
#' @param id Module ID.
#'
#' @noRd
mod_interactive_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # ── 1. Page banner ────────────────────────────────────────────────────
    tags$section(
      class = "pip-dd-banner",
      tags$div(
        class = "pip-dd-banner__inner",
        tags$h1(class = "pip-dd-banner__title", "Deep Dives"),
        tags$p(
          class = "pip-dd-banner__text",
          "Select a peer-reviewed method and investigate poverty and ",
          "inequality estimates."
        )
      )
    ),

    # ── 2. Two-column layout: controls left, chart right ─────────────────
    tags$div(
      class = "pip-dd-body",

      tags$div(
        class = "pip-dd-layout",

        # ── Left: controls card ──────────────────────────────────────────
        tags$div(
          class = "pip-dd-layout__controls",
          tags$div(
            class = "pip-card",

            tags$h3(class = "pip-card__heading", "Choose inputs"),

            # Method picker
            selectInput(
              inputId = ns("select_method"),
              label   = "Method",
              choices = c(
                "Welfare conversion",
                "Household allocation",
                "Subnational definition",
                "NA\u2013Survey gap adjustment"
              ),
              selected = "Welfare conversion"
            ),

            # Economy picker
            selectInput(
              inputId  = ns("select_economy"),
              label    = "Economy",
              choices  = NULL,
              selected = NULL
            ),

            # SN-specific controls (poverty line + granular toggle)
            uiOutput(ns("sn_controls")),

            # MTG-specific controls (all-years toggle)
            uiOutput(ns("mtg_controls")),

            tags$hr(class = "pip-card__divider"),

            # Method description
            uiOutput(ns("method_panel")),

            # Action buttons
            tags$div(
              class = "pip-card__actions",
              actionButton(
                ns("learn_more"),
                "Learn more",
                class = "btn btn-primary"
              ),
              downloadButton(
                ns("download_data"),
                "Download data"
              )
            )
          )
        ),

        # ── Right: chart card ────────────────────────────────────────────
        tags$div(
          class = "pip-dd-layout__chart",
          tags$div(
            class = "pip-card",
            plotly::plotlyOutput(
              outputId = ns("top_chart"),
              height   = "480px"
            )
          )
        )
      )
    ),

    # ── 3. Additional analysis section (rendered by server) ───────────────
    uiOutput(ns("bottom_section_ui"))
  )
}


#' interactive_dashboard Server Functions
#'
#' @param id    module id
#' @param data_dm   data.table for Welfare conversion (DM) method.
#' @param data_stb  data.table for Household allocation (STB) method.
#' @param data_sn   data.table for Subnational definition (SN) method.
#' @param data_sn_cross  Cross-country data for SN method.
#' @param data_yk   data.table for NA–Survey gap adjustment (MTG) method,
#'   produced by `data-raw/d_yk.R`.
#' @param dm_metadata   Metadata tibble for DM method.
#' @param stb_metadata  Metadata tibble for STB method.
#' @param sn_metadata   Metadata tibble for SN method.
#' @param yk_metadata   Metadata tibble for MTG method.
#' @param method_override  An optional reactive that, when non-NULL, overrides
#'   the selected method (used by the Home page to navigate directly to a method).
#' @noRd
# R/mod_interactive_dashboard.R

mod_interactive_dashboard_server <- function(
    id,
    data_dm,
    data_stb,
    data_sn,
    data_sn_cross,
    data_yk,
    dm_metadata,
    stb_metadata,
    sn_metadata,
    yk_metadata,
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
      } else if (input$select_method == "Subnational definition") {
        data_sn
      } else {
        # NA–Survey gap adjustment: return only is_latest rows for economy picker
        data_yk
      }
    })

    # MTG-specific reactive values (sidebar controls for Chart 1)
    mtg_na_type   <- reactiveVal("hfce")
    mtg_ppp       <- reactiveVal("2021")
    mtg_all_years <- reactiveVal(FALSE)

    # MTG-specific reactive values (Chart 2 / Gini controls — independent of sidebar)
    mtg_gini_na_type   <- reactiveVal("hfce")
    mtg_gini_latest    <- reactiveVal(FALSE)

    # ─── 2) Whenever the dataset changes (and at startup), repopulate economy picker ──
    observeEvent(dataset(), {
      if (input$select_method == "NA\u2013Survey gap adjustment") {
        # For MTG use country_name from is_latest rows (non-NA names only)
        countries <- sort(unique(
          data_yk$country_name[data_yk$is_latest & !is.na(data_yk$country_name)]
        ))
      } else {
        countries <- sort(unique(dataset()$country_name))
      }
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
    
    # Convenience flag for MTG method
    is_mtg <- reactive(selected_method() == "NA\u2013Survey gap adjustment")

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
    
    # ─── MTG-specific controls ─────────────────────────────────────────────────
    output$mtg_controls <- renderUI({
      req(input$select_method)
      if (input$select_method != "NA\u2013Survey gap adjustment") return(NULL)
      # NA type is hardcoded to HFCE; PPP vintage is hardcoded to 2021.
      # Only the all-years toggle is exposed to the user.
      tagList(
        checkboxInput(
          inputId = ns("mtg_all_years"),
          label   = "Show all survey years (not only latest)",
          value   = FALSE
        )
      )
    })

    # Keep MTG sidebar toggle reactives in sync with UI inputs.
    # mtg_na_type and mtg_ppp are hardcoded — no UI inputs to sync.
    observeEvent(input$mtg_all_years, mtg_all_years(input$mtg_all_years))

    # Keep MTG Gini chart controls in sync with UI inputs
    observeEvent(input$mtg_gini_latest,  mtg_gini_latest(input$mtg_gini_latest))

    # ─── Lorenz comparison: file lookup and reactive values ──────────────────────
    lorenz_file_lookup <- mtg_scan_cumulative_files()

    # Build country choices for Lorenz picker: iso3 → country_name from data_yk
    # Returns an empty named vector when no fst files are found, which causes
    # selectInput to render with no choices rather than crashing.
    lorenz_country_choices <- {
      if (nrow(lorenz_file_lookup) == 0L) {
        stats::setNames(character(0L), character(0L))
      } else {
        yk_names <- unique(data_yk[, c("country_code", "country_name")])
        avail    <- lorenz_file_lookup[, .(iso3, year)]
        merged   <- merge(avail, yk_names,
                          by.x = "iso3", by.y = "country_code", all.x = TRUE)
        choices  <- stats::setNames(merged$iso3, merged$country_name)
        choices[order(names(choices))]
      }
    }

    # Pre-built reverse lookup: iso3 code → display name (avoids linear scan
    # on every country-change event)
    lorenz_name_lookup <- stats::setNames(
      names(lorenz_country_choices),
      lorenz_country_choices
    )

    # Helper: load one country's fst data and update lorenz reactives
    load_lorenz_country <- function(iso3_sel) {
      file_row <- lorenz_file_lookup[iso3 == iso3_sel]
      if (nrow(file_row) == 0L) {
        lorenz_data(NULL)
        lorenz_meta(list(iso3 = iso3_sel, year = NA, country_name = iso3_sel))
        return()
      }
      dt    <- mtg_read_cumulative(iso3_sel, file_row$year[1L])
      cname <- lorenz_name_lookup[[iso3_sel]]
      if (is.null(cname) || is.na(cname)) cname <- iso3_sel
      lorenz_data(dt)
      lorenz_meta(list(iso3 = iso3_sel, year = file_row$year[1L],
                       country_name = cname))
    }

    # Reactive: currently loaded Lorenz data (one country at a time)
    lorenz_data <- reactiveVal(NULL)
    lorenz_meta <- reactiveVal(list(iso3 = NULL, year = NULL, country_name = NULL))

    # Pre-load first country so the chart is not blank when the tab opens
    if (length(lorenz_country_choices) > 0L) {
      load_lorenz_country(lorenz_country_choices[[1L]])
    }

    # Load Lorenz data when country selection changes
    observeEvent(input$lorenz_country, {
      req(input$lorenz_country)
      load_lorenz_country(input$lorenz_country)
    }, ignoreInit = TRUE)

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

      if (is_mtg()) {
        plot_mtg_gap_gdppc(
          data           = data_yk,
          select_country = selected_economy(),
          na_type        = mtg_na_type(),
          ppp_vintage    = mtg_ppp(),
          show_all_years = mtg_all_years()
        )
      } else if (selected_method() == "Subnational definition") {
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
      meth <- selected_method()
      desc_text <- if (meth == "Welfare conversion") {
        dm_metadata$description
      } else if (meth == "Household allocation") {
        stb_metadata$description
      } else if (meth == "Subnational definition") {
        sn_metadata$description
      } else {
        yk_metadata$description
      }
      p(desc_text)
    })

    # ─── 6) Bottom section: Additional Analysis ──────────────────────────────────────
    output$bottom_section_ui <- renderUI({
      req(input$select_method)

      # Determine tab button labels based on active method
      rankings_label <- "Differences"
      changes_label  <- if (input$select_method == "NA\u2013Survey gap adjustment") {
        "Lorenz comparisons"
      } else {
        "Changes"
      }
      scatter_label  <- if (input$select_method == "NA\u2013Survey gap adjustment") {
        "Gini comparison"
      } else {
        "Scatterplot"
      }

      # Active tab CSS classes
      is_rankings <- current_tab() == "rankings"
      is_changes  <- current_tab() %in% c("changes", "lorenz")
      is_scatter  <- current_tab() == "scatter"

      # Determine grid variant:
      #   lorenz  → 3-column (controls + chart + stats)
      #   changes with no SN controls → full-width (single column)
      #   everything else → default 2-column (controls + chart)
      panel_class <- if (current_tab() == "lorenz") {
        "pip-analysis-panel pip-analysis-panel--triple"
      } else if (current_tab() == "changes" && !is_sn()) {
        "pip-analysis-panel pip-analysis-panel--full"
      } else {
        "pip-analysis-panel"
      }

      tags$section(
        class = "pip-analysis-section",
        tags$div(
          class = "pip-analysis-section__inner",

          # Section heading
          tags$h2(
            class = "pip-analysis-section__heading",
            "Additional Analysis"
          ),

          # Introductory sentence — gives the section editorial context
          tags$p(
            class = "pip-analysis-section__intro",
            "Use these views to compare patterns across economies and ",
            "understand how alternative estimates differ from standard PIP results."
          ),

          # ── Tab bar ───────────────────────────────────────────────────
          tags$div(
            class = "pip-tabs",

            # Differences tab (disabled for MTG)
            if (input$select_method == "NA\u2013Survey gap adjustment") {
              actionButton(
                ns("btn_rankings"),
                rankings_label,
                class    = "pip-tab",
                disabled = TRUE
              )
            } else {
              actionButton(
                ns("btn_rankings"),
                rankings_label,
                class = if (is_rankings) "pip-tab pip-tab--active" else "pip-tab"
              )
            },

            # Changes / Lorenz tab
            actionButton(
              ns("btn_changes"),
              changes_label,
              class = if (is_changes) "pip-tab pip-tab--active" else "pip-tab"
            ),

            # Scatterplot / Gini tab
            actionButton(
              ns("btn_scatter"),
              scatter_label,
              class = if (is_scatter) "pip-tab pip-tab--active" else "pip-tab"
            )
          ),

          # ── Chart + controls area — CSS grid (see pip-analysis-panel) ──
          tags$div(
            class = panel_class,
            uiOutput(ns("scatter_controls_ui"),
                     class = "pip-analysis-panel__col-wrapper"),
            uiOutput(ns("bottom_chart_column_ui"),
                     class = "pip-analysis-panel__col-wrapper")
          )
        )
      )
    })

    # ─── 7) Bottom-chart toggles ──────────────────────────────────────────────────────
    observeEvent(input$btn_rankings,  { if (!is_mtg()) current_tab("rankings") })
    observeEvent(input$btn_changes, {
      if (is_mtg()) {
        current_tab("lorenz")
      } else {
        current_tab("changes")
      }
    })
    observeEvent(input$btn_scatter,   current_tab("scatter"))

    # When switching to MTG, force 'scatter' tab (Gini comparison)
    observeEvent(input$select_method, {
      if (input$select_method == "NA\u2013Survey gap adjustment") {
        current_tab("scatter")
      }
    })

    # Dynamic UI for scatter/rankings controls (left panel)
    output$scatter_controls_ui <- renderUI({
      # Lorenz tab: controls on the LEFT (country + gap share slider)
      if (current_tab() == "lorenz" && is_mtg()) {
        return(tags$div(
          class = "pip-analysis-panel__controls",
          tags$div(
            class = "pip-card pip-card--elevated",
            tags$h5(class = "pip-card__subheading", "Lorenz Curve Controls"),
            selectInput(
              inputId  = ns("lorenz_country"),
              label    = "Economy",
              choices  = lorenz_country_choices,
              selected = lorenz_country_choices[1]
            ),
            sliderInput(
              inputId = ns("lorenz_gap_share"),
              label   = "HFCE gap share (%):",
              min     = 5L,
              max     = 100L,
              value   = 50L,
              step    = 5L,
              post    = "%"
            )
          )
        ))
      }

      if (current_tab() %in% c("scatter", "rankings")) {

        # MTG method: Gini-specific controls only
        if (is_mtg()) {
          return(tags$div(
            class = "pip-analysis-panel__controls",
            tags$div(
              class = "pip-card pip-card--elevated",
              tags$h5(class = "pip-card__subheading", "Gini Chart Controls"),
              checkboxInput(
                inputId = ns("mtg_gini_latest"),
                label   = "Show latest year only",
                value   = FALSE
              )
            )
          ))
        }

        # Standard controls for all other methods
        tags$div(
          class = "pip-analysis-panel__controls",
          tags$div(
            class = "pip-card pip-card--elevated",
            tags$h5(class = "pip-card__subheading", "Plot Controls"),

            # Log scale toggle (scatter only)
            if (current_tab() == "scatter") {
              checkboxInput(
                inputId = ns("log_scale"),
                label   = "Log scale (both axes)",
                value   = FALSE
              )
            },

            # Poverty line filter
            selectInput(
              inputId  = ns("poverty_line_filter"),
              label    = "Poverty Line",
              choices  = c(
                "All poverty lines" = "all",
                "$2.15 only"        = "$2.15",
                "$3.65 only"        = "$3.65",
                "$6.85 only"        = "$6.85"
              ),
              selected = "all"
            ),
            tags$hr(class = "pip-card__divider"),
            tags$h5(class = "pip-card__subheading", "Statistics"),

            if (current_tab() == "scatter") {
              uiOutput(ns("scatter_stats"))
            } else if (current_tab() == "rankings") {
              uiOutput(ns("rankings_stats"))
            }
          )
        )

      } else if (is_sn()) {
        # SN Changes tab: reporting level filter
        tags$div(
          class = "pip-analysis-panel__controls",
          tags$div(
            class = "pip-card pip-card--elevated",
            tags$h5(class = "pip-card__subheading", "Plot Controls"),
            selectInput(
              inputId  = ns("sn_reporting_level"),
              label    = "Reporting Level",
              choices  = SN_REPORTING_LEVELS,
              selected = sn_reporting_level()
            )
          )
        )
      } else {
        NULL
      }
    })
    
    # Dynamic UI for bottom chart column
    output$bottom_chart_column_ui <- renderUI({
      # Lorenz tab (3-col): chart in __chart, stats in __stats
      if (current_tab() == "lorenz" && is_mtg()) {
        return(tagList(
          tags$div(
            class = "pip-analysis-panel__chart",
            tags$div(
              class = "pip-card pip-card--elevated",
              plotly::plotlyOutput(
                outputId = ns("bottom_chart_plotly"),
                height   = "500px"
              )
            )
          ),
          tags$div(
            class = "pip-analysis-panel__stats",
            uiOutput(ns("lorenz_stats_ui"))
          )
        ))
      }

      # All other tabs: single __chart column wrapping an elevated card
      tags$div(
        class = "pip-analysis-panel__chart",
        tags$div(
          class = "pip-card pip-card--elevated",
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
      )
    })
    
    # Update log scale reactive values
    observeEvent(input$log_scale, { log_scale(input$log_scale) })

    # Regular ggplot outputs (changes only now)
    output$bottom_chart <- renderPlot({
      if (current_tab() == "changes") {
        sn_title <- if (is_sn()) {
          glue::glue(
            "Difference between the <span class='pip-stat--db'>**DB**</span> and <span class='pip-stat--dou'>**DOU**</span> estimates for the subnational definition"
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

    # Plotly output for scatter, rankings, and lorenz
    output$bottom_chart_plotly <- plotly::renderPlotly({
      if (current_tab() == "lorenz" && is_mtg()) {
        req(lorenz_data())
        meta <- lorenz_meta()
        gap  <- if (is.null(input$lorenz_gap_share)) 50L else input$lorenz_gap_share
        plot_mtg_lorenz_comparison(
          data         = lorenz_data(),
          country_name = meta$country_name,
          country_code = meta$iso3,
          survey_year  = meta$year,
          gap_share    = gap
        )
      } else if (current_tab() == "scatter" && is_mtg()) {
        plot_mtg_gini_sensitivity(
          data             = data_yk,
          select_country   = selected_economy(),
          na_type          = mtg_gini_na_type(),
          ppp_vintage      = mtg_ppp(),
          show_latest_only = mtg_gini_latest()
        )
      } else if (current_tab() == "scatter") {
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
      } else if (current_tab() == "rankings" && !is_mtg()) {
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
    
    # Render Lorenz statistics panel (right side of chart)
    output$lorenz_stats_ui <- renderUI({
      if (current_tab() != "lorenz" || !is_mtg()) return(NULL)
      req(lorenz_data())

      gap   <- if (is.null(input$lorenz_gap_share)) 50L else input$lorenz_gap_share
      stats <- compute_lorenz_stats(lorenz_data(), gap)
      meta  <- lorenz_meta()

      tags$div(
        class = "pip-card pip-card--elevated",
        tags$h5(class = "pip-card__subheading", "Distribution Statistics"),
        tags$div(
          class = "pip-lorenz-stats",
          tags$p(tags$strong("Country:"), tags$br(),
                 paste0(meta$country_name, " (", meta$iso3, ")")),
          tags$p(tags$strong("Survey year:"), tags$br(), meta$year),
          tags$p(tags$strong("Gap share:"), tags$br(), paste0(gap, "%")),
          tags$hr(),
          tags$p(tags$strong("Gini (survey):"), tags$br(),
                 if (!is.na(stats$gini_std)) sprintf("%.4f", stats$gini_std) else "N/A"),
          tags$p(tags$strong("Gini (HFCE-adjusted):"), tags$br(),
                 if (!is.na(stats$gini_adj)) sprintf("%.4f", stats$gini_adj) else "N/A"),
          tags$p(
            tags$strong("Gini change:"), tags$br(),
            if (!is.na(stats$gini_change)) {
              tags$span(
                class = if (stats$gini_change > 0) "pip-stat--positive" else "pip-stat--negative",
                sprintf("%+.4f", stats$gini_change)
              )
            } else {
              "N/A"
            }
          )
        )
      )
    })

    # Render statistics panel
    output$scatter_stats <- renderUI({
      if (current_tab() != "scatter") return(NULL)

      dt <- cross_country_data()
      if (is.null(dt)) return(NULL)

      if (poverty_line_filter() != "all") {
        dt <- dt |> dplyr::filter(poverty_line == poverty_line_filter())
      }

      dt_complete <- dt |>
        dplyr::filter(!is.na(headcount_default) & !is.na(headcount_estimate))

      if (nrow(dt_complete) == 0) return(NULL)

      correlation   <- cor(dt_complete$headcount_default,
                           dt_complete$headcount_estimate,
                           use = "complete.obs")
      mean_abs_diff <- mean(abs(dt_complete$headcount_estimate -
                                  dt_complete$headcount_default),
                            na.rm = TRUE)
      within_tolerance <- sum(
        abs(dt_complete$headcount_estimate - dt_complete$headcount_default) <= 3,
        na.rm = TRUE
      )
      pct_within <- (within_tolerance / nrow(dt_complete)) * 100
      rmse       <- sqrt(mean(
        (dt_complete$headcount_estimate - dt_complete$headcount_default)^2,
        na.rm = TRUE
      ))

      tags$div(
        class = "pip-analysis-stats",
        tags$p(tags$strong("Correlation:"), tags$br(),
               sprintf("%.3f", correlation)),
        tags$p(tags$strong("Mean Absolute Difference:"), tags$br(),
               sprintf("%.2f pp", mean_abs_diff)),
        tags$p(tags$strong("Within \u00b13pp bands:"), tags$br(),
               sprintf("%d/%d (%.1f%%)", within_tolerance, nrow(dt_complete), pct_within)),
        tags$p(tags$strong("RMSE:"), tags$br(),
               sprintf("%.2f pp", rmse))
      )
    })
    
    # Render rankings statistics panel
    output$rankings_stats <- renderUI({
      if (current_tab() != "rankings") return(NULL)

      dt <- cross_country_data()
      if (is.null(dt)) return(NULL)

      dt_complete <- dt |>
        dplyr::filter(!is.na(headcount_default) & !is.na(headcount_estimate))

      if (poverty_line_filter() != "all") {
        dt_complete <- dt_complete |>
          dplyr::filter(poverty_line == poverty_line_filter())
      }

      if (nrow(dt_complete) == 0) return(NULL)

      diff_pp     <- dt_complete$headcount_default - dt_complete$headcount_estimate
      abs_diff_pp <- abs(diff_pp)
      n_economies <- length(unique(dt_complete$country_name))
      n_points    <- nrow(dt_complete)
      bias        <- mean(diff_pp, na.rm = TRUE)
      sd_diff     <- sd(diff_pp, na.rm = TRUE)
      loa_lower   <- bias - 1.96 * sd_diff
      loa_upper   <- bias + 1.96 * sd_diff
      within_3pp  <- sum(abs_diff_pp <= 3, na.rm = TRUE)
      pct_within  <- (within_3pp / n_points) * 100

      tags$div(
        class = "pip-analysis-stats",
        tags$p(tags$strong("Number of economies:"), tags$br(),
               sprintf("%d", n_economies)),
        tags$p(tags$strong("Mean difference:"), tags$br(),
               sprintf("%.2f pp", bias)),
        tags$p(tags$strong("Standard Deviation:"), tags$br(),
               sprintf("%.2f pp", sd_diff)),
        tags$p(tags$strong("Limits of Agreement*:"), tags$br(),
               sprintf("[%.2f, %.2f] pp", loa_lower, loa_upper)),
        tags$p(tags$strong("Within \u00b13pp:"), tags$br(),
               sprintf("%d/%d (%.1f%%)", within_3pp, n_points, pct_within)),
        tags$p(
          class = "pip-analysis-stats__note",
          "*Limits of Agreement: Mean \u00b1 1.96\u00d7SD, indicating the range within which 95% of differences are expected to fall."
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
        h4(class = "pip-modal__country-title", clicked_country),
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
                    class = "pip-modal__diff-row",
                    tags$td(strong("Absolute Difference")),
                    tags$td(
                      class = "pip-modal__diff-value",
                      strong(paste0(round(pline_data$diff, 1), " pp"))
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
        "Welfare conversion"          = "dm_full_description.md",
        "Household allocation"        = "stb_full_description.md",
        "Subnational definition"      = "sn_full_description.md",
        "NA\u2013Survey gap adjustment" = "yk_full_description.md",
        NULL
      )
      if (is.null(md_file)) {
        shiny::showNotification(
          paste("No full-description available for method:", input$select_method),
          type = "error"
        )
        return()
      }

      md_path <- app_sys("app/data", md_file)
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
        method_name <- gsub("[^a-z0-9]+", "_", tolower(input$select_method))
        paste0("innovation_hub_", method_name, "_data.csv")
      },
      content = function(file) {
        if (is_mtg()) {
          write.csv(data_yk, file, row.names = FALSE)
        } else {
          write.csv(dataset(), file, row.names = FALSE)
        }
      }
    )

  })  # end moduleServer
}    # end mod_interactive_dashboard_server
