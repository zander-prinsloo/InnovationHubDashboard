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
          # you can hard‐code or override in the server with updateSelectInput()
          choices = c("Welfare conversion", "Household allocation"),
          selected = "Welfare conversion"
        ),

        # 2 Economy picker
        selectInput(
          inputId = ns("select_economy"),
          label   = "Select Economy:",
          choices = NULL,           # we’ll populate this in server
          selected = NULL
        ),

        # 3 dynamic text panel
        uiOutput(ns("method_panel")),
        br(),

        # 4 learn‐more button
        actionButton(ns("learn_more"), "Learn more", class = "btn btn-primary")
      ),
      column(
        width = 6,
        plotOutput(
          outputId = ns("top_chart"),
          height   = "300px",
          click    = ns("top_click")
        )
      )
    ),

    br(),

    ## 2nd row: dark‐blue infoblock ----
    fluidRow(
      column(
        12,
        tags$div(
          style = "
            background-color: #003f5c;
            color: #ffffff;
            padding: 20px;
            border-radius: 4px;
          ",
          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer volutpat justo elit, vel placerat lectus tempus in..."),
          p("Donec rhoncus eget metus maximus vehicula. Aenean id nulla porttitor, rhoncus nisi at, condimentum odio...")
        )
      )
    ),

    br(),

    ## 3rd row: toggle buttons ----
    fluidRow(
      column(
        width = 4, align = "center",
        actionButton(ns("btn_rankings"),  "Rankings",   class = "btn btn-outline-primary")
      ),
      column(
        width = 4, align = "center",
        actionButton(ns("btn_changes"),   "Changes",    class = "btn btn-outline-primary")
      ),
      column(
        width = 4, align = "center",
        actionButton(ns("btn_scatter"),   "Scatterplot",class = "btn btn-outline-primary")
      )
    ),

    br(),

    ## 4th row: bottom chart + side note ----
    fluidRow(
      column(
        width = 10,
        plotOutput(
          outputId = ns("bottom_chart"),
          height   = "350px"
        )
      ),
      column(
        width = 2,
        tags$p(
          "Click on any economy to deep dive above.",
          style = "color: #ff7f0e; font-weight: bold; padding-top: 80px;"
        )
      )
    )

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
    dm_metadata,
    stb_metadata
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ─── 1) Reactive dataset based on method ─────────────────────────────────────────
    dataset <- reactive({
      if (input$select_method == "Welfare conversion") {
        data_dm
      } else {
        data_stb
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

    # ─── 3) Capture the current selections ─────────────────────────────────────────────
    selected_method  <- reactive(input$select_method)
    selected_economy <- reactive(input$select_economy)
    current_tab      <- reactiveVal("rankings")

    # ─── 4) Top chart ─────────────────────────────────────────────────────────────────
    output$top_chart <- renderPlot({
      plot_single_country(
        data           = dataset(),
        select_country = selected_economy(),
        select_method  = selected_method()
      )
    })

    # ─── 5) Left-hand panel: metadata description ─────────────────────────────────────
    output$method_panel <- renderUI({
      econ <- selected_economy()
      meth <- selected_method()
      desc_text <- if (meth == "Welfare conversion") {
        dm_metadata$description
      } else {
        stb_metadata$description
      }
      tagList(
        p(strong("Method:"),   meth),
        p(strong("Economy:"),  econ),
        p(desc_text)
      )
    })

    # ─── 6) Bottom-chart toggles ──────────────────────────────────────────────────────
    observeEvent(input$btn_rankings,  current_tab("rankings"))
    observeEvent(input$btn_changes,   current_tab("changes"))
    observeEvent(input$btn_scatter,   current_tab("scatter"))

    output$bottom_chart <- renderPlot({
      switch(
        current_tab(),
        rankings = plot_rankings(dataset()),
        changes  = plot_changes(dataset()),
        scatter  = plot_scatter(dataset())
      )
    })

    # ─── 7) Learn-more modal ──────────────────────────────────────────────────────────
    observeEvent(input$learn_more, {
      md_file <- switch(
        input$select_method,
        "Welfare conversion"   = "dm_full_description.md",
        "Household allocation" = "stb_full_description.md",
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

  })  # end moduleServer
}    # end mod_interactive_dashboard_server
