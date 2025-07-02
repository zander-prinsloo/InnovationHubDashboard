#' interactive_dashboard UI Function
#'
#' @noRd
mod_interactive_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(

    ## 1st row: method text on left, big chart placeholder on right ----
    fluidRow(
      column(
        6,
        tags$p(strong("Method:"), "xxxx"),
        tags$p(strong("Economy:"), "xxxx"),
        tags$p("Short, two sentence description of the method to be placed here. This text will come down onto the next row."),
        tags$p("For xxxx, the difference in the estimated poverty headcount is x."),
        tags$p("Want to learn more about the method used by x?"),
        actionButton(ns("learn_more"), "Learn more", class = "btn btn-primary")
      ),
      column(
        6,
        tags$div(
          style = "background-color: #e0e0e0; height: 300px; border-radius: 4px;",
          # replace with plotOutput(ns("top_chart")) later
          ""
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
        4, align = "center",
        actionButton(ns("btn_rankings"),  "Rankings",  class = "btn btn-outline-primary")
      ),
      column(
        4, align = "center",
        actionButton(ns("btn_changes"),   "Changes",   class = "btn btn-outline-primary")
      ),
      column(
        4, align = "center",
        actionButton(ns("btn_scatter"),   "Scatterplot", class = "btn btn-outline-primary")
      )
    ),

    br(),

    ## 4th row: bottom chart + side note ----
    fluidRow(
      column(
        10,
        tags$div(
          style = "background-color: #e0e0e0; height: 350px; border-radius: 4px;",
          # replace with plotOutput(ns("bottom_chart")) later
          ""
        )
      ),
      column(
        2,
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
#' @noRd
mod_interactive_dashboard_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## (a) learn_more could pop up a modal, e.g.:
    observeEvent(input$learn_more, {
      showModal(modalDialog(
        title = "More about this method",
        "Here you could include a longer markdown description or link to your docs.",
        easyClose = TRUE
      ))
    })

    ## (b) button logic for bottom chart (later)
    ## you might do something like:
    ## current_tab <- reactiveVal("rankings")
    ## observeEvent(input$btn_rankings,  current_tab("rankings"))
    ## observeEvent(input$btn_changes,   current_tab("changes"))
    ## observeEvent(input$btn_scatter,   current_tab("scatter"))
    ##
    ## output$bottom_chart <- renderPlot({
    ##   switch(current_tab(),
    ##     rankings = plot_rankings(...),
    ##     changes  = plot_changes(...),
    ##     scatter  = plot_scatter(...)
    ##   )
    ## })

  })
}
