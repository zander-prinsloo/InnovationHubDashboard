#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    
    # ---- PIP Header ----
    tags$header(
      class = "pip-header",
      
      # Logo on the left
      tags$div(
        class = "pip-header-logo",
        tags$img(
          src = "www/pip-logo.png",
          alt = "World Bank Poverty and Inequality Platform Logo"
        )
      ),
      
      # Title in the middle
      tags$div(
        class = "pip-header-title",
        "Innovation Hub"
      ),
      
      # Navigation menu on the right
      tags$nav(
        tags$ul(
          class = "pip-header-nav",
          
          tags$li(
            class = "pip-header-nav-item",
            tags$a(
              href = "https://pip.worldbank.org/home",
              class = "pip-header-nav-link",
              target = "_blank",
              "PIP Home"
            )
          ),
          
          tags$li(
            class = "pip-header-nav-item",
            tags$span(
              class = "pip-header-nav-link active",
              "Deep Dives"
            )
          ),
          
          tags$li(
            class = "pip-header-nav-item",
            tags$a(
              href = "https://avsolatorio.github.io/ai-for-data-blog/semantic-search/ids-doc.html",
              class = "pip-header-nav-link",
              target = "_blank",
              "Research Repository"
            )
          )
        )
      )
    ),
    
    # ---- Main dashboard content ----
    fluidPage(
      mod_interactive_dashboard_ui("interactive_dashboard_1")
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  add_resource_path(
    "designs",
    app_sys("app/designs")
  )

  tags$head(
    tags$link(
      rel = "icon",
      type = "image/png",
      href = "designs/pip-logos/favicon-PIP.png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "InnovationHubDashboard"
    ),
    # Load header CSS
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "www/header.css"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
