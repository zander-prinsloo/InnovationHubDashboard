#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),

    # ---- PIP Header ----
    tags$header(
      class = "pip-header",

      # Brand group: logo + divider + identity text
      tags$div(
        class = "pip-header__brand",
        tags$div(
          class = "pip-header-logo",
          tags$img(
            src = "www/pip-logo.png",
            alt = "World Bank Poverty and Inequality Platform Logo"
          )
        ),
        tags$span(class = "pip-header__brand-divider"),
        tags$span(
          class = "pip-header-identity",
          "Innovation Hub"
        )
      ),

      # Navigation menu on the right
      tags$nav(
        tags$ul(
          class = "pip-header-nav",

          tags$li(
            class = "pip-header-nav-item",
            actionLink(
              inputId = "nav_home",
              label   = "Home",
              class   = "pip-header-nav-link pip-header-nav-link--active"
            )
          ),

          tags$li(
            class = "pip-header-nav-item",
            actionLink(
              inputId = "nav_deep_dives",
              label   = "Deep Dives",
              class   = "pip-header-nav-link"
            )
          ),

          tags$li(
            class = "pip-header-nav-item",
            actionLink(
              inputId = "nav_research_repo",
              label   = "Research Repository",
              class   = "pip-header-nav-link"
            )
          ),

          tags$li(
            class = "pip-header-nav-item",
            tags$a(
              href   = "https://pip.worldbank.org/home",
              class  = "pip-header-nav-link",
              target = "_blank",
              "Return to PIP"
            )
          )
        )
      )
    ),

    # ---- Main content: hidden tab panels ----
    tabsetPanel(
      id   = "main_tabs",
      type = "hidden",

      tabPanelBody(
        value = "home",
        mod_home_ui("home_1")
      ),

      tabPanelBody(
        value = "deep_dives",
        fluidPage(
          mod_interactive_dashboard_ui("interactive_dashboard_1")
        )
      ),

      # ---- Research Repository: Vue/Vuetify semantic search app in iframe ----
      tabPanelBody(
        value = "research_repo",
        tags$div(
          class = "research-repo-container",
          # Placeholder div — add banner content here in a future iteration
          tags$div(
            class = "pip-dd-banner",
            tags$div(
              class = "pip-dd-banner__inner",
              tags$h1(class = "pip-dd-banner__title", "Research Repository"),
              tags$p(
                class = "pip-dd-banner__text",
                "Convenient search through all World Bank Policy Research Working Papers.", 
                "Updated as of 20 March 2026."
              )
            )
          ),
          tags$iframe(
            src   = "research_repo/ids-doc.html",
            width = "100%",
            height = "100%",
            allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope"
          )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' Note: `research_repo` is registered as a separate resource path (not under
#' `www/`) so that `bundle_resources()` does not inject the Vuetify CSS files
#' from the Research Repository into the parent Shiny app, which would cause
#' style bleed into other tabs (e.g. dark overlays in Deep Dives).
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

  # research_repo is intentionally kept separate from www/ so that
  # bundle_resources() does not bundle the Vuetify CSS into the parent app,
  # which would bleed into and distort other tabs (Deep Dives etc.).
  add_resource_path(
    "research_repo",
    app_sys("app/research_repo")
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
    # Load PIP redesign CSS (replaces header.css)
    tags$link(
      rel  = "stylesheet",
      type = "text/css",
      href = "www/pip-redesign.css"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
