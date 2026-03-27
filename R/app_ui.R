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

    # ---- Hugging Face proxy URL ----
    tags$script(HTML("
      let HF_PROXY_BASE = null;

      Shiny.addCustomMessageHandler('hf-proxy-url', (msg) => {
        HF_PROXY_BASE = msg.url;
        window.HF_PROXY_BASE = msg.url;
        console.log('Proxy base:', HF_PROXY_BASE);
      });
    ")),

    # ---- End Hugging Face proxy URL ----

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
            actionLink(
              inputId = "nav_home",
              label   = "Home",
              class   = "pip-header-nav-link active"
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
              href = "https://pip.worldbank.org/home",
              class = "pip-header-nav-link",
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
          tags$div(class = "research-repo-banner"),
          tags$iframe(
            src    = "research_repo/docs-demo.html",
            width  = "100%",
            height = "100%",
            style  = "border: none; display: block;",
            allow  = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope"
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
