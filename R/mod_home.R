#' Home Page Module
#'
#' @description Landing page for the PIP Innovation Hub with banner
#'   and 2x2 grid of method preview cards.
#'
#' @param id Module ID
#' @param dm_metadata,stb_metadata,sn_metadata,yk_metadata One-row data frames
#'   with columns \code{title}, \code{citation}, and \code{paper_url}.
#'
#' @noRd
mod_home_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # ── Banner ──────────────────────────────────────────────────────────────
    tags$div(
      class = "home-banner",
      tags$div(
        class = "home-banner-title",
        "PIP", tags$br(), "Innovation", tags$br(), "Hub"
      ),
      tags$div(
        class = "home-banner-text",
        "This site showcases experimental poverty estimates following ",
        "alternative methodologies to PIP\u2019s. Explore these estimates in the ",
        actionLink(ns("banner_deep_dives"), "Deep Dives", class = ""),
        " page or find the latest research in the ",
        actionLink(ns("banner_research"), "Research Repository", class = ""),
        "!"
      )
    ),

    # ── Method grid (2 x 2) ────────────────────────────────────────────────
    tags$div(
      class = "method-grid",

      tags$h3(class = "method-grid-title", "Methods featured in the deep dives:"),

      # Top row: YK + DM
      fluidRow(
        column(
          width = 6,
          uiOutput(ns("card_yk"))
        ),
        column(
          width = 6,
          uiOutput(ns("card_dm"))
        )
      ),

      # Bottom row: STB + SN
      fluidRow(
        column(
          width = 6,
          uiOutput(ns("card_stb"))
        ),
        column(
          width = 6,
          uiOutput(ns("card_sn"))
        )
      )
    )
  )
}


#' @noRd
mod_home_server <- function(id,
                            dm_metadata,
                            stb_metadata,
                            sn_metadata,
                            yk_metadata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: build a method card
    build_card <- function(image_src, title, citation,
                           paper_url, click_id) {
      tags$div(
        class = "method-card",

        # Clickable image → navigates to Deep Dives
        tags$img(
          src   = image_src,
          alt   = title,
          class = "method-card-image",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random())",
            click_id
          )
        ),

        tags$div(
          class = "method-card-body",

          # Title → paper URL
          tags$div(
            class = "method-card-title",
            tags$a(href = paper_url, target = "_blank", title)
          ),

          # Citation → paper URL
          tags$div(
            class = "method-card-citation",
            tags$a(href = paper_url, target = "_blank", citation)
          )
        )
      )
    }

    # ── Render cards ────────────────────────────────────────────────────────
    output$card_yk <- renderUI({
      build_card(
        image_src = "www/landing_yk.png",
        title     = yk_metadata$title,
        citation  = yk_metadata$citation,
        paper_url = yk_metadata$paper_url,
        click_id  = ns("click_yk")
      )
    })

    output$card_dm <- renderUI({
      build_card(
        image_src = "www/landing_dm.png",
        title     = dm_metadata$title,
        citation  = dm_metadata$citation,
        paper_url = dm_metadata$paper_url,
        click_id  = ns("click_dm")
      )
    })

    output$card_stb <- renderUI({
      build_card(
        image_src = "www/landing_stb.png",
        title     = stb_metadata$title,
        citation  = stb_metadata$citation,
        paper_url = stb_metadata$paper_url,
        click_id  = ns("click_stb")
      )
    })

    output$card_sn <- renderUI({
      build_card(
        image_src = "www/landing_sn.png",
        title     = sn_metadata$title,
        citation  = sn_metadata$citation,
        paper_url = sn_metadata$paper_url,
        click_id  = ns("click_sn")
      )
    })

    # ── Banner links ────────────────────────────────────────────────────────
    # "Research Repository" link in banner opens external URL
    observeEvent(input$banner_research, {
      shinyjs::runjs(
        "window.open('https://avsolatorio.github.io/ai-for-data-blog/semantic-search/ids-doc.html', '_blank')"
      )
    })

    # Return a reactive that fires with the method name when a card image
    # is clicked, or "deep_dives" when the banner Deep Dives link is clicked
    nav_event <- reactiveValues(method = NULL, counter = 0)

    observeEvent(input$banner_deep_dives, {
      nav_event$method  <- NULL
      nav_event$counter <- nav_event$counter + 1
    })

    observeEvent(input$click_dm, {
      nav_event$method  <- "Welfare conversion"
      nav_event$counter <- nav_event$counter + 1
    })

    observeEvent(input$click_yk, {
      nav_event$method  <- "NA\u2013Survey gap adjustment"
      nav_event$counter <- nav_event$counter + 1
    })

    observeEvent(input$click_stb, {
      nav_event$method  <- "Household allocation"
      nav_event$counter <- nav_event$counter + 1
    })

    observeEvent(input$click_sn, {
      nav_event$method  <- "Subnational definition"
      nav_event$counter <- nav_event$counter + 1
    })

    # Return reactive list for parent server to observe
    return(
      list(
        method  = reactive(nav_event$method),
        counter = reactive(nav_event$counter)
      )
    )
  })
}
