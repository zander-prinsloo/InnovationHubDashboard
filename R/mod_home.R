#' Home Page Module
#'
#' @description Landing page for the PIP Innovation Hub. Renders:
#'   \enumerate{
#'     \item A full-width dark-navy hero section with title, description, and
#'           a CSS-only abstract data visualisation motif. Hero content is
#'           constrained to \code{max-width: 1200px} to align with the tiles below.
#'     \item A white content section with a heading, intro line, and two large
#'           PIP-style feature tiles (Deep Dives and Research Repository).
#'     \item A full-bleed dark-navy \dQuote{Featured Methods} banner containing
#'           four smaller method tiles (background \code{#243c56}).
#'   }
#'
#' @param id Module ID.
#'
#' @noRd
mod_home_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # ── 1. Hero / banner ──────────────────────────────────────────────────
    tags$section(
      class = "pip-hero",

      # Centred inner wrapper — constrains content to 1200px, aligning
      # hero columns with the large feature tiles in the section below.
      tags$div(
        class = "pip-hero__inner",

        # Left: title + supporting text
        tags$div(
          class = "pip-hero__content",
          tags$h1(
            class = "pip-hero__title",
            "PIP Innovation Hub"
          ),
          tags$p(
            class = "pip-hero__subtitle",
            "The home for novel work on poverty and inequality measurement"
          ),
          tags$p(
            class = "pip-hero__text",
            "Compare the World Bank\u2019s official poverty estimates to those ",
            "produced using alternative methodologies from peer-reviewed ",
            "papers, or access hundreds of World Bank Policy Research ",
            "Working Papers in the research repository."
          ),
          tags$div(
            class = "pip-hero__actions",
            actionLink(
              inputId = ns("banner_deep_dives"),
              label   = "Explore Deep Dives",
              class   = "pip-btn pip-btn--primary"
            ),
            actionLink(
              inputId = ns("banner_research"),
              label   = "Research Repository",
              class   = "pip-btn pip-btn--secondary"
            )
          )
        ),

      # Right: CSS-only abstract data visualisation motif
      tags$div(
        class = "pip-hero__visual",
        # Horizontal gridlines
        tags$div(
          class = "pip-chart-lines",
          tags$span(), tags$span(), tags$span(), tags$span(), tags$span()
        ),
        # Trend line overlay
        tags$div(class = "pip-chart-trend-line"),
        # Scatter dots (12 points simulating an upward trend)
        do.call(tags$div,
          c(list(class = "pip-chart-dots"),
            lapply(seq_len(12), \(.) tags$span(class = "pip-chart-dot"))
          )
        ),
        # Axis tick marks
        tags$div(
          class = "pip-chart-ticks",
          tags$span(class = "pip-chart-tick"),
          tags$span(class = "pip-chart-tick"),
          tags$span(class = "pip-chart-tick"),
          tags$span(class = "pip-chart-tick"),
          tags$span(class = "pip-chart-tick"),
          tags$span(class = "pip-chart-tick")
        )
      )    # /pip-hero__visual
    )      # /pip-hero__inner
    ),     # /pip-hero

    # ── 2. Main content section ────────────────────────────────────────────
    tags$section(
      class = "pip-section",

      tags$div(
        class = "pip-section__inner",

        tags$h2(
          class = "pip-section__heading",
          "Explore the PIP Innovation Hub"
        ),
        tags$p(
          class = "pip-section__intro",
          "Explore experimental poverty estimation tools, methodological ",
          "deep dives, and supporting research."
        ),

        # ── Two large feature tiles ──────────────────────────────────────
        tags$div(
          class = "pip-grid-2",

          # Tile 1: Deep Dives
          tags$div(
            class = "pip-tile-large",
            # Image area
            tags$div(
              class = "pip-tile-large__image",
              tags$img(
                src = "designs/hub-designs/example-Innovation-Hub-Landing-Page.png",
                alt = "Deep Dives preview"
              )
            ),
            # Content area
            tags$div(
              class = "pip-tile-large__content",
              tags$h3(class = "pip-tile-large__title", "Deep Dives"),
              tags$p(
                class = "pip-tile-large__desc",
                "Compare standard PIP estimates with estimates following alternative ",
                "methodologies across economies and poverty lines. Select a method, ",
                "choose an economy, and explore the results interactively."
              ),
              tags$div(
                class = "pip-tile-large__btn-wrap",
                actionLink(
                  inputId = ns("feature_deep_dives"),
                  label   = "Explore more",
                  class   = "pip-tile-large__btn"
                )
              )
            )
          ),

          # Tile 2: Research Repository
          tags$div(
            class = "pip-tile-large",
            # Image area
            tags$div(
              class = "pip-tile-large__image pip-tile-large__image--contain",
              tags$img(
                src = "designs/pip-logos/WB-PIP-horizontal/color/transparent-png/WB-PIP-E-horizontal-RGB-high.png",
                alt = "Research Repository preview"
              )
            ),
            # Content area
            tags$div(
              class = "pip-tile-large__content",
              tags$h3(class = "pip-tile-large__title", "Research Repository"),
              tags$p(
                class = "pip-tile-large__desc",
                "Access methodological papers and analytics published across ", 
                "the World Bank. Use semantic search to find relevant work ", 
                "across all World Bank working papers."
              ),
              tags$div(
                class = "pip-tile-large__btn-wrap",
                actionLink(
                  inputId = ns("feature_research"),
                  label   = "Explore more",
                  class   = "pip-tile-large__btn"
                )
              )
            )
          )
        )
      )
    ),

    # ── 3. Featured Methods dark banner ───────────────────────────────────
    # Full-bleed navy section containing the four method tiles.
    # Tile colour is overridden to #243c56 (--pip-tile-featured) via CSS.
    tags$section(
      class = "pip-methods-banner",
      tags$div(
        class = "pip-methods-banner__inner",
        tags$h2(
          class = "pip-methods-banner__heading",
          "Featured Methods"
        ),
        tags$p(
          class = "pip-methods-banner__subheading",
          "The Deep Dives page currently features estimates produced by ",
          "methodologies from four different peer-reviewed papers. These ",
          "will be expanded over time."
        ),
        tags$div(
          class = "pip-grid-4",
          uiOutput(ns("card_yk")),
          uiOutput(ns("card_dm")),
          uiOutput(ns("card_stb")),
          uiOutput(ns("card_sn"))
        )
      )
    )
  )
}


#' Home Page Server
#'
#' @param id Module ID.
#' @param dm_metadata,stb_metadata,sn_metadata,yk_metadata One-row data frames
#'   with at least a \code{paper_url} column (URL to the method paper).
#' @return A named list of reactives:
#'   \describe{
#'     \item{method}{Character or NULL — method to pre-select in Deep Dives.}
#'     \item{target}{Character — tab to navigate to: \code{"deep_dives"} or
#'       \code{"research_repo"}.}
#'     \item{counter}{Integer — incremented on every navigation event.}
#'   }
#' @noRd
mod_home_server <- function(id,
                            dm_metadata,
                            stb_metadata,
                            sn_metadata,
                            yk_metadata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Helper: build a small method tile ──────────────────────────────────
    #
    # @param image_src  Path to the method preview image.
    # @param heading    Short description used as the bold tile heading.
    # @param paper_url  URL to the method paper ("Read paper" link). If NA/NULL,
    #                   the link is omitted.
    # @param click_id   Shiny input ID used by onclick to fire Deep Dives
    #                   navigation. Clicking the tile or "Explore" triggers this;
    #                   "Read paper" stops propagation to avoid double-navigation.
    #
    # @return A `shiny.tag` representing a `.pip-tile-small` card.
    build_card <- function(image_src, heading, paper_url, click_id) {
      tags$div(
        class   = "pip-tile-small",
        onclick = sprintf("Shiny.setInputValue('%s', Math.random())", click_id),

        # Image area
        tags$div(
          class = "pip-tile-small__image",
          tags$img(
            src = image_src,
            alt = heading
          )
        ),

        # Content area
        tags$div(
          class = "pip-tile-small__content",
          # Bold heading: short description (no paper title link)
          tags$p(class = "pip-tile-small__title", heading),
          # "Explore" CTA — clicking anywhere on the tile also fires click_id
          tags$span(class = "pip-tile-small__cta", "Explore"),
          # "Read paper" external link — stops tile onclick to avoid double-nav
          if (!is.null(paper_url) && !is.na(paper_url) && nzchar(paper_url)) {
            tags$a(
              href    = paper_url,
              target  = "_blank",
              class   = "pip-tile-small__paper-link",
              onclick = "event.stopPropagation()",
              "Read paper"
            )
          }
        )
      )
    }

    # ── Render method tiles ────────────────────────────────────────────────

    output$card_yk <- renderUI({
      build_card(
        image_src = "www/landing_yk.png",
        heading   = "Adjusting for the gap between survey means and national accounts to capture missing top incomes.",
        paper_url = yk_metadata$paper_url,
        click_id  = ns("click_yk")
      )
    })

    output$card_dm <- renderUI({
      build_card(
        image_src = "www/landing_dm.png",
        heading   = "Converting income distributions to consumption distributions for cross-country comparability.",
        paper_url = dm_metadata$paper_url,
        click_id  = ns("click_dm")
      )
    })

    output$card_stb <- renderUI({
      build_card(
        image_src = "www/landing_stb.png",
        heading   = "Applying household economies of scale via the square-root equivalence scale.",
        paper_url = stb_metadata$paper_url,
        click_id  = ns("click_stb")
      )
    })

    output$card_sn <- renderUI({
      build_card(
        image_src = "www/landing_sn.png",
        heading   = "Using globally consistent urban\u2013rural definitions to measure subnational poverty.",
        paper_url = sn_metadata$paper_url,
        click_id  = ns("click_sn")
      )
    })

    # ── Navigation events ──────────────────────────────────────────────────
    # A reactiveValues bundle is returned to the parent server so it can
    # switch tabs and optionally pre-select a method in Deep Dives.

    nav_event <- reactiveValues(method = NULL, counter = 0, target = "deep_dives")

    # Hero CTA: "Explore Deep Dives" → Deep Dives tab, no method pre-select
    observeEvent(input$banner_deep_dives, {
      nav_event$method  <- NULL
      nav_event$target  <- "deep_dives"
      nav_event$counter <- nav_event$counter + 1
    })

    # Hero CTA: "Research Repository" → Research Repository tab
    observeEvent(input$banner_research, {
      nav_event$method  <- NULL
      nav_event$target  <- "research_repo"
      nav_event$counter <- nav_event$counter + 1
    })

    # Large feature tile: Deep Dives → Deep Dives tab, no method pre-select
    observeEvent(input$feature_deep_dives, {
      nav_event$method  <- NULL
      nav_event$target  <- "deep_dives"
      nav_event$counter <- nav_event$counter + 1
    })

    # Large feature tile: Research Repository → Research Repository tab
    observeEvent(input$feature_research, {
      nav_event$method  <- NULL
      nav_event$target  <- "research_repo"
      nav_event$counter <- nav_event$counter + 1
    })

    # Small method tiles → Deep Dives tab with method pre-selected
    observeEvent(input$click_dm, {
      nav_event$method  <- "Welfare conversion"
      nav_event$target  <- "deep_dives"
      nav_event$counter <- nav_event$counter + 1
    })

    observeEvent(input$click_yk, {
      nav_event$method  <- "NA\u2013Survey gap adjustment"
      nav_event$target  <- "deep_dives"
      nav_event$counter <- nav_event$counter + 1
    })

    observeEvent(input$click_stb, {
      nav_event$method  <- "Household allocation"
      nav_event$target  <- "deep_dives"
      nav_event$counter <- nav_event$counter + 1
    })

    observeEvent(input$click_sn, {
      nav_event$method  <- "Subnational definition"
      nav_event$target  <- "deep_dives"
      nav_event$counter <- nav_event$counter + 1
    })

    # Return reactive list for the parent server to observe
    return(
      list(
        method  = reactive(nav_event$method),
        target  = reactive(nav_event$target),
        counter = reactive(nav_event$counter)
      )
    )
  })
}
