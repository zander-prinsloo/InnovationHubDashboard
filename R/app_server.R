#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # 1a) load each dataset
  load("data/d_dm.rda")   # creates object d_dm
  load("data/d_stb.rda")  # creates object d_stb
  load("data/d_sn.rda")   # creates object d_sn
  load("data/d_yk.rda")   # creates object d_yk

  dm_metadata  <- readxl::read_excel("data/dm_metadata.xlsx")
  stb_metadata <- readxl::read_excel("data/stb_metadata.xlsx")
  sn_metadata  <- readxl::read_excel("data/sn_metadata.xlsx")
  yk_metadata  <- readxl::read_excel("data/yk_metadata.xlsx")

  # NOTE: packages below are loaded at runtime because the golem app
  # relies on their side-effects (e.g. tidyverse attaching dplyr/ggplot2).
  # A future refactor should replace these with explicit namespace calls.
  require(fastverse, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  require(ggtext, quietly = TRUE)
  require(ggrepel, quietly = TRUE)
  require(scales, quietly = TRUE)
  require(glue, quietly = TRUE)
  require(grid, quietly = TRUE)

  # 1b) preprocess d_sn: remove default method, drop unneeded cols, join country_name
  country_lookup <- data.frame(
    code = c("AGO","BFA","BGD","TCD","CIV","COL","EGY","ETH","GAB",
             "GHA","GIN","GNB","LSO","MRT","MWI","NER","SEN","TZA","UGA","VNM"),
    country_name = c("Angola","Burkina Faso","Bangladesh","Chad",
                     "Côte d'Ivoire","Colombia","Egypt","Ethiopia","Gabon",
                     "Ghana","Guinea","Guinea-Bissau","Lesotho","Mauritania",
                     "Malawi","Niger","Senegal","Tanzania","Uganda","Vietnam")
  )
  data_sn <- d_sn |>
    fsubset(method != "default") |>
    fselect(-c(headcount_default, population_share_default, welfare_type)) |>
    merge(country_lookup, by = "code", all.x = TRUE)

  # 1c) create cross-country SN dataset: pivot DB→headcount_default, DOU→headcount_estimate
  data_sn_cross <- d_sn |>
    fsubset(sub_level == "" & method %in% c("db", "dou")) |>
    fselect(code, region_code, year, reporting_level, poverty_line, method, headcount_estimate) |>
    tidyr::pivot_wider(
      names_from  = method,
      values_from = headcount_estimate,
      names_prefix = "hc_"
    ) |>
    dplyr::mutate(
      headcount_default  = hc_db  * 100,
      headcount_estimate = hc_dou * 100
    ) |>
    dplyr::select(-hc_db, -hc_dou) |>
    merge(country_lookup, by = "code", all.x = TRUE) |>
    as.data.frame()

  # 1d) combine, dropping the unwanted cols
  data_cntry_plot <- rowbind(d_stb |>
                               fselect(-c(pip_vintage,
                                          welfare_type)),
                             d_dm |>
                               fselect(-c(welfare_type,
                                          gini_default,
                                          gini_estimate)))

  # ── Navigation: tab switching ────────────────────────────────────────────
  # Reactive to pass method override into Deep Dives module
  method_override <- reactiveVal(NULL)

  # Helper to switch active nav styling
  switch_nav <- function(active_tab) {
    shinyjs::removeClass(id = "nav_home",          class = "active")
    shinyjs::removeClass(id = "nav_deep_dives",    class = "active")
    shinyjs::removeClass(id = "nav_research_repo", class = "active")
    shinyjs::addClass(id = active_tab, class = "active")
  }

  # Header nav: Home
  observeEvent(input$nav_home, {
    updateTabsetPanel(session, "main_tabs", selected = "home")
    switch_nav("nav_home")
  })

  # Header nav: Deep Dives
  observeEvent(input$nav_deep_dives, {
    updateTabsetPanel(session, "main_tabs", selected = "deep_dives")
    switch_nav("nav_deep_dives")
  })

  # Header nav: Research Repository
  observeEvent(input$nav_research_repo, {
    updateTabsetPanel(session, "main_tabs", selected = "research_repo")
    switch_nav("nav_research_repo")
  })

  # ── Home page module ─────────────────────────────────────────────────────
  home_nav <- mod_home_server(
    id           = "home_1",
    dm_metadata  = dm_metadata,
    stb_metadata = stb_metadata,
    sn_metadata  = sn_metadata,
    yk_metadata  = yk_metadata
  )

  # When a card image or banner Deep Dives link is clicked, go to Deep Dives
  observeEvent(home_nav$counter(), {
    updateTabsetPanel(session, "main_tabs", selected = "deep_dives")
    switch_nav("nav_deep_dives")
    if (!is.null(home_nav$method())) {
      method_override(home_nav$method())
    }
  }, ignoreInit = TRUE)

  # ── Deep Dives module ────────────────────────────────────────────────────
  mod_interactive_dashboard_server(
    id              = "interactive_dashboard_1",
    data_dm         = d_dm,
    data_stb        = d_stb,
    data_sn         = data_sn,
    data_sn_cross   = data_sn_cross,
    data_yk         = d_yk,
    dm_metadata     = dm_metadata,
    stb_metadata    = stb_metadata,
    sn_metadata     = sn_metadata,
    yk_metadata     = yk_metadata,
    method_override = method_override
  )
}

