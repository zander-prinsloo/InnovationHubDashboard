#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # packages
  #---------------------------
  library(fastverse)
  library(dplyr)
  library(ggtext)
  library(ggrepel)
  library(viridis)
  library(ggalt)  # for geom_dumbbell
  library(scales)
  library(glue)
  library(grid)

  dir <- "C:/Users/wb612474/OneDrive - WBG/innovation_hub/temp_data_dashboard"
  # load titles
  titles <- list(
    main_title   = "Comparing poverty estimates for Colombia",
    subtitle_use = "Default PIP methodology at the $2.15 international poverty line",
    caption_use  = "Estimates supplied by authors; not strictly comparable across vintages.")

  dcol <- fst::read_fst(path = fs::path(dir,
                                        "dcol.fst"))

  mod_country_deepdives_multiple_methods_server(
    id     = "dd_col",
    d_all  = dcol,
    titles = titles
  )
}
