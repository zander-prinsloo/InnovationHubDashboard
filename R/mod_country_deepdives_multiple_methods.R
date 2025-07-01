#' country_deepdives_multiple_methods UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_country_deepdives_multiple_methods_ui <- function(id, md_dir) {
  ns    <- NS(id)
  dir   <- "C:/Users/wb612474/OneDrive - WBG/innovation_hub/temp_data_dashboard"
  dirmd <- fs::path("inst",
                    "app")
  ## read the markdown *once* when the UI is built ---------
  intro <- includeMarkdown(fs::path(dirmd,
                                    "paper_descriptions",
                                    "scrolly_intro_text.md"))
  md1   <- includeMarkdown(fs::path(dirmd,
                                    "paper_descriptions",
                                    "stettehbaah_2024-08-16.md"))
  md2   <- includeMarkdown(fs::path(dirmd,
                                    "paper_descriptions",
                                    "dmahler_2024-08-15.md"))
  #md3   <- includeMarkdown(fs::path(dir, "snakamura2_2024-08-30.md"))
  md3a   <- includeMarkdown(fs::path(dirmd,
                                     "paper_descriptions",
                                     "snakamura2_cleaned_intro.md"))
  md3b   <- includeMarkdown(fs::path(dirmd,
                                     "paper_descriptions",
                                     "snakamura2_cleaned_1.md"))
  md3c   <- includeMarkdown(fs::path(dirmd,
                                     "paper_descriptions",
                                     "snakamura2_cleaned_2.md"))
  metadata1 <- readxl::read_excel(path = fs::path(dirmd,
                                               "metadata",
                                               "stettehbaah_2024-08-16.xlsx"))
  briefdesc1 <- metadata1$description
  metadata2  <- readxl::read_excel(path = fs::path(dirmd,
                                               "metadata",
                                               "dmahler_2024-08-15.xlsx"))
  briefdesc2 <- metadata2$description
  metadata3  <- readxl::read_excel(path = fs::path(dirmd,
                                               "metadata",
                                               "snakamura2_2024-08-30.xlsx"))
  briefdesc3 <- metadata3$description

  tagList(
    scrollytell::scrolly_container(
      ns("scr"),

      scrollytell::scrolly_graph(
        plotOutput(ns("deepdive_plot"), height = "650px")
      ),

      scrollytell::scrolly_sections(
        scrollytell::scrolly_section(intro, id = "s1"),
        scrollytell::scrolly_section(briefdesc1, id = "s2"),
        scrollytell::scrolly_section(md2, id = "s3"),
        scrollytell::scrolly_section(briefdesc3, id = "s4"),
        #scrollytell::scrolly_section("testing", id = "s4a"),
        scrollytell::scrolly_section(md3a, id = "s5"),
        scrollytell::scrolly_section(md3b, id = "s5a"),
        scrollytell::scrolly_section("testing", id = "s5b"),
        scrollytell::scrolly_section("testing", id = "s5b"),
        scrollytell::scrolly_section(md3c, id = "s5c")
      )
      # scrollytell::scrolly_sections(
      #   scrollytell::scrolly_section(id = "s1", h1("Plot A")),
      #   scrollytell::scrolly_section(id = "s2", h1("Plot B")),
      #   scrollytell::scrolly_section(id = "s3", h1("Plot C")),
      #   scrollytell::scrolly_section(id = "s4", h1("Plot D")),
      #   scrollytell::scrolly_section(id = "s5", h1("Plot E"))
      # )
    )
  )
}


#' country_deepdives_multiple_methods Server Functions
#'
#' @noRd
mod_country_deepdives_multiple_methods_server <- function(id,
                                                          d_all,
                                                          titles){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    #dir <- "C:/Users/wb612474/OneDrive - WBG/innovation_hub/temp_data_dashboard"
    ## ---------- 1. read markdown files once ----------------------
    # md1 <- includeMarkdown(fs::path(dir, "stettehbaah_2024-08-16.md"))
    # md2 <- includeMarkdown(fs::path(dir, "dmahler_2024-08-15.md"))
    # md3 <- includeMarkdown(fs::path(dir, "snakamura2_2024-08-30.md"))
    #
    # output$md_1 <- renderUI(md1)
    # output$md_2 <- renderUI(md2)
    # output$md_3 <- renderUI(md3)

    ## ---------- 2. build plots (unchanged) -----------------------

    plots <-
      plot_country_deepdives_multiple_methods(
        plot_default = plot_country_method_default(d            = d_all,
                                                   main_title   = titles$main_title,
                                                   subtitle_use = titles$subtitle_use,
                                                   caption_use  = titles$caption_use),
        plot_alloc   = plot_country_method_alloc(d            = d_all,
                                                 main_title   = titles$main_title,
                                                 subtitle_use = titles$subtitle_use,
                                                 caption_use  = titles$caption_use),
        plot_cons    = plot_country_method_consc(d            = d_all,
                                                 main_title   = titles$main_title,
                                                 subtitle_use = titles$subtitle_use,
                                                 caption_use  = titles$caption_use),
        plot_rurb1   = plot_country_method_rurb1(d            = d_all,
                                                 main_title   = titles$main_title,
                                                 subtitle_use = titles$subtitle_use,
                                                 caption_use  = titles$caption_use),
        plot_rurb2   = plot_country_method_rurb2(d            = d_all,
                                                 main_title   = titles$main_title,
                                                 subtitle_use = titles$subtitle_use,
                                                 caption_use  = titles$caption_use)
      )

    scrl_num <- reactive({
      scr <- input$scr
      if (is.null(scr)) {
        return(NULL)
      }

      ## scrollytell sometimes returns a numeric index
      ## normalise to our "s1" … "s5" convention
      if (is.numeric(scr)) {
        scr <- paste0("s", scr)
      }

      as.character(scr)
    })

    ## 2b.  Render the one that matches the scroll section -------
    output$deepdive_plot <- renderPlot({
      req(input$scr)                               # "s1" … "s5"
      country_dd_plot(scrl_num(), plots)
    })

    ## 2c.  Keep scrollama in sync --------------------------------
    output$scr <- scrollytell::renderScrollytell(
      scrollytell::scrollytell()
    )

  })
}

## To be copied in the UI
# mod_country_deepdives_multiple_methods_ui("country_deepdives_multiple_methods_1")

## To be copied in the server
# mod_country_deepdives_multiple_methods_server("country_deepdives_multiple_methods_1")
