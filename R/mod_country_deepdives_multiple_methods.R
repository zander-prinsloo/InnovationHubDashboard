#' country_deepdives_multiple_methods UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_country_deepdives_multiple_methods_ui <- function(id) {
  ns <- NS(id)

  tagList(
    scrollytell::scrolly_container(
      ns("scr"),                      # <-- JUST the ID, no argument name

      scrollytell::scrolly_graph(
        plotOutput(ns("deepdive_plot"), height = "650px")
      ),

      scrollytell::scrolly_sections(
        scrollytell::scrolly_section(id = "s1", h3("Default PIP methodology")),
        scrollytell::scrolly_section(id = "s2", h3("Household allocation rules (2019)")),
        scrollytell::scrolly_section(id = "s3", h3("Income → consumption conversion (2022)")),
        scrollytell::scrolly_section(id = "s4", h3("DOU sub-national approach (2015)")),
        scrollytell::scrolly_section(id = "s5", h3("Dartboard sub-national approach (2015)"))
      )
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
    ## 2a.  Build the five plots ONCE ----------------------------
    plots <- reactiveVal({
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
    })

    ## 2b.  Render the one that matches the scroll section -------
    output$deepdive_plot <- renderPlot({
      req(input$scr)                               # "s1" … "s5"
      country_dd_plot(input$scr, plots())
    })

    ## 2c.  Keep scrollama in sync --------------------------------
    output$scr <- scrollytell::renderScrollytell(scrollytell::scrollytell())
  })
}

## To be copied in the UI
# mod_country_deepdives_multiple_methods_ui("country_deepdives_multiple_methods_1")

## To be copied in the server
# mod_country_deepdives_multiple_methods_server("country_deepdives_multiple_methods_1")
