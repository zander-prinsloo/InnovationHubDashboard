

plot_country_deepdives_multiple_methods <-
  function(plot_default,
           plot_alloc,
           plot_cons,
           plot_rurb1,
           plot_rurb2) {

  # return one of the five ggplot objects
  list("s1" = plot_default,
       "s2" = plot_alloc,
       "s3" = plot_cons,
       "s4" = plot_rurb1,
       "s5" = plot_rurb2)
}


plot_country_method_default <-
  function(d,
           main_title,
           subtitle_use,
           caption_use) {

    d[variable          == "headcount_default" &
        reporting_level == "national"] |>
    ggplot(mapping = aes(x = year,
                         y = value)) +

      ## One point per year
      geom_point(colour = "#0072B2",
                 size = 3) +

      ## Exact values – repel avoids clashes, keeps the graphic tidy
      geom_text_repel(d,
                      mapping     = aes(x      = year,
                                        y      = value,
                                        colour = variable,
                                        shape  = variable,
                                        label  = label_not),
                      nudge_x     = 0.15,
                      size        = 5,
                      show.legend = FALSE) +

      ## Scales / labels
      scale_x_continuous(breaks = d$year) +
      scale_y_continuous(expand = expansion(mult = c(0,
                                                     0.05))) +
      labs(title    = main_title,
           subtitle = paste0(substitle_use),
           caption  = caption_use,
           x        = NULL,
           y        = "Headcount (%)") +

      ## 3e.  Scales / labels
      scale_colour_manual(values = c(headcount_default  = "#0072B2",
                                     headcount_estimate = "#D55E00")) +
      scale_shape_manual(values  = c(headcount_default  = 16,
                                     headcount_estimate = 17)) +
      scale_x_continuous(breaks  = sort(unique(d_step2$year))) +
      scale_y_continuous(limits  = c(min(d$value) - 1,
                                     max(d$value) + 1),
                         expand  = expansion(mult = c(0,
                                                      .02))) +

      ## 3f.  Theme
      theme_minimal(base_size  = 11) +
      theme(panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position    = "none")

  }

plot_country_method_alloc <-
  function(d,
           main_title,
           subtitle_use,
           caption_use) {

    d_step2 <- d |>
      fsubset(!(year %in% c(2015, 2022)
                & variable == "headcount_estimate"))

    #–– Compute the 2019 gap once for annotation –––––––
    gap_2019 <- d |>
      fsubset(year == 2019) |>
      fsummarise(diff = diff(value))
    gap_2019 <- gap_2019$diff

    #–– Plot –––––––––––––––––––––––––––––––––––––––––––
    ggplot() +

      #–– Gap label right on the arrow tip ––
      annotate("text",
               x = 2019.25,
               y = max(d_step2$value[d_step2$year == 2019]) + .4,
               label = paste0("(+", round(gap_2019, 2), " pp)"),
               color = "#D55E00") +

      ## Arrow segment just for 2019
      geom_segment(
        data = d_step2 |>
          filter(year == 2019) |>
          summarise(x = first(year),
                    y = min(value),
                    yend = max(value)),
        aes(x    = x,
            xend = x,
            y    = y,
            yend = yend),
        linewidth = .7,
        colour = scales::alpha("#0072B2", .6),
        alpha = 0.6,
        arrow     = arrow(type   = "closed",
                          length = unit(.15, "cm"))) +

      ## Points
      geom_point(d_step2 |>
                   fsubset(year == 2019),
                 mapping = aes(x = year,
                               y = value,
                               colour = variable,
                               shape = variable),
                 size = 3) +

      geom_point(d_step2 |>
                   fsubset(!year == 2019),
                 alpha = 0.4,
                 mapping = aes(x = year,
                               y = value,
                               colour = variable,
                               shape = variable),
                 size = 3) +

      geom_text_repel(d_step2 |>
                        fsubset(year == 2019),
                      mapping = aes(x = year,
                                    y = value,
                                    colour = variable,
                                    shape = variable,
                                    label = label),
                      nudge_x = 0.15,
                      size    = 5,
                      show.legend = FALSE) +

      geom_text_repel(d_step2 |>
                        fsubset(!year == 2019),
                      alpha = 0.4,
                      mapping = aes(x = year,
                                    y = value,
                                    colour = variable,
                                    shape = variable,
                                    label = label_not),
                      nudge_x = 0.15,
                      size    = 5,
                      show.legend = FALSE) +

      scale_colour_manual(values = c(
        headcount_default  = "#0072B2",  # blue
        headcount_estimate = "#D55E00"   # orange
      )) +
      scale_shape_manual(values = c(
        headcount_default  = 16,         # solid circle
        headcount_estimate = 17          # triangle
      )) +

      scale_x_continuous(breaks = sort(unique(d_step2$year))) +
      scale_y_continuous(limits = c(min(d$value) - 1,
                                    max(d$value) + 1),
                         expand = expansion(mult = c(0,
                                                     .02))) +
      labs(title    = paste0(main_title,
                             " using different household consumption allocation rules"),
           subtitle = paste0(substitle_use),
           caption  = caption_use,
           x        = NULL,
           y        = "Headcount (%)",
           colour   = NULL,
           shape    = NULL) +

      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position    = "none")



  }
plot_country_method_consc <-
  function(d,
           main_title,
           subtitle_use,
           caption_use) {

    #–– 1.  Data for this step ––––––––––––––––––––––––––––––
    d_step3 <- d |>
      fsubset(!(year       == 2015 &
                  variable == "headcount_estimate"))


    #–– 2.  Compute the 2019 gap once for annotation –––––––
    gap_2022 <- d_step3 |>
      fsubset(year == 2022) |>
      fsummarise(diff = diff(value))
    gap_2022 <- gap_2022$diff

    #–– 3.  Plot –––––––––––––––––––––––––––––––––––––––––––
    ggplot() +

      #–– Gap label right on the arrow tip ––
      annotate("text",
               x     = 2022.25,
               y     = min(d_step3$value[d_step3$year == 2022]) - .4,
               label = paste0("(", round(gap_2022, 2), " pp)"),
               color = "#D55E00") +

      ## Arrow segment just for 2019
      geom_segment(
        data = d_step3 |>
          filter(year == 2022) |>
          summarise(x = first(year),
                    y = max(value),
                    yend = min(value)),
        aes(x    = x,
            xend = x,
            y = y,
            yend = yend),
        linewidth = .7,
        #colour    = "grey40",
        colour = scales::alpha("#0072B2", .6),
        alpha = 0.6,
        arrow     = arrow(type   = "closed",
                          length = unit(.15, "cm"))) +

      geom_point(d_step3 |>
                   fsubset(year == 2022),
                 mapping = aes(x = year,
                               y = value,
                               colour = variable,
                               shape = variable),
                 size = 3) +

      geom_point(d_step3 |>
                   fsubset(!year == 2022),
                 alpha = 0.4,
                 mapping = aes(x = year,
                               y = value,
                               colour = variable,
                               shape = variable),
                 size = 3) +

      geom_text_repel(d_step3 |>
                        fsubset(year == 2022),
                      mapping = aes(x = year,
                                    y = value,
                                    colour = variable,
                                    shape = variable,
                                    label = label),
                      nudge_x = 0.15,
                      size    = 5,
                      show.legend = FALSE) +

      geom_text_repel(d_step3 |>
                        fsubset(!year == 2022),
                      alpha = 0.4,
                      mapping = aes(x = year,
                                    y = value,
                                    colour = variable,
                                    shape = variable,
                                    label = label_not),
                      nudge_x = 0.3,
                      size    = 5,
                      show.legend = FALSE) +

      scale_colour_manual(values = c(
        headcount_default  = "#0072B2",  # blue
        headcount_estimate = "#D55E00"   # orange
      )) +
      scale_shape_manual(values = c(
        headcount_default  = 16,         # solid circle
        headcount_estimate = 17          # triangle
      )) +

      scale_x_continuous(breaks = sort(unique(d_step3$year))) +
      scale_y_continuous(limits = c(min(d$value) - 1, max(d$value) + 1),
                         expand = expansion(mult = c(0, .02))) +
      labs(title    = paste0(main_title,
                             " by converting income distributions to consumption distributions"),
           subtitle = paste0(substitle_use),
           caption  = caption_use,
           x        = NULL,
           y        = "Headcount (%)",
           colour   = NULL,
           shape    = NULL) +


      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position    = "none"
      )




  }
plot_country_method_rurb1 <-
  function(d,
           main_title,
           subtitle_use,
           caption_use) {

    #–– 1.  Data for this step ––––––––––––––––––––––––––––––
    d_step4 <- d |>
      fsubset(!(year               == 2015 &
                  method           == "db" &
                  !reporting_level == "national"))


    #–– 2.  Compute the 2019 gap once for annotation –––––––
    gap_2015 <- d_step4 |>
      fsubset(year == 2015) |>
      fsummarise(diff = diff(value))
    gap_2015 <- gap_2015$diff

    # single row just holding the min / max of the alternatives
    range_2015  <- d_step4 |>                        # every alt-method for 2015
      fsubset(year     == 2015,
              variable == "headcount_estimate") |>
      fsummarise(year = 2015,
                 ymin = min(value),
                 ymax = max(value))

    #–– 3.  Plot –––––––––––––––––––––––––––––––––––––––––––
    ggplot() +

      ## Points
      geom_point(d_step4 |>
                   fsubset(year == 2015),
                 mapping = aes(x = year,
                               y = value,
                               colour = variable,
                               shape = variable),
                 size = 3) +
      ## 2b. Vertical range bar (min–max of alt 2015)
      geom_linerange(data = range_2015,
                     aes(x    = year,
                         ymin = ymin,
                         ymax = ymax),
                     colour   = scales::alpha("#D55E00", .55),
                     linewidth = .9) +
      ## Points
      geom_point(d_step4 |>
                   fsubset(!year == 2015),
                 alpha = 0.4,
                 mapping = aes(x = year,
                               y = value,
                               colour = variable,
                               shape = variable),
                 size = 3) +



      ##  Exact value labels
      geom_text_repel(d_step4 |>
                        fsubset(year == 2015),
                      mapping = aes(x = year,
                                    y = value,
                                    colour = variable,
                                    shape = variable,
                                    label = label),
                      nudge_x = 0.15,
                      size    = 5,
                      show.legend = FALSE) +
      ##  Exact value labels
      geom_text_repel(d_step4 |>
                        fsubset(!year == 2015),
                      alpha = 0.4,
                      mapping = aes(x = year,
                                    y = value,
                                    colour = variable,
                                    shape = variable,
                                    label = label_not),
                      nudge_x = 0.15,
                      size    = 5,
                      show.legend = FALSE) +


      ## 3e.  Scales / labels
      scale_colour_manual(values = c(
        headcount_default  = "#0072B2",  # blue
        headcount_estimate = "#D55E00"   # orange
      )) +
      scale_shape_manual(values = c(
        headcount_default  = 16,         # solid circle
        headcount_estimate = 17          # triangle
      )) +
      # scale_x_continuous(breaks = d_step4$year) +
      # scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      scale_x_continuous(breaks = sort(unique(d_step4$year))) +
      scale_y_continuous(limits = c(min(d$value) - 1, max(d$value) + 1),
                         expand = expansion(mult = c(0, .02))) +
      labs(title    = paste0(main_title,
                             " using the Degree of Urbanization (DOU) approach for consistent subnational measurements"),
           subtitle = paste0(substitle_use),
           caption  = caption_use,
           x = NULL,
           y = "Headcount (%)",
           colour = NULL, shape = NULL) +

      ## 3f.  Theme
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position    = "none"
      )


  }
plot_country_method_rurb2 <-
  function(d,
           main_title,
           subtitle_use,
           caption_use) {




    #–– 1.  Data for this step ––––––––––––––––––––––––––––––
    d_step4b <- d |>
      fsubset(!(year == 2015 &
                  method == "dou" &
                  !reporting_level == "national"))


    #–– 2.  Compute the 2019 gap once for annotation –––––––
    gap_2015b <- d_step4b |>
      fsubset(year == 2015) |>
      fsummarise(diff = diff(value))
    gap_2015b <- gap_2015b$diff

    # single row just holding the min / max of the alternatives
    range_2015b  <- d_step4b |>                        # every alt-method for 2015
      fsubset(year == 2015,
              variable == "headcount_estimate") |>
      fsummarise(year = 2015,
                 ymin = min(value),
                 ymax = max(value))

    #–– 3.  Plot –––––––––––––––––––––––––––––––––––––––––––
    ggplot() +

      geom_point(d_step4b |>
                   fsubset(year == 2015),
                 mapping = aes(x = year,
                               y = value,
                               colour = variable,
                               shape = variable),
                 size = 3) +

      geom_linerange(data = range_2015b,
                     aes(x = year, ymin = ymin, ymax = ymax),
                     colour   = scales::alpha("#D55E00", .55),
                     linewidth = .9) +

      geom_point(d_step4b |>
                   fsubset(!year == 2015),
                 alpha = 0.4,
                 mapping = aes(x = year,
                               y = value,
                               colour = variable,
                               shape = variable),
                 size = 3) +

      geom_text_repel(d_step4b |>
                        fsubset(year == 2015),
                      mapping = aes(x = year,
                                    y = value,
                                    colour = variable,
                                    shape = variable,
                                    label = label),
                      nudge_x = 0.15,
                      size    = 5,
                      show.legend = FALSE) +

      geom_text_repel(d_step4b |>
                        fsubset(!year == 2015),
                      alpha = 0.4,
                      mapping = aes(x = year,
                                    y = value,
                                    colour = variable,
                                    shape = variable,
                                    label = label_not),
                      nudge_x = 0.15,
                      size    = 5,
                      show.legend = FALSE) +

      scale_colour_manual(values = c(
        headcount_default  = "#0072B2",  # blue
        headcount_estimate = "#D55E00"   # orange
      )) +
      scale_shape_manual(values = c(
        headcount_default  = 16,         # solid circle
        headcount_estimate = 17          # triangle
      )) +

      scale_x_continuous(breaks = sort(unique(d_step4b$year))) +
      scale_y_continuous(limits = c(min(d$value) - 1, max(d$value) + 1),
                         expand = expansion(mult = c(0, .02))) +
      labs(title    = paste0(main_title,
                             " using the Dartboard (DB) approach for consistent subnational measurements"),
           subtitle = paste0(substitle_use),
           caption  = caption_use,
           x = NULL,
           y = "Headcount (%)",
           colour = NULL, shape = NULL) +

      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position    = "none"
      )




  }

