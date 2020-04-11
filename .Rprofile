theme_josi <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        size = .75
      ),
      panel.grid = ggplot2::element_line(),
      panel.border = ggplot2::element_rect(size = .5, fill = NA, color = "#524d4d"),
      #text = ggplot2::element_text(family = "HelveticaNeue-CondensedBold"),
      plot.title = ggplot2::element_text(color = "#1c1b19", size = 18),
      plot.subtitle = ggplot2::element_text(color = "#2e2828", size = 15),
      plot.caption = ggplot2::element_text(
        color = "#524d4d", size = 8,
        hjust = 1,
        margin = ggplot2::margin(t = 10)#,
        # family = "Avenir Next Condensed Medium"
      )
    )

}

ggplot2::theme_set(theme_josi())
