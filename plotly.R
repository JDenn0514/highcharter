library(plotly)



lca_data_dvs %>%
  # make it a plotly object, this is similar to the ggplot() function
  # this also sets global aesthetics
  plot_ly(x = ~mean, y = ~lca_class) %>%
  #
  add_markers(
    color = ~dvs,
    colors = as.character(adl_palettes$categorical[1:4]),
    size = I(100)
  ) %>%
  # add_trace(
  #   type = "scatter",
  #   color = ~dvs,
  #   showlegend = TRUE
  # ) %>%
  add_segments(
    x = ~min, xend = ~max,
    yend = ~lca_class,
    showlegend = FALSE,
    color = I("black")
  ) %>%
  layout(
    title = "Plotly",
    xaxis = list(title = "Standard Deviations")
  )

schema()



