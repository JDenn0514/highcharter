library(adldata)
library(adlgraphs)
library(tidyverse)
library(haven)
library(labelled)
library(highcharter)
library(htmlwidgets)


# data cleaning -----------------------------------------------------------


rm(pol_pos)
pol_pos <- pol_pos %>%
  mutate(
    lca_class = labelled(
      lca_class,
      labels = c(
        "Egalitarian Humanitarians (25%)" = "Class 1",
        "Hateful Hostiles (9%)" = "Class 2",
        "Antagonistic Advocates (19%)" = "Class 3",
        "Distant Dissidents (16%)" = "Class 4",
        "Stalwart Partisans (32%)" = "Class 5"
      )
    ),
    lca_class_f = as_factor(lca_class),
    # flip pro-palestinian so it's anti-palestinian
    factor_score_pro_pal_flip = labelled(
      0 - factor_score_pro_pal,
      label = "Anti-Palestinian/Hamas"
    ),

    # reverse the variables in malice
    across(
      c(isr_way, suffering, indifferent, commit_genocide, isr_policy),
      num_rev,
      .names = "{col}_rev"
    ),

    # create the scores
    # ADL Index/Jewish conspiracy theories
    trad_n_rescale = jtools::gscale(trad_n, n.sd = 1) %>%
      structure(
        label = "Jewish Conspiracy Theories",
        transformation = "Standardized `trad_n` by 1 SD"
      ),
    # malice sum
    malice_sum = rowSums(across(c(isr_way_rev, suffering_rev, indifferent_rev, commit_genocide_rev))) %>%
      structure(
        label = "Israeli Malice",
        transformation = "Sum of `isr_way_rev`, `suffering_rev`, `indifferent_rev`, `commit_genocide_rev`"
      ),
    # malice standardized
    malice_sum_rescale = jtools::gscale(malice_sum, n.sd = 1) %>%
      structure(
        label = "Israeli Malice",
        transformation = "Standardized `malice_sum` by 1 SD"
      ),
    # legitimacy sum
    legit_sum = rowSums(across(c(prod_isr, comf_isr, vote_isr, petition, mil_aid))) %>%
      structure(
        label = "General Anti-Israel Sentiment",
        transformation = "Simple sum of `prod_isr`, `comf_isr`, `vote_isr`, `petition`, `mil_aid`"
      ),
    #legitimacy standardized
    legit_sum_rescale = jtools::gscale(legit_sum, n.sd = 1) %>%
      structure(
        label = "General Anti-Israel Sentiment",
        transformation = "Standardized `legit_sum` by 1 SD"
      ),
    # dislike jews / anti-Jewish social network
    dislike_jews_rescale = jtools::gscale(dislike_jews, n.sd = 1) %>%
      structure(
        label = "Anti-Jewish Social Network",
        transformation = "Standardized `dislike_jews` by 1 SD"
      ),
    # conspiracy theory
    acts_avg_rescale = jtools::gscale(acts_avg, n.sd = 1) %>%
      structure(
        label = "Conspiracy Theory Belief",
        transformation = "Standardized `acts_avg` by 1 SD"
      ),
    # reverse oppressor vs oppressed
    vs_rev = num_rev(vs),
    # rescale it
    vs_rev_rescale = jtools::gscale(vs_rev, n.sd = 1) %>%
      structure(
        label = "Oppressor vs Oppressed Worldview",
        transformation = "Standardized `vs` by 1 SD"
      ),
    # social dominance orientation
    sdo_avg_rescale = jtools::gscale(sdo_avg, n.sd = 1) %>%
      structure(
        label = "Social Dominance Orientation",
        transformation = "Standardized `sdo_avg` by 1 SD"
      )
  )




# try to make a bar plot --------------------------------------------------

df <- pol_pos %>%
  get_freqs(trad_n, race_f) %>%
  drop_na(everything()) %>%
  #select(pct, gender_f2, trad_n) %>%
  pct_conv() %>%
  group_by(trad_n) %>%
  mutate(
    min = min(pct),
    max = max(pct)
  )

df %>%
  hchart(
    type = "columnrange",
    hcaes(y = pct, x = trad_n, low = min, high = max),
    color = "black",
  ) %>%
  hc_add_series(
    data = df,
    "scatter",
    hcaes(y = pct, x = trad_n, group = race_f),
  ) %>%
  hc_plotOptions(
    columnrange = list(
      maxPointWidth = 10
    ),
    scatter = list(                   # put line here instead of area
      #stacking = input$stacked,
      lineColor = "#ffffff",
      lineWidth = 0,
      marker = list(
        lineWidth = 0,
        radius = 7.5,
        lineColor = "#ffffff"
      )
    )
  )



# lca dv analysis ---------------------------------------------------------

# write out a function
mean_fun <- function(factor) {
  pol_pos %>%
    get_means({{ factor }}, lca_class, wt = wts) %>%
    add_column(dvs = labelled::var_label(pol_pos[[factor]]))
}

mean_fun("trad_n_rescale")

# get the dvs used in the graph
dvs <- c("trad_n_rescale", "malice_sum_rescale", "legit_sum_rescale", "factor_score_pro_pal_flip")

# calculate the data for the graph
# map the vari
lca_data_dvs <- map(dvs, mean_fun) %>%
  # since each object in the list is df we can bind them together
  list_rbind() %>%
  mutate(
    # reorder the legend
    dvs = refactor(
      dvs,
      c("Anti-Palestinian/Hamas", "Israeli Malice", "General Anti-Israel Sentiment", "Jewish Conspiracy Theories"),
    ),
    # reorder the y axis
    lca_class = refactor(
      lca_class,
      c("Stalwart Partisans (32%)", "Egalitarian Humanitarians (25%)", "Antagonistic Advocates (19%)", "Distant Dissidents (16%)", "Hateful Hostiles (9%)")
    )
  ) %>%
  group_by(lca_class) %>%
  mutate(
    min = min(conf.low) + 0.05,
    max = max(conf.high) - 0.05
  )

# set the levels of the axis
lvls <- lca_data_dvs %>%
  mutate(lca_class = fct_rev(lca_class)) %>%
  pull(lca_class) %>%
  levels()

hc <- lca_data_dvs %>%
  hchart(
    type = "columnrange",
    hcaes(x = lca_class, y = mean, low = min, high = max),
  ) %>%
  hc_add_series(
    data = lca_data_dvs,
    "scatter",
    hcaes(x = lca_class, y = mean, group = dvs)
  ) %>%
  hc_plotOptions(
    series = list(
      # make it so that it only gives the values when hovering directly over the points
      stickyTracking = FALSE,
      style = list(
        fontFamily = "Roboto"
      )
    ),
    columnrange = list(
      # adjust the thickness of the bar (in px)
      maxPointWidth = 6,
      # make the bar black
      color = "black",
      # THIS TURNS OFF THE HOVER AND TOOLTIP FOR THE COLUMNS
      enableMouseTracking = FALSE
    ),
    # update the scatter plot
    scatter = list(
      # remove the linewidth so there is no linewidth (in px)
      lineWidth = 0,
      # adjust the points
      marker = list(
        # set the size of the circles (in px)
        radius = 9
      ),
      # show the legend
      showInLegend = TRUE
    )
  ) %>%
  # specify the y-axis (actually the x-axis since this is inverted)
  hc_yAxis(
    # specify the title
    title = list(
      # this is the text of the title
      text = "Score Relative to Overall Average<br>(In Standard Deviations)",
      # this is the styling of the fonts title
      style = list(
        # the font family
        fontFamily = "Roboto",
        # the size of the font
        fontSize = "20px"
      ),
      # we want to use HTML formatting
      useHTML = TRUE,
      # align the text to the center
      textAlign = "center"
    ),
    tickInterval = 0.5,
    plotLines = list(
      # add a solid black line at y = 0
      list(
        # specify that it is black
        color = "black",
        # speicify the width of the line
        width = 2,
        # specify that it is at 0
        value = 0
      )
    )
  ) %>%
  # remove the title from the x-axis (since this inverted it's actually for the y-axis)
  hc_xAxis(
    title = NULL,
    categories = lvls
  ) %>%
  # add the title
  hc_title(
    # you need to specify the text that goes into the title
    text = "Hateful Hostiles Score Above Average on All Four Measures and<br>Egalitarian Humanitarians are Below Average on All Four Measures",
    style = list(
      # here you specify the family of the font
      fontFamily = "Roboto",
      fontSize = "24px",
      fontWeight = "bold"
    ),
    useHtml = TRUE
  ) %>%
  # update tooltip
  hc_tooltip(
    # specify the width of the border (in px)
    borderWidth = 2,
    distance = 18,
    pointFormat = "{series.name}: <b>{point.y}</b>"
  ) %>%
  # customize legend
  hc_legend(
    layout = "horizontal",
    verticalAlign = "top",
    align = "center"
  ) %>%
  # adjust the size of the chart
  hc_size(width = 1080, height = 400) %>%
  # This takes the argument for inverted = TRUE
  hc_chart(inverted = TRUE) %>%
  # update the colors
  hc_colors(
    as.character(adl_palettes$categorical[1:4])
  )


hc

saveWidget(hc, "non_contained.html", selfcontained = FALSE, libdir = "external_resources")


