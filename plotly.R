library(tidyverse)
library(haven)
library(labelled)
library(plotly)
library(adlgraphs)
library(adldata)
library(htmlwidgets)




# data cleaning -----------------------------------------------------------

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



# dumbbell plot -----------------------------------------------------------

lca_data_dvs


p <- lca_data_dvs %>%
  # make it a plotly object, this is similar to the ggplot() function
  # this also sets global aesthetics
  plot_ly(color = ~dvs, colors = adl_palettes$categorical[1:4]) %>%
  #
  add_segments(
    x = ~min, xend = ~max,
    y = ~lca_class, yend = ~lca_class,
    showlegend = FALSE,
    color = I("black")
  ) %>%
  # add_trace(
  #   type = "scatter",
  #   mode = "marker",
  #   marker = list(
  #
  #   )
  # )
  add_markers(
    x = ~mean,
    y = ~lca_class,
    #data = lca_data_dvs %>% filter(dvs == "Jewish Conspiracy Theories"),
    # color = ~dvs,
    # colors = as.character(adl_palettes$categorical[1:4]),
    size = I(100),
    #fill = "toself",
    opacity = 1
  ) %>%
  # add_trace(
  #   type = "scatter",
  #   mode = "markers",
  #   color = ~dvs,
  #   showlegend = TRUE
  # ) %>%

  layout(
    title = "Hateful Hostiles Score Above Average on All Four Measures and<br>Egalitarian Humanitarians are Below Average on All Four Measures",
    xaxis = list(title = "Standard Deviations"),
    yaxis = list(title = "")
  )

p

saveWidget(p, "p2.html", selfcontained = F, libdir = "lib")


schema()
adl_palettes$categorical[1]


# faceting a plot ---------------------------------------------------------

