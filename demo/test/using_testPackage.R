#######################################
# NOTE THAT
# This demo code shows that what tests can be ran
#######################################

#set the correct working directory
#ctrl+A select all to run

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

library("knitr")
knit("../demo_keys.Rmd")

library(testthat)
library(testthatChem)

############################################################################
# Main
############################################################################
my_data_list <-
  list(
    "rollmeanS" = rollmeanS,
    "df" = df,
    "correlation" = correlation,
    "a_plot" = a_plot
  )
for (i in seq_along(my_data_list)) {
  name <- names(my_data_list)[i]
  data <- my_data_list[[i]]
  if (name == "rollmeanS") {
    test_value_lengths(data, 160, name = name)
    test_value_cal(max(data, na.rm = TRUE), 59.571429, name = name)
  }
  if (name == "df") {
    test_column_names(data,
                      c(
                        as.character('Elements'),
                        as.character('xbar'),
                        as.character('sigma'),
                        as.character('rsd')
                      ),
                      name = name)
    test_dimensions(data, c(as.integer(3), as.integer(4)), name = name)
    test_column_type(data, "Elements", "character", name = name)
    test_column_type(data, "xbar", "double", name = name)
    test_entry(data, "rsd", 3, 8.3996056, name = name)
  }
  if (name == "correlation" ||
      name == "a_plot") {
    #check plot data structures and data attributes
    test_column_names(
      data,
      c(
        as.character('data'),
        as.character('layers'),
        as.character('scales'),
        as.character('mapping'),
        as.character('theme'),
        as.character('coordinates'),
        as.character('facet'),
        as.character('plot_env'),
        as.character('labels')
      ),
      name = name
    )
    test_dimensions_plot(data, "data",  c(as.integer(166), as.integer(5)), name = name)
    test_column_type_plot(data, "data", "unemploy", "integer", name = name)

    if (name == "correlation") {
      test_entry_data_plot(correlation, "data", 2, "psavert", 3, correlation$data$psavert[3], name = name)
      test_exist_label_plot(data,"labels", 1, typeof(correlation$labels[1]))
      test_exist_label_plot(data,"labels", 2, typeof(correlation$labels[2]))#type of list existed
    }
    if (name == "a_plot") {
      test_exist_label_plot(data,"labels", 1, typeof(a_plot$labels[1]))
    }
  }

}
