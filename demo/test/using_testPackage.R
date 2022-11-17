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

######################################
##Test for rollmean data
######################################
#this test checks the length of the rollmeanS value
test_value_lengths(rollmeanS, 160, name = "rollmeanS")
#this test checks calculations, here it is comparing if the max(rollmeanS) is consistent with our solution
test_value_cal(max(rollmeanS, na.rm = TRUE), 59.571429, name = "rollmeanS")

######################################
##Test for df data (tibble)
######################################
#This test checks all the column names of the df data against our solution,
#    for example, names(df) will print out all the column names of df
test_column_names(df,
                  c(
                    as.character('Elements'),
                    as.character('xbar'),
                    as.character('sigma'),
                    as.character('rsd')
                  ),
                  name = "df")
#This test checks the data dimension of the df data against our solution,
#    for example, dim(df) will print out the dimension of df
test_dimensions(df, c(as.integer(3), as.integer(4)), name = "df")
#This test checks the data type of each column of the df data against our solution,
#    for example, the column df$Elements contains "character" data type
#    for example, the column df$xbar contains "double" data type
test_column_type(df, "Elements", "character", name = "df")
test_column_type(df, "xbar", "double", name = "df")
#This test checks the value of the df data against our solution, we used expect_equal()
#    here, we check if df$rsd[3] is equals to 8.3996056
test_entry(df, "rsd", 3, 8.3996056, name = "df")

######################################
##Test for correlation data
######################################
#Check plot data structures and data attributes

#This test checks if the correlation plot contains the correct column names
test_column_names(
  correlation,
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
  name = "correlation"
)
#This test checks if the correlation$data contains the correct data dimension
test_dimensions_plot(correlation, "data",  c(as.integer(166), as.integer(5)), name = "correlation")
#This test checks if the correlation$data$employ contains the correct data type
test_column_type_plot(correlation, "data", "unemploy", "integer", name = "correlation")
#This test checks if the correlation$data$psavert[3] has the correct entry which is 18
test_entry_data_plot(correlation,
                     "data",
                     2,
                     "psavert",
                     3,
                     18,
                     name = "correlation")
#The following two tests check if the x and y label of a plot exist.
#When typeof(correlation$labels[1])=="list" means the labels exist
# > correlation$labels[1]
# >$x
#  [1] "unemploy"
test_exist_label_plot(correlation, "labels", 1, "list")
test_exist_label_plot(correlation, "labels", 2, "list")#type of list existed

######################################
##Test for a_plot data
######################################
#The following test checks if typeof(a_plot$labels[1])=="list", if it is, then the label exist
test_exist_label_plot(a_plot, "labels", 1, "list")
