#set the correct working directory
#ctrl+A select all to run

library("knitr")
knit("../demo_keys.Rmd")
library(testthat)
library(testthatChem)
#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc()

############################################################################
# Main
############################################################################
my_data_list <-
  list(
    "material" = material,
    "material2" = material2,
    "var_T" = var_T,
    "var_T2" = var_T2,
    "chiKeys" = chiKeys,
    "chiKeys2" = chiKeys2,
    "IDs" = IDs,
    "IDs2" = IDs2,
    "results" = results,
    "results2" = results2,
    "resultsCombined" = resultsCombined,
    "b_plot" = b_plot,
    "bbplot" = bbplot
  )
for (i in seq_along(my_data_list)) {
  name <- names(my_data_list)[i]
  data <- my_data_list[[i]]
  if (name == "material" || name == "material2") {
    test_column_names(data, c("SMILES"), name = name)
    test_dimensions(data, c(as.integer(10), as.integer(1)), name = name)
    test_column_type(data, "SMILES", "character", name = name)
    #check entry
    if (name == "material") {
      test_entry(
        data,
        "SMILES",
        10,
        "FC(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(=O)O",
        name = name
      )
    }
    if (name == "material2") {
      test_entry(
        data,
        "SMILES",
        10,
        "FC(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)CO",
        name = name
      )
    }
  }
  if (name == "var_T" || name == "var_T2") {
    test_dimensions(data, c(as.integer(10), as.integer(8)), name = name)
    test_column_type(data, "log_po", "double", name = name)
    if (name == "var_T") {
      test_entry(data, "Group", 10, "Group1", name = name)
      test_entry(data, "CmpdID", 10, 10, name = name)
    }
    if (name == "var_T2") {
      test_entry(data, "Group", 10, "Group2", name = name)
      test_entry(data, "CmpdID", 10, 10, name = name)
    }
  }
  if (name == "chiKeys" || name == "chiKeys2") {
    test_column_names(data,
                      c(
                        as.character('CID'),
                        as.character('MolecularFormula'),
                        as.character('InChIKey')
                      ),
                      name = name)
    test_dimensions(data, c(as.integer(10), as.integer(3)), name = name)
    test_column_type(data, "CID", "character", name = name)
    test_column_type(data, "MolecularFormula", "character", name = name)
    test_column_type(data, "InChIKey", "character", name = name)
    test_NA(data, "CID", FALSE, name = name)
  }
  if (name == "IDs" || name == "IDs2") {
    test_column_names(data, c(as.character('query'), as.character('cid')), name = name)
    test_dimensions(data, c(as.integer(10), as.integer(2)), name = name)
    test_column_type(data, "query", "character", name = name)
    test_column_type(data, "cid", "character", name = name)
  }
  if (name == "results" || name == "results2") {
    test_dimensions(data, c(as.integer(10), as.integer(11)), name = name)
    test_column_type(data, "log_po", "double", name = name)
    test_column_type(data, "log_pa", "double", name = name)
    test_NA(data, "log_po", FALSE, name = name)
    test_NA(data, "log_pa", FALSE, name = name)
  }
  if (name == "resultsCombined") {
    test_dimensions(data, c(as.integer(20), as.integer(11)), name = name)
    test_column_names(
      data,
      c(
        as.character('SMILES'),
        as.character('cid'),
        as.character('MolecularFormula'),
        as.character('InChIKey'),
        as.character('Group'),
        as.character('CmpdID'),
        as.character('Name'),
        as.character('log_po'),
        as.character('log_poW'),
        as.character('log_pa'),
        as.character('log_poC')
      ),
      name = name
    )
    test_column_type(data, "SMILES", "character", name = name)
    test_column_type(data, "cid", "character", name = name)


    test_column_type(data, "MolecularFormula", "character", name = name)
    test_column_type(data, "InChIKey", "character", name = name)
    test_column_type(data, "Group", "character", name = name)
    test_column_type(data, "CmpdID", "integer", name = name)
    test_column_type(data, "Name", "character", name = name)
    test_column_type(data, "log_poC", "double", name = name)
    test_column_type(data, "log_po", "double", name = name)
    test_column_type(data, "log_poW", "double", name = name)
    test_column_type(data, "log_pa", "double", name = name)
    test_NA(data, "log_poC", FALSE, name = name)
    test_NA(data, "log_po", FALSE, name = name)
    test_NA(data, "log_poW", FALSE, name = name)
    test_NA(data, "log_pa", FALSE, name = name)
  }
  if (name == "bbplot" || name == "b_plot") {
    #print all structures under bbplot/b_plot
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
    test_dimensions_plot(data, "data",  c(as.integer(20), as.integer(11)), name = name)
    test_column_type_plot(data, "data", "log_po", "double", name = name)
    test_entry_plot(data, "labels", 4, "label", "CmpdID", name = name)

    if (name == "bbplot") {
      test_entry_plot(data, "labels", 1, "x", "log KOA", name = name)
    }
    if (name == "b_plot") {
      test_entry_plot(data, "labels", 1, "x", "log KOC", name = name)
    }
  }
}
