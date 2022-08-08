#set the correct working directory
#ctrl+A select all to run

library("knitr")
knit("../demo_keys.Rmd")


############################################################################
# Main
############################################################################
my_data_list <-
  list(
    "chemical" = chemical,
    "chemical2" = chemical2,
    "compTox" = compTox,
    "compTox2" = compTox2,
    "InChiKeys" = InChiKeys,
    "InChiKeys2" = InChiKeys2,
    "PubChem_IDs" = PubChem_IDs,
    "PubChem_IDs2" = PubChem_IDs2,
    "results" = results,
    "results2" = results2,
    "resultsCombined" = resultsCombined,
    "Koc_plot" = Koc_plot,
    "Kplot" = Kplot
  )
for (i in seq_along(my_data_list)) {
  name <- names(my_data_list)[i]
  data <- my_data_list[[i]]
  if (name == "chemical" || name == "chemical2") {
    test_column_names(data, c("SMILES"), name = name)
    test_dimensions(data, c(as.integer(10), as.integer(1)), name = name)
    test_column_type(data, "SMILES", "character", name = name)
    #check entry
    if (name == "chemical") {
      test_entry(
        data,
        "SMILES",
        10,
        "FC(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(=O)O",
        name = name
      )
    }
    if (name == "chemical2") {
      test_entry(
        data,
        "SMILES",
        10,
        "FC(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)CO",
        name = name
      )
    }
  }
  if (name == "compTox" || name == "compTox2") {
    test_column_names(
      data,
      c(
        as.character('Key'),
        as.character('Group'),
        as.character('CmpdID'),
        as.character('Name'),
        as.character('log_KOA'),
        as.character('log_KOW'),
        as.character('log_KAW'),
        as.character('log_KOC')
      ),
      name = name
    )
    test_dimensions(data, c(as.integer(10), as.integer(8)), name = name)
    test_column_type(data, "log_KOA", "double", name = name)
    test_column_type(data, "log_KOW", "double", name = name)
    test_column_type(data, "log_KAW", "double", name = name)
    test_column_type(data, "log_KOC", "double", name = name)
    if (name == "compTox") {
      test_entry(data, "Group", 10, "Group1", name = name)
      test_entry(data, "CmpdID", 10, 10, name = name)
    }
    if (name == "compTox2") {
      test_entry(data, "Group", 10, "Group2", name = name)
      test_entry(data, "CmpdID", 10, 10, name = name)
    }
  }
  if (name == "InChiKeys" || name == "InChiKeys2") {
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
  if (name == "PubChem_IDs" || name == "PubChem_IDs2") {
    test_column_names(data, c(as.character('query'), as.character('cid')), name = name)
    test_dimensions(data, c(as.integer(10), as.integer(2)), name = name)
    test_column_type(data, "query", "character", name = name)
    test_column_type(data, "cid", "character", name = name)
  }
  if (name == "results" || name == "results2") {
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
        as.character('log_KOA'),
        as.character('log_KOW'),
        as.character('log_KAW'),
        as.character('log_KOC')
      ),
      name = name
    )
    test_dimensions(data, c(as.integer(10), as.integer(11)), name = name)
    test_column_type(data, "log_KOA", "double", name = name)
    test_column_type(data, "log_KAW", "double", name = name)
    test_NA(data, "log_KOA", FALSE, name = name)
    test_NA(data, "log_KAW", FALSE, name = name)
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
        as.character('log_KOA'),
        as.character('log_KOW'),
        as.character('log_KAW'),
        as.character('log_KOC')
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
    test_column_type(data, "log_KOC", "double", name = name)
    test_column_type(data, "log_KOA", "double", name = name)
    test_column_type(data, "log_KOW", "double", name = name)
    test_column_type(data, "log_KAW", "double", name = name)
    test_NA(data, "log_KOC", FALSE, name = name)
    test_NA(data, "log_KOA", FALSE, name = name)
    test_NA(data, "log_KOW", FALSE, name = name)
    test_NA(data, "log_KAW", FALSE, name = name)
  }
  if (name == "Kplot" || name == "Koc_plot") {
    #print all structures under Kplot/Koc_plot
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
    test_column_type_plot(data, "data", "log_KOA", "double", name = name)
    test_column_type_plot(data, "data", "log_KOW", "double", name = name)
    test_column_type_plot(data, "data", "log_KAW", "double", name = name)
    test_column_type_plot(data, "data", "log_KOC", "double", name = name)

    test_entry_plot(data, "labels", 2, "y", "log KAW", name = name)
    test_entry_plot(data, "labels", 3, "colour", "Group", name = name)
    test_entry_plot(data, "labels", 4, "label", "CmpdID", name = name)

    if (name == "Kplot") {
      test_entry_plot(data, "labels", 1, "x", "log KOA", name = name)
    }
    if (name == "Koc_plot") {
      test_entry_plot(data, "labels", 1, "x", "log KOC", name = name)
    }
  }
}
