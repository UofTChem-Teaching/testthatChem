library("knitr")
knit("../demo2Tutorial1_keys.Rmd")
#source("../demo2Tutorial1_keys.Rmd", chdir = TRUE)
library(testthat)

#run: testthat::test_dir('test')

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc()

############################################################################
# Functions
############################################################################

# Test that the column names of data match the expected names.
test_column_names <- function(data, expected, name = "the data") {
  description <-
    paste("check if", name, "contains the correct column names")
  test_that(description, {
    expect_named(data, expected)
  })
}

# Test that the data matches the expected dimensions c(num_row, num_cols)
test_dimensions <- function(data, expected, name = "the data") {
  description <-
    paste("check if", name, "has the correct number of rows and columns")
  # !! used to display value in error label
  test_that(description, {
    expect_equal(dim(data), !!expected)
  })
}

# Test that the data has a column with the given name matching the expected type.
test_column_type <-
  function(data, col_name, expected, name = "the data") {
    description <-
      paste("check if",
            name,
            "has a column named",
            col_name,
            "with type",
            expected)
    test_that(description, {
      expect_type(data[[col_name]], expected)
    })
  }

test_entry <-
  function(data, col_name, row_index, expected, name = "the data") {
    description <-
      paste("check if",
            name,
            "has the correct value at row",
            row_index,
            "and column",
            col_name)
    test_that(description, {
      expect_equal(data[[col_name]][row_index], !!expected)
    })
  }

test_NA <- function(data, col_name, expected, name = "the data") {
  description <-
    paste("check if cells inside", name, "contains NA invalid rows")
  test_that(description, {
    expect_equal(any(is.na(data[[col_name]])),!!expected)
  })
}

test_dimensions_plot <-
  function(data, col_name, expected, name = "the data") {
    description <-
      paste("check if",
            name,
            "- column",
            col_name,
            "has the correct number of rows and columns")
    # !! used to display value in error label
    test_that(description, {
      expect_equal(dim(data[[col_name]]), !!expected)
    })
  }

test_column_type_plot <-
  function(data,
           table_name,
           col_name,
           expected,
           name = "the data") {
    description <-
      paste("check if",
            name,
            "has a column named",
            col_name,
            "with type",
            expected)
    test_that(description, {
      expect_type(data[[table_name]][[col_name]], expected)
    })
  }

test_entry_plot <-
  function(data,
           col_name,
           row_index,
           lb,
           expected,
           name = "the data") {
    description <-
      paste(
        "check if",
        name,
        "has the correct value of",
        expected,
        "at row",
        row_index,
        "of the column:",
        col_name,
        ",level:",
        lb
      )
    test_that(description, {
      expect_equal(data[[col_name]][row_index][[lb]], !!expected)
    })
  }


############################################################################
# Main
############################################################################
my_data_list <-
  list(
    "compounds" = compounds,
    "compounds2" = compounds2,
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
  if (name == "compounds" || name == "compounds2") {
    test_column_names(data, c("SMILES"), name = name)
    test_dimensions(data, c(as.integer(10), as.integer(1)), name = name)
    test_column_type(data, "SMILES", "character", name = name)
    #check entry
    if (name == "compounds") {
      test_entry(
        data,
        "SMILES",
        10,
        "FC(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(=O)O",
        name = name
      )
    }
    if (name == "compounds2") {
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
