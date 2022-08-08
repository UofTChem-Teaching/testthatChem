library("knitr")
knit("../demo2Tutorial1_keys.Rmd")

library(testthat)

#run: testthat::test_dir('test')

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc()


## If you want to access Rmd function after knit("../demo.Rmd")

# test_that("positive integers can be added", {
#   expect_equal(sum(5, 5), 10)
#   expect_equal(sum(8, 2), 10)
#   expect_equal(sum(1, 1), 2)
# })
#


# Test that the column names of data match the expected names.
test_column_names <- function(data, expected, name = "the data") {
  description <-
    paste("check if",
          name,
          "contains the correct column names")
  
  test_that(description, {
    expect_named(data, expected)
  })
}

# Test that the data matches the expected dimensions c(num_row, num_cols)
test_dimensions <- function(data, expected, name = "the data") {
  description <-
    paste("check if",
          name,
          "has the correct number of rows and columns")
  
  test_that(description, {
    expect_equal(dim(data),!!expected)  # !! used to display value in error label
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
      expect_equal(data[[col_name]][row_index],!!expected)
    })
  }



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
  check_column <-
    paste("check if",
          name,
          "contains columne names and has correct column size")
  check_column_name <-
    paste("check if",
          name,
          "contains correct column names")
  check_dim <-
    paste("check if",
          name,
          "contains columne names and has correct column size")
  check_internal_type <-
    paste("check if cells inside tibble",
          name,
          "contains correct data objects/types")
  check_NA <-
    paste("check if cells inside tibble",
          name,
          "contains NA invalid rows")
  check_values <-
    paste("check if cells/plots ",
          name,
          "are imported correctly and have the correct values")
  #print(checks) or print(paste(i,name,data)) if we want all prints
  if (name == "compounds" ||
      name == "compounds2") {
    test_column_names(data, c("SMILES"), name = name)
    test_dimensions(data, c(10, 1), name = name)
    test_column_type(data, "SMILES", "character", name = name)
    
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
  
  if (name == "compTox" ||
      name == "compTox2") {
    test_that(check_column, {
      expect_equal(length(names(data)), 8)
    })
    test_that(check_column_name, {
      expect_equal(
        names(data),
        c(
          as.character('Key'),
          as.character('Group'),
          as.character('CmpdID'),
          as.character('Name'),
          as.character('log_KOA'),
          as.character('log_KOW'),
          as.character('log_KAW'),
          as.character('log_KOC')
        )
      )
    })
    test_that(check_dim, {
      expect_equal(dim(data), c(as.integer(10), as.integer(8)))
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KOA), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KOW), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KAW), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KOC), "double")
    })
    if (name == "compTox") {
      test_that(check_values, {
        expect_equal(data$Group[10], "Group1")
      })
      test_that(check_values, {
        expect_equal(data$CmpdID[10], 10)
      })
    }
    if (name == "compTox2") {
      test_that(check_values, {
        expect_equal(data$Group[10], "Group2")
      })
      test_that(check_values, {
        expect_equal(data$CmpdID[10], 10)
      })
    }
  }
  if (name == "InChiKeys" ||
      name == "InChiKeys2") {
    test_that(check_column, {
      expect_equal(length(names(data)), 3)
    })
    test_that(check_dim, {
      expect_equal(dim(data), c(as.integer(10), as.integer(3)))
    })
    test_that(check_column_name, {
      expect_equal(names(data),
                   c(
                     as.character('CID'),
                     as.character('MolecularFormula'),
                     as.character('InChIKey')
                   ))
    })
    
    test_that(check_internal_type, {
      expect_equal(typeof(data$CID), "character")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$MolecularFormula), "character")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$InChIKey), "character")
    })
    test_that(check_NA, {
      expect_equal(any(is.na(data$CID)), FALSE)
    })
  }
  if (name == "PubChem_IDs" ||
      name == "PubChem_IDs2") {
    test_that(check_column, {
      expect_equal(length(names(data)), 2)
    })
    test_that(check_dim, {
      expect_equal(dim(data), c(as.integer(10), as.integer(2)))
    })
    test_that(check_column_name, {
      expect_equal(names(data), c(as.character('query'), as.character('cid')))
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$query), "character")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$cid), "character")
    })
  }
  if (name == "results" ||
      name == "results2") {
    test_that(check_column, {
      expect_equal(length(names(data)), 11)
    })
    test_that(check_column_name, {
      expect_equal(
        names(data),
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
        )
      )
    })
    test_that(check_dim, {
      expect_equal(dim(data), c(as.integer(10), as.integer(11)))
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KOA), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KAW), "double")
    })
    test_that(check_NA, {
      expect_equal(any(is.na(data$log_KOA)), FALSE)
    })
    test_that(check_NA, {
      expect_equal(any(is.na(data$log_KAW)), FALSE)
    })
  }
  if (name == "resultsCombined") {
    test_that(check_column, {
      expect_equal(length(names(data)), 11)
    })
    test_that(check_column_name, {
      expect_equal(
        names(data),
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
        )
      )
    })
    test_that(check_dim, {
      expect_equal(dim(data), c(as.integer(20), as.integer(11)))
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$SMILES), "character")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$cid), "character")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$MolecularFormula), "character")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$InChIKey), "character")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$Group), "character")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$CmpdID), "integer")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$Name), "character")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KOC), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KOA), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KOW), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$log_KAW), "double")
    })
    test_that(check_NA, {
      expect_equal(any(is.na(data$log_KOC)), FALSE)
    })
    test_that(check_NA, {
      expect_equal(any(is.na(data$log_KOA)), FALSE)
    })
    test_that(check_NA, {
      expect_equal(any(is.na(data$log_KOW)), FALSE)
    })
    test_that(check_NA, {
      expect_equal(any(is.na(data$log_KAW)), FALSE)
    })
  }
  if (name == "Kplot" ||
      name == "Koc_plot") {
    #print all structures under Kplot/Koc_plot
    test_that(check_column_name, {
      expect_equal(
        names(data),
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
        )
      )
    })
    test_that(check_dim, {
      expect_equal(dim(data$data), c(as.integer(20), as.integer(11)))
    })
    
    test_that(check_internal_type, {
      expect_equal(typeof(data$data$log_KOA), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$data$log_KOW), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$data$log_KAW), "double")
    })
    test_that(check_internal_type, {
      expect_equal(typeof(data$data$log_KOC), "double")
    })
    
    if (name == "Kplot") {
      test_that(check_values, {
        expect_equal(data$labels[1]$x, "log KOA")
      })
    }
    if (name == "Koc_plot") {
      test_that(check_values, {
        expect_equal(data$labels[1]$x, "log KOC")
      })
    }
    test_that(check_values, {
      expect_equal(data$labels[2]$y, "log KAW")
    })
    test_that(check_values, {
      expect_equal(data$labels[3]$colour, "Group")
    })
    test_that(check_values, {
      expect_equal(data$labels[4]$label, "CmpdID")
    })
  }
}