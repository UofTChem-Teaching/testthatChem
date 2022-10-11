# ChemTest routine and autoGrading for 210 and 310
#
#Import distributed packages from Github directly
#
#install.packages("devtools")
#devtools::install_github("UofTChem-Teaching/testthat_chem")
#library(testthat_chem)
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @export

#check column names and check if data contains the correct numble of columns
test_column_names <- function(data, expected, name = "the data") {
  description <-
    paste("check if", name, "contains the correct column names")
  test_that(description, {
    expect_named(data, expected)
  })
}

#check lengths of a value, note that "data" and "values" are two different environment types
test_value_lengths <- function(data, expected, name = "the data") {
  description <-
    paste("check if", name, "has the correct length")
  test_that(description, {
    expect_equal(length(data), !!expected)
  })
}

#check dimensions of dataa
test_dimensions <- function(data, expected, name = "the data") {
  description <-
    paste("check if", name, "has the correct number of rows and columns")
  test_that(description, {
    expect_equal(dim(data), !!expected)
  })
}

#check types, could be integer/double/character/raw/numeric/logical/complex
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

#check the value of a specific calculation. e.g., expect_equal(max(rollsummer, na.rm = TRUE), 50)
test_value_cal <-
  function(data, expected, name = "the data") {
    description <-
      paste("check if",
            name,
            "has the correct value")
    test_that(description, {
      expect_equal(data,!!expected)
    })
  }

#check the entry of a tibble
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
#check if NA exist
test_NA <- function(data, col_name, expected, name = "the data") {
  description <-
    paste("check if cells inside", name, "contains NA invalid rows")
  test_that(description, {
    expect_equal(any(is.na(data[[col_name]])),!!expected)
  })
}


############################
#check data stored in plots#
############################
test_dimensions_plot <-
  function(data, col_name, expected, name = "the data") {
    description <-
      paste("check if",
            name,
            "- column",
            col_name,
            "has the correct number of rows and columns")
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

#check if a plot is labeled
test_exist_label_plot <-
  function(data,
           table_name,
           col_name,
           expected,
           name = "the data") {
    description <-
      paste("check if",
            name,
            "has a x<-1/y<-2 || x<-2/y<-1 label",
            col_name,
            "with type",
            expected)
    test_that(description, {
      expect_type(data[[table_name]][col_name], expected)
    })
  }

#check the value of a label entry in a plot
test_entry_label_plot <-
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



#check data entry in a plot. row_index1 points to the data_name
#e.g.,   wplot$data[7]$NHex[1922] = wplot[[data]][7][[NHex]][1992]
test_entry_data_plot <-
  function(data,
           col_name,
           row_index,
           data_name,
           entry_index,
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
        data_name,
        ",entry number:",
        entry_index
      )
    test_that(description, {
      expect_equal(data[[col_name]][row_index][[data_name]][entry_index], !!expected)
    })
  }
