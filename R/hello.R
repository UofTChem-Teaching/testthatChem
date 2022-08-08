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

test_column_names <- function(data, expected, name = "the data") {
  description <-
    paste("check if", name, "contains the correct column names")
  test_that(description, {
    expect_named(data, expected)
  })
}

test_dimensions <- function(data, expected, name = "the data") {
  description <-
    paste("check if", name, "has the correct number of rows and columns")
  test_that(description, {
    expect_equal(dim(data), !!expected)
  })
}

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