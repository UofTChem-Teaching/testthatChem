library("knitr")
knit("../demo2Tutorial1_keys.Rmd")
#source("../demo2Tutorial1_keys.Rmd", chdir = TRUE)
library(testthat)

#run: testthat::test_dir('test')

#rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc()


## If you want to access Rmd function

# test_that("positive integers can be added", {
#   expect_equal(sum(5, 5), 10)
#   expect_equal(sum(8, 2), 10)
#   expect_equal(sum(1, 1), 2)
# })
# 
# test_that("negative integers can be added", {
#   expect_equal(sum(15, -5), 10)
#   expect_equal(sum(-8, -6), -14)
#   expect_equal(sum(-10, 20), 10)
# })


my_data_list <- list("compounds"=compounds,"compounds2"=compounds2,"compTox"=compTox,"compTox2"=compTox2, "InChiKeys"=InChiKeys,"InChiKeys2"=InChiKeys2,"PubChem_IDs"=PubChem_IDs,"PubChem_IDs2"=PubChem_IDs2, "results"=results, "results2"=results2, "resultsCombined" = resultsCombined, "Koc_plot"=Koc_plot,"Kplot"=Kplot)
for (i in seq_along(my_data_list)){
  check_column <- paste("check if", names(my_data_list)[i], "contains columne names and has correct column size")
  check_column_name <- paste("check if", names(my_data_list)[i], "contains correct column names")
  check_dim <- paste("check if", names(my_data_list)[i], "contains columne names and has correct column size")
  check_internal_type <- paste("check if cells inside tibble", names(my_data_list)[i], "contains correct data objects/types")
  check_NA <- paste("check if cells inside tibble", names(my_data_list)[i], "contains NA invalid rows")
  check_values <- paste("check if cells/plots ", names(my_data_list)[i], "are imported correctly and have the correct values")
  #print(checks) or print(paste(i,names(my_data_list)[i],my_data_list[[i]])) if we want all prints
  if(names(my_data_list)[i] == "compounds" || names(my_data_list)[i] == "compounds2"){
    test_that(check_column,{expect_equal(length(names(my_data_list[[i]])),1)})
    test_that(check_column_name,{expect_equal(names(my_data_list[[i]]),c(as.character('SMILES')))})
    test_that(check_dim,{expect_equal(dim(my_data_list[[i]]),c(as.integer(10),as.integer(1)))})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$SMILES),"character")})
    if(names(my_data_list)[i] == "compounds"){ test_that(check_values,{expect_equal(my_data_list[[i]]$SMILES[10],"FC(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(=O)O")})  }
    if(names(my_data_list)[i] == "compounds2"){ test_that(check_values,{expect_equal(my_data_list[[i]]$SMILES[10],"FC(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)CO")})  }
  }
  
  if(names(my_data_list)[i] == "compTox"|| names(my_data_list)[i] =="compTox2"){
    test_that(check_column,{expect_equal(length(names(my_data_list[[i]])),8)})
    test_that(check_column_name,{expect_equal(names(my_data_list[[i]]),c(as.character('Key'),as.character('Group'),as.character('CmpdID'),as.character('Name'),as.character('log_KOA'),as.character('log_KOW'),as.character('log_KAW'),as.character('log_KOC')))})
    test_that(check_dim,{expect_equal(dim(my_data_list[[i]]),c(as.integer(10),as.integer(8)))})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KOA),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KOW),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KAW),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KOC),"double")})
    if(names(my_data_list)[i] == "compTox"){ test_that(check_values,{expect_equal(my_data_list[[i]]$Group[10],"Group1")})  
      test_that(check_values,{expect_equal(my_data_list[[i]]$CmpdID[10],10)})}
    if(names(my_data_list)[i] == "compTox2"){ test_that(check_values,{expect_equal(my_data_list[[i]]$Group[10],"Group2")})  
      test_that(check_values,{expect_equal(my_data_list[[i]]$CmpdID[10],10)})}
  }
  if(names(my_data_list)[i] == "InChiKeys" || names(my_data_list)[i] == "InChiKeys2"){
    test_that(check_column,{expect_equal(length(names(my_data_list[[i]])),3)})
    test_that(check_dim,{expect_equal(dim(my_data_list[[i]]),c(as.integer(10),as.integer(3)))})
    test_that(check_column_name,{expect_equal(names(my_data_list[[i]]),c(as.character('CID'),as.character('MolecularFormula'),as.character('InChIKey')))})
    
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$CID),"character")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$MolecularFormula),"character")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$InChIKey),"character")})
    test_that(check_NA,{expect_equal(any(is.na(my_data_list[[i]]$CID)),FALSE)})
  }
  if(names(my_data_list)[i] == "PubChem_IDs" || names(my_data_list)[i] == "PubChem_IDs2" ){
    test_that(check_column,{expect_equal(length(names(my_data_list[[i]])),2)})
    test_that(check_dim,{expect_equal(dim(my_data_list[[i]]),c(as.integer(10),as.integer(2)))})
    test_that(check_column_name,{expect_equal(names(my_data_list[[i]]),c(as.character('query'),as.character('cid')))})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$query),"character")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$cid),"character")})
  }
  if(names(my_data_list)[i] == "results" || names(my_data_list)[i] == "results2" ){
    test_that(check_column,{expect_equal(length(names(my_data_list[[i]])),11)})
    test_that(check_column_name,{expect_equal(names(my_data_list[[i]]),c(as.character('SMILES'),as.character('cid'),as.character('MolecularFormula'),as.character('InChIKey'),as.character('Group'),as.character('CmpdID'),as.character('Name'),as.character('log_KOA'),as.character('log_KOW'),as.character('log_KAW'),as.character('log_KOC')))})
    test_that(check_dim,{expect_equal(dim(my_data_list[[i]]),c(as.integer(10),as.integer(11)))})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KOA),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KAW),"double")})
    test_that(check_NA,{expect_equal(any(is.na(my_data_list[[i]]$log_KOA)),FALSE)})
    test_that(check_NA,{expect_equal(any(is.na(my_data_list[[i]]$log_KAW)),FALSE)})
  }
  if(names(my_data_list)[i] == "resultsCombined"){
    test_that(check_column,{expect_equal(length(names(my_data_list[[i]])),11)})
    test_that(check_column_name,{expect_equal(names(my_data_list[[i]]),c(as.character('SMILES'),as.character('cid'),as.character('MolecularFormula'),as.character('InChIKey'),as.character('Group'),as.character('CmpdID'),as.character('Name'),as.character('log_KOA'),as.character('log_KOW'),as.character('log_KAW'),as.character('log_KOC')))})
    test_that(check_dim,{expect_equal(dim(my_data_list[[i]]),c(as.integer(20),as.integer(11)))})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$SMILES),"character")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$cid),"character")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$MolecularFormula),"character")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$InChIKey),"character")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$Group),"character")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$CmpdID),"integer")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$Name),"character")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KOC),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KOA),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KOW),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$log_KAW),"double")})
    test_that(check_NA,{expect_equal(any(is.na(my_data_list[[i]]$log_KOC)),FALSE)})
    test_that(check_NA,{expect_equal(any(is.na(my_data_list[[i]]$log_KOA)),FALSE)})
    test_that(check_NA,{expect_equal(any(is.na(my_data_list[[i]]$log_KOW)),FALSE)})
    test_that(check_NA,{expect_equal(any(is.na(my_data_list[[i]]$log_KAW)),FALSE)})
  }
  if(names(my_data_list)[i] == "Kplot"|| names(my_data_list)[i] == "Koc_plot"){
    #print all structures under Kplot/Koc_plot
    test_that(check_column_name,{expect_equal(names(my_data_list[[i]]),c(as.character('data'),as.character('layers'),as.character('scales'),as.character('mapping'),as.character('theme'),as.character('coordinates'),as.character('facet'),as.character('plot_env'),as.character('labels') ))})
    test_that(check_dim,{expect_equal(dim(my_data_list[[i]]$data),c(as.integer(20),as.integer(11)))})
    
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$data$log_KOA),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$data$log_KOW),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$data$log_KAW),"double")})
    test_that(check_internal_type,{expect_equal(typeof(my_data_list[[i]]$data$log_KOC),"double")})
    
    
    if(names(my_data_list)[i] == "Kplot"){test_that(check_values,{expect_equal(my_data_list[[i]]$labels[1]$x,"log KOA")})}
    if(names(my_data_list)[i] == "Koc_plot"){test_that(check_values,{expect_equal(my_data_list[[i]]$labels[1]$x,"log KOC")})}
    test_that(check_values,{expect_equal(my_data_list[[i]]$labels[2]$y,"log KAW")})
    test_that(check_values,{expect_equal(my_data_list[[i]]$labels[3]$colour,"Group")})
    test_that(check_values,{expect_equal(my_data_list[[i]]$labels[4]$label,"CmpdID")})
  }
}
