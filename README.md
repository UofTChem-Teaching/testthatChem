
# The Guide for the package "testthatChem"
R package to support automated testing in courses.

## Instruction for the package installation and loading
	install.packages("devtools")
	devtools::install_github("UofTChem-Teaching/testthatChem")
When prompt: "Enter one or more numbers, or an empty line to skip updates:" Please input ##1## for All
### If you have installed an old version of "testthatChem" but want to update to the new version, please run the following lines
	detach("package:testthatChem", unload = TRUE)
	remove.packages("testthatChem")
	devtools::install_github("UofTChem-Teaching/testthatChem")

## Instruction for running tests on .R /.Rmd

### Remember to clear Global environment before running tests
	rm(list = ls(all.names = TRUE))
	gc()

### When tests are running on .R
	
	library(testthat)
	library(testthatChem)
	library(devtools)
### When tests are running on .Rmd
	
	library(testthat)
	library(testthatChem)
	library(devtools)
	library("knitr")
	knit("../xxx.Rmd")
#### * Sometimes, you might see a warning message like: WARNING: Rtools is required to build R packages, but is not currently installed. You can ignore it because you are not required to build/compile a package. 

## Specific instructions for different machines 

### For Windows Machine
You might encounter errors like the following:
1.Error: package or namespace load failed for ‘rlang’:
 .onLoad failed in loadNamespace() for 'rlang', details:
  call: NULL
  error: The rlang package is not properly installed.
The DLL version does not correspond to the package version.
Please update rlang to the latest version.

2.Error: package or namespace load failed for ‘mice’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
namespace ‘rlang’ 0.x.x is already loaded, but >= x.x.x is required

Delete the installed rlang package and force R to install the specific version should fix that:

	install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_1.0.3.tar.gz", repo=NULL, type="source")

### For Mac OS
No issues raised

### For Cloud
No issues raised
