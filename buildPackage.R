## Building the package
## ---------------------------------------------------------------------

library(roxygen2)

## Remove the folder if it exists
if(file.exists("./faoswsTrade"))
    unlink("faoswsTrade", recursive = TRUE)

## Build the package
package.skeleton("faoswsTrade", code_files = paste("./codes/",
                           dir("./codes/", pattern = "\\.R$"), sep = ""),
                 force = FALSE)

## Include the DESCRIPTION file
file.copy(from = "./DESCRIPTION", to = "faoswsTrade/",
          overwrite = TRUE)
unlink("./faoswsTrade/Read\\-and\\-delete\\-me")

## Use roxygen to build the documentation
roxygenize("faoswsTrade")
## unlink("./faoswsTrade/inst/", recursive = TRUE)

## Build and check the package
system("R CMD INSTALL --build faoswsTrade")
system("R CMD build faoswsTrade")
## system("R CMD check --as-cran faoswsTrade")

