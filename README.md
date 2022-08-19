# Breast cancer screening over 70

Software used in the manuscrpt "Opting into breast screening over the age of 70 years: seeking evidence to support informed choice", by Savaridas, SL; Gierlinski, M; Quinn, J; Warwick, VR; Evans, AE, [DOI:https://doi.org/10.1016/j.crad.2022.01.057](https://doi.org/10.1016/j.crad.2022.01.057)

## Instructions

Clone the repository to a local disk. Open an R session (we recommend RStudio), make sure you are in the main project directory. First, create a `renv` package environment:

```
install.packages("renv")
renv::restore()
```

This will install all necessady R packages. When this is done, you can run the pipeline to create all figures and data used in the manuscript.

```
targets::tar_make()
```

Finally, you can render the HTML document, reporting all steps of analysis.

```
rmarkdown::render("doc/analysis.Rmd")
```
