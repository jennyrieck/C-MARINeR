# C-Mariner Resources

Want to help the C-MARINeR Team, but aren't sure where to start? Here are some resources to get you going:

**R Code**

* [Hadley Wickham's guide to unit testing](http://r-pkgs.had.co.nz/tests.html)
* [Speeding up code](https://www.r-bloggers.com/strategies-to-speedup-r-code/)

**R Markdown**

* [Hadley Wickham's guide to writing vignettes](http://r-pkgs.had.co.nz/vignettes.html)

**Shiny App**

* [ShinyApp walk-through tutorial by Dean Attali](https://deanattali.com/blog/building-shiny-apps-tutorial/)
* [ShinyApp video tutorials](https://shiny.rstudio.com/tutorial/)
* [ShinyApp Gallery](https://shiny.rstudio.com/gallery/)

**Git & RStudio**

* [Happy Git with R](https://happygitwithr.com/)

**Projects**

* [How to avoid having Jenny Bryan set your computer on fire](https://www.tidyverse.org/articles/2017/12/workflow-vs-script/)
* Find open source freely distributable connectivity data (akin to what is available at http://umcd.humanconnectomeproject.org/) also potentially: https://github.com/faskowit/brain-networks-across-the-web
* See our [issues page](https://github.com/jennyrieck/C-MARINeR/issues) for more projects

**All things STATIS**

* [Abdi et al., STATIS and DISTATIS](https://www.utdallas.edu/~herve/abdi_Wires_AWVB2012_Final.pdf)

**Package Development**

* [devtools cheat sheet](https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf)
* [roxygen (for manuals, etc...)](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html)

**Package dependencies**
* [magrittr](https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html)
* [GSVD](https://github.com/derekbeaton/gsvd)

or

``` r
devtools::install_github("derekbeaton/GSVD")
```

**covSTATIS package**

You'll need to install the package to use it, or, to clone the git repository and open it from the .Rproj file. From the .Rproj file, you can use `devools::load_all()` to load the development version of the package from your directory.

``` r
 devtools::install_github("jennyrieck/C-MARINeR", subdir = "/code/covstatis")
```
