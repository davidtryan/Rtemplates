## Markdown Resources

## http://rmarkdown.rstudio.com/
## http://blog.rstudio.org/2014/06/18/r-markdown-v2/
## https://support.rstudio.com/hc/en-us/articles/200552086-Using-R-Markdown
## http://shiny.rstudio.com/articles/rmarkdown.html
## https://onlinecourses.science.psu.edu/statprogram/markdown

#TBD
# http://shiny.rstudio.com/articles/rm-cheatsheet.html
# http://cran.r-project.org/web/packages/markdown/index.html

#Interactive
# http://shiny.rstudio.com/articles/interactive-docs.html
# http://rmarkdown.rstudio.com/authoring_shiny.html  
# http://rmarkdown.rstudio.com/authoring_embedded_shiny.html
# http://blog.rstudio.org/category/shiny/
# http://blog.rstudio.org/2014/06/19/interactive-documents-an-incredibly-easy-way-to-use-shiny/
# https://gist.github.com/wch/9744711
# http://stackoverflow.com/questions/24239420/tex-package-not-installing-in-r-version-3-1-0

#####################################################################

if('rmarkdown' %in% rownames(installed.packages()) == FALSE) {
  install.packages('rmarkdown')
}
library('rmarkdown')

if('TeX' %in% rownames(installed.packages()) == FALSE) {
  install.packages('TeX')
}
library('rmarkdown')

#Use Ctrl+Shift+K in RStudio or rmarkdown::render("file.Rmd") otherwise




Header 1
-----------------------------------------------------
  
  This is an R Markdown document. 

Use an asterisk mark to provide emphasis such as *italics* and **bold**.

Create lists with a dash:
  
  - Item 1
- Item 2
- Item 3

You can write `in-line` code with a back-tick.

```
Code block display
with fixed-width font
```

> Blockquotes are offset




R Code Chunks
------------------------------------------
  
  
  With R Markdown, you can insert R code chunks including plots:
  
  ```{r qplot, fig.width=4, fig.height=3, message=FALSE}
#quick summary and plot
library(ggplot2)
summary(cars)
qplot(speed, dist, data=cars) + 
  geom_smooth()
```


More Tools
------------------------------------------
  
  I counted `r 1+1` red trucks on the highway.

The arithmetic mean is equal to $\frac{1}{n} \sum_{i=i}^{n} x_{i}$, or the summation of n numbers divided by n.

