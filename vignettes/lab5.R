## ---- include = FALSE----------------------------------------------------
library(lab5)
path <- "2014_riksdagsval_per_valdistrikt.xls"#read_excel("../data/2014_riksdagsval_per_valdistrikt.xls")

## ---- results = "hide"---------------------------------------------------
viz <- elect_viz$new(path = path)

## ------------------------------------------------------------------------
head(viz$get_counties())

## ------------------------------------------------------------------------
viz$get_county()
viz$set_county("Uppsala län")
viz$get_county()

## ------------------------------------------------------------------------
viz$get_mean_p_vals()

