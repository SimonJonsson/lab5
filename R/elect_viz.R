#' An RC class that manages all the data
#' @field path The input path for the visualization in the format of 2014_riksdagsval_per_*.xls
#' @export elect_viz
#' @export
elect_viz <- setRefClass(
  "elect_viz",
  fields = list(
    county = "character",
    county_list = "character",
    data = "data.frame",
    mean_p_vals = "vector",
    path = "character"
  ),
  methods = list(
    initialize = function(path) {
      "Initializes the data set, sets the available counties and
      then selects the first county in the list of counties as the default one"

      # Gets the list of possible counties, removes unused indices
      url <- paste("www.val.se/val/val2014/statistik/",path, sep="")
      #raw_data <-
      GET(url, write_disk(tdf <- tempfile(fileext = ".xls")))
      data <<- read_excel(tdf)

      data <<- data[-1,] # To remove NA's, we omit row 1
      county_list <<- unique(data$X__4[c(-1,-2)])
      county <<- county_list[1]
    },
    get_counties = function() {
      "Returns a list of all the available muniplicities"
      stopifnot(county_list == unique(data$X__4[c(-1,-2)]))
      return(county_list)
    },
    set_county = function(val) {
      "Sets the currently selected county"
      stopifnot(val %in% county_list)
      county <<- val
    },
    get_county = function() {
      "Returns the current county"
      stopifnot(county %in% county_list)
      return(county)
    },
    get_mean_p_vals = function() {
      "Returns the mean p-values for the currently selected county"
      stopifnot(county %in% county_list)
      options(warn=-1) # To suppress warnings

      # Returns a vector of mean p-values for each the parties
      mean_p_vals <<- lapply(seq(9,27,by=2), function(x) {
        val <-
          as.numeric(
            data[[paste("X__", as.character(x), sep="")]]
          )
        mean(val[data$X__4 == county])
      })

      # Returns the names of the different parties and sets it as name for the p-value vector
      names(mean_p_vals) <<- unlist(lapply(seq(9,27,by=2), function(x) {
        namestr <- strsplit(data[[paste("X__", as.character(x), sep="")]][1], " ")
        namestr[[1]][1]
      }))

      mean_p_vals <<- unlist(mean_p_vals)
      options(warn=0) # Enables warnings again
      return(mean_p_vals)
    }
  )
)
