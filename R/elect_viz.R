
# IDEA: Get all mean percentages from each kommun
#' An RC class that manages all the data - - -
#'@export elect_viz
#'@export
elect_viz <- setRefClass(
  "elect_viz",
  fields = list(
    muniplicity = "character",
    muni_list = "character",
    data = "data.frame",
    mean_p_vals = "vector"
  ),
  methods = list(
    initialize = function(data) {
      # Gets the list of possible muniplicities, removes unused indices
      data <<- data[-1,] # To remove NA's we omit row 1
      muni_list <<- unique(data$X__4[c(-1,-2)])
      muniplicity <<- muni_list[1]
    },
    get_muniplicites = function() {
      return(muni_list)
    },
    set_muniplicity = function(val) {
      muniplicity <<- val
    },
    get_muniplicity = function() {
      "Returns the current muniplicity"
      return(muniplicity)
    },
    get_mean_p_vals = function() {
      "Returns the mean p-values for the currently selected muniplicity"
      options(warn=-1) # To suppress warnings
      mean_p_vals <<- lapply(seq(9,27,by=2), function(x) {
        val <-
          as.numeric(
            data[[paste("X__", as.character(x), sep="")]]
          )
        mean(val[data$X__4 == muniplicity])
      })

      names(mean_p_vals) <<- unlist(lapply(seq(9,27,by=2), function(x) {
        namestr <- strsplit(sheet[[paste("X__", as.character(x), sep="")]][2], " ")
        namestr[[1]][1]
      }))

      mean_p_vals <<- unlist(mean_p_vals)
      options(warn=0)
      return(mean_p_vals)
    }
  )
)

X <- elect_viz$new(data = sheet)
barplot(X$get_mean_p_vals(), main=X$get_muniplicity())
X$get_mean_p_vals()
X$set_muniplicity("Norrbottens län")
barplot(X$get_mean_p_vals(), main=X$get_muniplicity())
X$get_mean_p_vals()
X$set_muniplicity("Uppsala län")
barplot(X$get_mean_p_vals(), main=X$get_muniplicity())
X$get_mean_p_vals()

