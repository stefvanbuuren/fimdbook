# Hooks for figure chunks
parinit <- function() {
  par(ps = 8,
      mex = 0.5,
      mar = c(5, 5, 1, 1) + 0.1)
}

solo <- function(before, options, envir) {
  if (before) {
    par(mfrow = c(1, 1))
    parinit()
  }
}

duo <- function(before, options, envir) {
  if (before) {
    par(mfrow = c(1, 2))
    parinit()
  }
}

four <- function(before, options, envir) {
  if (before) {
    par(mfrow = c(2, 2))
    parinit()
  }
}

six <- function(before, options, envir) {
  if (before) {
    par(mfrow = c(3, 2))
    parinit()
  }
}

knitr::knit_hooks$set(solo = solo, duo = duo, four = four, six = six)

# switch for selected empty figures
empty_figure <- FALSE

# hook for formulatilde
# https://stackoverflow.com/questions/14710477/pretty-tilde-from-r-chunk-with-knitr

# hook_source <- knit_hooks$get('source')
# knit_hooks$set(source = function(x, options) {
#   txt <- hook_source(x, options)
#   gsub('~', '\\\\formulatilde', txt)
# })
