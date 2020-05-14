
check_colHeaders <- function(z, hot) {
  # the generic purpose of this check function:
  generic_error_message <-
    "colHeaders must be a logical vector of length 1 or a character vector of length ncols(data)"

  # make sure we got a vector
  if (!is.vector(z)) {
    stop(generic_error_message)
  }

  # checks for when user provides logical vector
  if (class(z) == 'logical') {
    if (length(z) == 1)
      return(z)
    else
      stop("colHeaders of type logical must be length 1.")
  }

  # checks for when user provides character vector
  if (class(z) == 'character') {
    if (length(z) == length(hot$x$columns))
      return(z)
    else
      stop("colHeaders of type character must have length of ncols(data).")
  }

  # if neither condition is met then provide generic error
  stop(generic_error_message)
}

check_rowHeaders <- function(rowHeaders, hot) {
  # TODO similar implementation to check_colHeaders, infact, we may be able to make it into one function check_headers
  rowheaders
}

check_colWidths <- function(x, length){
  if(is.null(x))
    return(FALSE)
  if(length(x) != length)
    return(FALSE)
  TRUE
}
