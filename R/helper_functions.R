
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

resolve_column_id <- function(hot, column_id) {
  # prevent the possibility of iterating over the same column twice
  column_id <- unique(column_id)

  if (is.numeric(column_id))
    if (all(column_id %in% seq_along(hot$x$columns)))
      return(column_id)
  cnames <- unlist(get_column_setting(hot, 'data'))
  if (is.character(column_id))
    return(which(cnames %in% column_id))

  # else we give a generic complaint
  stop("column_id must be an integer or character vector indicating columns of the data")


}

get_value <- function(value, position) {
  if (length(value) == 1)
    return(value)
  else
    return(value[position])
}

get_column_setting <- function(hot, setting, col_id = NULL) {
  if (!is.null(col_id))
    return(hot$x$columns[[col_id]][[setting]])

  return(
    unlist(lapply(hot$x$columns,
                  function(a) {
                    a[[setting]]
                  }))
  )
}

# set_column_setting <- function(hot, setting_name, setting_value, columnId, position) {
#   hot$x$columns[[columnId]][setting_name] <- setting_value
#   return(hot)
# }
#
# set_table_setting <- function(hot, setting_name, setting_value) {
#   if (is.null(setting_value))
#     return(hot)
#   hot$x[[setting_name]] <- setting_value
#   return(hot)
# }





set_alignmentClasses <- function(hot, column_id, position, halign, valign) {
    # TODO write this function

    if (is.null(halign) & is.null(valign)) {
      # do nothing
      return(hot)
    }
    # both are present
    if (!is.null(halign) & !is.null(valign)) {
      # set them both
      return(hot)
    }
    # else only one of them is not null so set each one seperately
    if (!is.null(halign)) {
      # set just halign
      return(hot)
    }
    if (!is.null(valign)) {
      # set just valign
      return(hot)
    }
    # technically you shouldn't get to this point but you never know...
    stop(
      "Oops, development error. Please report the set_alignmentClasses function to the developer."
    )
  }




columnSorting <- function(indicator = TRUE,
                          sortEmptyCells = TRUE,
                          headerAction = TRUE,
                          jsCompareFunction = NULL)
  {
  # example function: "function(value, nextValue) {return 0;}"

  # the columnSorting needs to be a list that turns into a js object
  set_list <- list()
  # add the parameters
  set_list$indicator = indicator
  set_list$sortEmptyCells = sortEmptyCells
  set_list$headerAction = headerAction

  # now, if the user provides a sorting function as a string then we include it
  if ( !is.null(jsCompareFunction) ){
    set_list$compareFunctionFactory = glue::glue(
      "
      function(sortOrder, columnMeta) {
      return {*{jsCompareFunction}*}
      }
      ", .open = "{*{", .close = "}*}"
    )
  }
  return(set_list)
}

































































# set_column_entry <- function(hot,
#                              columnId,
#                              columnName,
#                              columnType,
#                              position) {
#   # we allow null values to be passed to simplify the primary code base
#   if (is.null(columnType))
#     return(hot)
#   # accomodate vectors of values by using getValue
#   columnType = get_value(columnType, position)
#
#   hot$x$columns[[columnId]] =
#     switch(
#       columnType,
#       'numeric'   = list(
#         data = columnName,
#         returnType = 'numeric',
#         title = columnName,
#         type = 'numeric',
#         numericFormat = list(pattern = '0,000.00',
#                              culture = 'en-US')
#       ),
#       'integer'   = list(
#         data = columnName,
#         returnType = 'integer',
#         title = columnName,
#         type = 'numeric',
#         numericFormat = list(pattern = '0,000',
#                              culture = 'en-US')
#       ),
#       'character' = list(
#         data = columnName,
#         returnType = 'character',
#         title = columnName,
#         type = 'autocomplete',
#         #source = , autocomplete can include source as vector of options
#         strict = 'false',
#         filter = 'false'
#       ),
#       'factor'    = list(
#         data = columnName,
#         returnType = 'factor',
#         title = columnName,
#         type = 'dropdown',
#         strict = 'true',
#         filter = 'true',
#         source = levels(hot$x$r$data[[columnId]])
#       ),
#       'logical'   = list(
#         data = columnName,
#         returnType = 'logical',
#         type = 'text'),
#       'Date'      = list(
#         data = columnName,
#         returnType = 'Date',
#         title = columnName,
#         type = 'date',
#         dateFormat = 'MM/DD/YYYY',
#         correctFormat = 'true'
#       ),
#       'POSIXct'   = list(
#         data = columnName,
#         returnType = 'POSIXct',
#         title = columnName,
#         type = 'time',
#         timeFormat = 'h:mm:ss a',
#         correctFormat = 'true'
#       ),
#       'checkbox'  = list(
#         data = columnName,
#         returnType = 'logical',
#         title = columnName,
#         type = 'checkbox'
#       ),
#       'text'   = list(
#         data = columnName,
#         returnType = 'character',
#         title = columnName,
#         type = 'text'
#       ),
#       # this is the unnamed default
#       list(
#         data = columnName,
#         returnType = 'character',
#         title = columnName,
#         type = 'text'
#       )
#
#     )
#   return(hot)
# }
