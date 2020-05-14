#' Set column-level settings.
#'
#' Pass settings to the handsontable object that are column specific. The function is vectorized over columns and the settings. This means that you can set the same settings for different columns and different settings for different columns in the same call.
#'
#' @param hot The Handsontable object created by rhot().
#' @param columnId Vector to specify which columns for the provided settings. Can be numeric vector of column indexes or character vector of column names.
#' @param returnType Sets the R-class that will be used to deserialize data back into R. By default these are set by the class of the columns in the data.frame provided to rhot().
#' @param allowInvalid
#' @param colHeader Length one character vector to set the column's display name. Doesn't alter underlying dataframe column names.
#' @param colWidth
#' @param copyable
#' @param copyPaste Boolean topermit or remove copy and paste action on a column. The Handsontable library defaults this value to TRUE.
#' @param correctFormat
#' @param dateFormat
#' @param defaultDate
#' @param editor
#' @param filter
#' @param filteringCaseSensitive
#' @param label
#' @param numericFormat
#' @param placeholder
#' @param readOnly
#' @param renderer default options: 'autocomplete', 'checkbox', 'html', 'numeric', 'password', 'text' or custRenderer
#' @param selectOptions
#' @param skipColumnOnPaste
#' @param source
#' @param strict
#' @param title
#' @param trimDropdown
#' @param trimWhitespace
#' @param type Character vector defining the type for each column. This is the preferred way of setting the editor, renderer, and validator (ERV) for a column when using predefined column types. To further customize your table, supply the type then override any or all of the three ERV settings.
#' @param validator Character vector with names of validator to be used for the column. Setting validator explicitly will override the validator that is implied by setting the column type. Handsontable provides 'autocomplete','date','dropdown', 'numeric', 'time' as prebuilt validators. See details for explanation of each. For further customization you can register your own custom validator by using \code{\link{rhot::validator_register_custom}} and then passing that validator's name to this parameter.
#' @param visibleRows
#' @param wordWrap
#' @param ... Additional settings passed to the column object.
#'
#'
#' @details
#' ## Type, Editor, Renderer, Validator
#'
#' ## Validator
#' ### Standard options
#' These are the options provided by Handsontable.
#' autocomplete
#' date
#' dropdown
#' numeric
#' time
#' custom validator: see rhot::validator_register_custom()
#' ### Custom Validator
#'
#'
#'
#'
# this sets settigns that pertain to a single column in the column list
#'
#' @export
hot_column <-
  function(hot,
           columnId, # null not allowed, if you want whole table then use hot_table
           returnType = NULL, # this is used for deserializing
           allowInvalid = NULL,
           colHeader = NULL,
           colWidth = NULL,
           copyable = NULL,
           copyPaste = NULL,
           correctFormat = NULL,
           dateFormat = NULL,
           defaultDate = NULL,
           editor = NULL,
           filter = NULL,
           filteringCaseSensitive = NULL,
           label = NULL,
           numericFormat = NULL,
           placeholder = NULL,
           readOnly = NULL,
           renderer = NULL,
           selectOptions = NULL,
           skipColumnOnPaste = NULL,
           source = NULL,
           strict = NULL,
           title = NULL,
           trimDropdown = NULL,
           trimWhitespace = NULL,
           type = NULL,
           validator = NULL,
           visibleRows = NULL,
           wordWrap = NULL,
           halign = NULL,
           valign = NULL,
           ...)
  {
    # TODO pass varargs to the column object
    # list(...)

    # resolve character column names to their id's
    col_ids <- resolve_column_id(hot, columnId)

    # apply the provided settings to all of the specified columns
    for( i in seq_along(col_ids) ) {

      if(!is.null(returnType))
        hot$x$columns[[col_ids[i]]]['returnType'] <- list(returnType)

      if(!is.null(allowInvalid))
        hot$x$columns[[col_ids[i]]]['allowInvalid'] <- list(allowInvalid)

      if(!is.null(colHeader))
        hot$x$columns[[col_ids[i]]]['colHeaders'] <- list(colHeader)

      if(!is.null(colWidth))
        hot$x$columns[[col_ids[i]]]['colWidths'] <- list(colWidth)

      if(!is.null(copyable))
        hot$x$columns[[col_ids[i]]]['copyable'] <- list(copyable)

      if(!is.null(copyPaste))
        hot$x$columns[[col_ids[i]]]['copyPaste'] <- list(copyPaste)

      if(!is.null(correctFormat))
        hot$x$columns[[col_ids[i]]]['correctFormat'] <- list(correctFormat)

      if(!is.null(dateFormat))
        hot$x$columns[[col_ids[i]]]['dateFormat'] <- list(dateFormat)

      if(!is.null(defaultDate))
        hot$x$columns[[col_ids[i]]]['defaultDate'] <- list(defaultDate)

      if(!is.null(editor))
        hot$x$columns[[col_ids[i]]]['editor'] <- list(editor)

      if(!is.null(filter))
        hot$x$columns[[col_ids[i]]]['filter'] <- list(filter)

      if(!is.null(filteringCaseSensitive))
        hot$x$columns[[col_ids[i]]]['filteringCaseSensitive'] <- list(filteringCaseSensitive)

      if(!is.null(label))
        hot$x$columns[[col_ids[i]]]['label'] <- list(label)

      if(!is.null(numericFormat))
        hot$x$columns[[col_ids[i]]]['numericFormat'] <- list(numericFormat)

      if(!is.null(placeholder))
        hot$x$columns[[col_ids[i]]]['placeholder'] <- list(placeholder)

      if(!is.null(readOnly))
        hot$x$columns[[col_ids[i]]]['readOnly'] <- list(readOnly)

      if(!is.null(renderer))
        hot$x$columns[[col_ids[i]]]['renderer'] <- list(renderer)

      if(!is.null(selectOptions))
        hot$x$columns[[col_ids[i]]]['selectOptions'] <- list(selectOptions)

      if(!is.null(skipColumnOnPaste))
        hot$x$columns[[col_ids[i]]]['skipColumnOnPaste'] <- list(skipColumnOnPaste)

      if(!is.null(source))
        hot$x$columns[[col_ids[i]]]['source'] <- list(source)

      if(!is.null(strict))
        hot$x$columns[[col_ids[i]]]['strict'] <- list(strict)

      if(!is.null(title))
        hot$x$columns[[col_ids[i]]]['title'] <- list(title)

      if(!is.null(trimDropdown))
        hot$x$columns[[col_ids[i]]]['trimDropdown'] <- list(trimDropdown)

      if(!is.null(trimWhitespace))
        hot$x$columns[[col_ids[i]]]['trimWhitespace'] <- list(trimWhitespace)

      if(!is.null(type))
        hot$x$columns[[col_ids[i]]]['type'] <- list(type)

      if(!is.null(validator))
        hot$x$columns[[col_ids[i]]]['validator'] <- list(validator)

      if(!is.null(visibleRows))
        hot$x$columns[[col_ids[i]]]['visibleRows'] <- list(visibleRows)

      if(!is.null(wordWrap))
        hot$x$columns[[col_ids[i]]]['wordWrap'] <- list(wordWrap)

      # if(!is.null(halign))
      #   set_alignments(halign)
      # if(!is.null(valign))
      #   set_alginments(valign)

      lapply(list(...),
             function(set){
               hot$x$columns[[col_ids[i]]][set] <- list(set)
             })
    }
    return(hot)
  }
