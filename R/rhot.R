
#' Shiny bindings for rhot
#'
#' Output and render functions for using rhot within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a rhot
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name rhot-shiny
#'
#' @export
rhotOutput <- function(outputId, width = '100%', height = '400px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'rhot', width, height, package = 'rhot')
}

#' @rdname rhot-shiny
#' @export
renderRhot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, rhotOutput, env, quoted = TRUE)
}

#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @param elementId The id given to the widget.
#' @param data The data.frame to fill the table.
#' @param width,height Dimensions for the widget's container.
#' @param overflow This css value is passed on to the widget's container. Its not recommended to change this.
#'
#' @export
rhot <- function(elementId,
                 data,
                 width = 'auto', # TODO? need to make a smart interpreter for these
                 height = 'auto',
                 overflow = 'hidden') {
  # create the list of basic table info that will be passed to rhot.js
  x <- list()
  x$data = jsonlite::toJSON(data, dataframe = 'rows')
  x$columns <- vector("list", length = ncol(data))
  x$cell <- list()
  x$sizeInfo <- list()
  x$rParams <- list() # anything from r that the user wants attached to the hot root element
  x$renderers <- list()
  x$validators <- list()
  x$editors <- list()
  x$hooksLocal <- list()
  x$hooksGlobal <- list() # use these carefully since they will apply to all tables on the web page

  x$sizeInfo$width <- width
  x$sizeInfo$height <- height
  x$sizeInfo$overflow <- overflow

  # create the widget binding
  hot <- htmlwidgets::createWidget(name = 'rhot',
                                   x = x,
                                   width = width,
                                   height = height,
                                   package = 'rhot',
                                   elementId = elementId)

  # make a namespace function so renderers, editors, validators apply to their respective tables only
  hot$ns <- function(str){paste0(elementId, "-", str)}

  # some rhot defaults
  cnames <- colnames(data)
  rtypes <- unname(unlist(lapply(data, class)))
  for(i in 1:ncol(data)){
    # set values that handsontable constructor is expecting
    hot$x$columns[[i]]$data = cnames[i]
    hot$x$columns[[i]]$title = cnames[i]
    hot$x$columns[[i]]$type = 'text'

    # set values that handsontable does not expect. but these are things used by the r htmlwidgets wrapper
    hot$x$columns[[i]]$R_type = rtypes[i]
  }

  # default to no menu
  hot <- hot %>%
    hot_menu(menu = FALSE)

  return(hot)
}





#' Set cell-level settings.
#'
#' @param hot
#' @param rowId
#' @param columnId
#' @param readOnly
#' @param comment Either a character matrix or a character vector. If character matrix then rowId and columnId are disregarded. Comments are passed at the cell-level to the cell object.
#'
# set the settings that only apply to a single cell.
# note that cell settings override table and column settings
#'
#' @export
hot_cell <- function(hot,
                     rowId = NULL,
                     columnId = NULL,
                     readOnly = NULL,
                     comment = NULL) {
  # create the combinations of the rows and columns
  # note that we subtract one from the indexes to align with javascript standard
  combs <- expand.grid('row' = rowId-1, 'col' = columnId-1)

  for( i in 1:nrow(combs) ){
    new_cell <- list('row' = combs$row[i], 'col' = combs$col[i])
    new_cell <- append(new_cell, c('readOnly' = readOnly))
    new_cell <- append(new_cell, c('comment' = comment))

    # add this newly minted cell to the cell list
    # TODO consolidate cell entries to ensure one entry per cell
    hot$x$cell = append(hot$x$cell, list(new_cell))
  }
  hot
}



#' Set table-level settings.
#'
#'
#'
#' @param autoColumnSize
#' @param autoRowSize
#' @param autoWrapCol
#' @param autoWrapRow
#' @param colHeaders Logical, by default colHeaders is set to TRUE and
#' rhot sets the column headers using the colnames of the provided dataframe.
#' To set the column headers you should use the title parameter.
#' To set a single column's header you should use the title parameter of hot_column().
#' @param columnHeaderHeight
#' @param colWidths
#' @param dragToScroll
#' @param enterMoves
#' @param fixedColumnsLeft
#' @param fixedRowsBottom
#' @param fixedRowsTop
#' @param fragmentSelection
#' @param language
#' @param manualColumnFreeze
#' @param manualColumnMove
#' @param manualColumnResize
#' @param manualRowMove
#' @param manualRowResize
#' @param maxCols
#' @param maxRows
#' @param mergeCells
#' @param minCols
#' @param minRows
#' @param observeDOMVisibility
#' @param outsideClickDeselects
#' @param rowHeaders
#' @param rowHeaderWidth
#' @param rowHeights
#' @param search
#' @param selectionMode
#' @param stretchH
#' @param tabMoves
#' @param titles Set all column titles at once. Must be character vector of length one or ncol(data).
#' @param undo
#' @param validate_upon_creation  something to validate the initial data
#' @param ... passed onto handsontable as table-level settings.
#'
#' @export
hot_table <-
  function(hot,
           autoColumnSize = NULL,
           autoRowSize = NULL,
           autoWrapCol = NULL,
           autoWrapRow = NULL,
           colHeaders = TRUE,
           columnHeaderHeight = NULL,
           colWidths = NULL,
           dragToScroll = NULL,
           enterMoves = NULL,
           fixedColumnsLeft = NULL,
           fixedRowsBottom = NULL,
           fixedRowsTop = NULL,
           fragmentSelection = NULL,
           language = NULL,
           manualColumnFreeze = NULL,
           manualColumnMove = NULL,
           manualColumnResize = NULL,
           manualRowMove = NULL,
           manualRowResize = NULL,
           maxCols = NULL,
           maxRows = NULL,
           mergeCells = NULL,
           minCols = NULL,
           minRows = NULL,
           observeDOMVisibility = NULL,
           outsideClickDeselects = NULL,
           rowHeaders = NULL,
           rowHeaderWidth = NULL,
           rowHeights = NULL,
           search = NULL,
           selectionMode = NULL,
           stretchH = 'none',
           tabMoves = NULL,
           titles = NULL,
           undo = NULL,
           validate_upon_creation = NULL, #? something to validate the initial data
           ...)
  {

    # set the varargs in the environment for the function
    list2env(list(...), environment())

    tab_sets <- setdiff(
      # grab the named params that were passed in
      names(sapply(match.call(expand.dots=TRUE), deparse)[-1]),
      # this defines the params that shouldn't be passed directly to the table
      c('hot', 'validate_upon_creation')
    )

    # first, set the table-wide settings that are present
    for (set in tab_sets) {
      hot$x[[set]] <- get(set) #eval(parse(text = set))
    }

    # some settings need to be handled in custom ways
    if (!is.null(validate_upon_creation)) {
      #TODO
    }

    if (!is.null(titles)) {
      hot <- set_all_columns(hot = hot, setting = 'title', value = titles)
    }

    return(hot)
  }


#' Configure table's context menu.
#'
#' A context menu can be configured and added to the table through this function.
#'
#' @param hot
#' @param menu TRUE or FALSE to add a menu to the table. Defaults is TRUE in this function but note that the table itself defaults the context menu to FALSE.
#' @param moreparams
#'
#'
#' @export
hot_menu <-
  function(hot,
           menu = TRUE,
           allowInsertColumn = NULL)
  {
    # TODO build this function out to be user friendly
    hot$x$contextMenu = menu
    hot$x$allowInsertColumn = allowInsertColumn
    return(hot)
  }




#' Set styles for the table.
#'
#' Change the defualt classes that are used for styling the table. Also, add custom borders to the table.
#'
#' @param hot
#'
#'#'
#' @export
hot_style <- function(hot, someClassSettings = NULL) {
  # TODO write this function and identify arguments from documnetation
  # figure out weird custom borders
  # add any css to be handled in rhot.js

  return(hot)
}




#' Register a custom cell renderer
#'
#' @param hot
#' @param name Name your renderer.
#' @param jsFunction Your javascript renderer function passed as a string.
#'
#' TODO Add details for what is expected of a renderer function.
#' @export
hot_register_renderer <- function(hot, name, jsFunction) {
  new_index <- 1 + length(hot$x$renderers)
  new_entry <- list(name = hot$ns(name), renderer = jsFunction)

  hot$x$renderers[[new_index]] <- new_entry

  return(hot)
}

#' Register a custom cell validator
#'
#' @param hot
#' @param name Name your validator.
#' @param jsFunction Your javascript validator function passed as a string.
#'
#' TODO Add details for what is expected of a validator function.
#' @export
hot_register_validator <- function(hot, name, jsFunction) {
  new_index <- 1 + length(hot$x$validators)
  new_entry <- list(name = hot$ns(name), validator = jsFunction)

  hot$x$validators[[new_index]] <- new_entry

  return(hot)
}

#' Register a custom editor with Handsontable
#'
#' @param hot
#' @param name Name your editor
#' @param jsFunction Your javascript editor function passed as a string.
#'
#' TODO Add details for what is expected of a validator function.
#' @export
hot_register_editor <- function(hot, name, jsFunction) {
  new_index <- 1 + length(hot$x$editors)
  new_entry <- list(name = hot$ns(name), editor = jsFunction)

  hot$x$editors[[new_index]] <- new_entry

  return(hot)
}




#' Attach data to the hot object
#'
#' @param hot
#'
#' @export
hot_attach <- function(hot, ...){
  args_list <- list(...)
  # make sure the variable params are all named so that we can access them in the hot object
  if ('' %in% names(args_list)) stop('All arguments must be named.')

  hot$x$params[[names(args_list)]] <- jsonlite::toJSON(args_list)

  hot
}


