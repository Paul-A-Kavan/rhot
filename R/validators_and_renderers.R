



# the validator and renderer function will return the JS wrapped strings and get
# passed into the hot_column call
#### VALIDATORS ####


#' Register a custom validator with Handsontable
#'
#'
#' @export
register_validator_numeric <- function(hot,
                                       name,
                                       max_value = NULL,
                                       min_value = NULL,
                                       inclusive_above = TRUE,
                                       inclusive_below = TRUE,
                                       integers_only = FALSE) {
  #TODO rewrite to allow for if user wants only upper or only lower bound

  # use different operators based on inclusive
  upper <- ifelse(inclusive_above, '<=', '<')
  lower <- ifelse(inclusive_below, '>=', '>')
  integ <- ifelse(integers_only, 'false', 'true') # reverse the boolean
  jsFunction <- glue::glue(
    "function customValidator(query, callback) {
        var var1 = query {*{upper}*} {*{max_value}*};
        var var2 = query {*{lower}*} {*{min_value}*};
        var var3 = {*{integ}*} || (query == Math.floor(query));
        callback( var1 && var2 && var3 );
      }",
      .open = "{*{", .close = "}*}")
  validator_javascript <- glue::glue(
    "(function(Handsontable){
        {*{jsFunction}*}
      Handsontable.validators.registerValidator(
          '{*{name}*}', customValidator);
      })(Handsontable);",
    .open = "{*{", .close = "}*}")

  # register the validator
  hot$x[[name]] <- htmlwidgets::JS(validator_javascript)

  return(hot)
}

#' Register a custom validator with Handsontable
#'
#'
#' @export
register_validator_character <- function(hot,
                                         name) {
  # TODO
  # need to write an actual javascript function
  warning('The character validator is not yet implemented')

  # this is the function that does the actual validating
  jsFunction <- glue::glue(
    "function customValidator(query, callback) {
        var var1 = query {*{upper}*} {*{max_value}*};
        var var2 = query {*{lower}*} {*{min_value}*};
        var var3 = {*{integ}*} || (query == Math.floor(query));
        callback( var1 && var2 && var3 );
      }",
    .open = "{*{", .close = "}*}")
  # this is just some utility code that we plug our jsFunction into
  validator_javascript <- glue::glue(
    "(function(Handsontable){
        {*{jsFunction}*}
      Handsontable.validators.registerValidator(
          '{*{name}*}', customValidator);
      })(Handsontable);",
    .open = "{*{", .close = "}*}")

  # finally, we add this registering cde to the hot object for execution as JS
  hot$x[[name]] <- htmlwidgets::JS(validator_javascript)

  return(hot)
}


#' Register a custom validator with Handsontable
#'
#' This function turns a string of Javascript into a packaged validator that can then be used in your Handsontable. See details for exact usage information.
#'
#' @param hot The Handsontable object to register the validator with.
#' @param name Specify a unique name for the validator, use this name to apply the validator in the rhot::hot_column validator argument.
#' @param jsFunction The custom validator JS function as a string.
#'
#' @return The Handsontable object, hot, with the specified changes applied.
#'
#' @details
#' You can provide a custom validator by passing the JavaScript code as a string. The string will be glued to a template wrapper function for you, then marked as JavaScript and passed to the Handsontable constructor for registration with the library. Here's an example of a custom validator:
#'
#' "function customValidator(query, callback) { \cr
#'   // this example validator always approves the cell's input \cr
#'   // the query object is the input from the table cell \cr
#'   // put your custom javascript and logic in here \cr
#'   // use the callback to tell the cell whether or not the value is acceptable \cr
#'   callback( false ); \cr
#'   // and that's it, everything else is handled by rhot and Handsontable \cr
#' }" \cr
#'
#' @export
register_validator_custom <- function(hot,
                                      name,
                                      jsFunction) {
  # wrap the javascript function in the registration code
  validator_javascript <- glue::glue(
    "
  (function(Handsontable){

    {*{jsFunction}*}

  Handsontable.validators.registerValidator(
      '{*{name}*}', customValidator);
  })(Handsontable);
  ",
    .open = "{*{", .close = "}*}")

  # attach the registrator to the table object to be run upon initialization
  hot$x[[name]] <- htmlwidgets::JS(validator_javascript)

  # and return the table
  return(hot)
}








#### RENDERERS ####

#' Register a custom validator with Handsontable
#'
#'
#' @export
register_renderer_heatmap <- function(hot,
                                      min_value = 0,
                                      max_value = 1,
                                      min_color = "#000000",
                                      max_color = "#ff0000") {
    JS(glue::glue(
      "
      function(hotInstance, td, row, column, prop, value, cellProperties) {
        Handsontable.renderers.BaseRenderer.apply(this, arguments);

        var minColor = {*{min_color}*}
        var maxColor = {*{max_color}*}
        var diffRed = endColor.red - startColor.red;
        var diffGreen = endColor.green - startColor.green;
        var diffBlue = endColor.blue - startColor.blue;
        diffRed = (diffRed * percentFade) + startColor.red;
        diffGreen = (diffGreen * percentFade) + startColor.green;
        diffBlue = (diffBlue * percentFade) + startColor.blue;
      }
    ",
      .open = "{*{", .close = "}*}"
    ))
}

#' Register a custom validator with Handsontable
#'
#'
#' @export
register_renderer_custom <-  function(hot, name, jsFunction) {
  if ( is.null(jsFunction) )
    # if null we default to the base renderer to protect the user
    jsFunction <-
    "function customRenderer(hotInstance, td, row, column, prop, value, cellProperties) {
      // Optionally include `BaseRenderer` which is responsible for adding/removing
      // CSS classes to/from the table cells.
      Handsontable.renderers.BaseRenderer.apply(this, arguments);
      // then put your custom logic for rendering here
      // everything else is handled by rhot and handsontable
    }"


  # make the javascript function to render our cells
  renderer_javascript <- glue::glue(
    "(function(Handsontable){
        {*{jsFunction}*}
    Handsontable.renderers.registerRenderer('{*{name}*}', customRenderer);
  })(Handsontable);
  ",  .open = "{*{", .close = "}*}")

  # finally, register the validator with the table object as javascript
  hot$x[[name]] <- htmlwidgets::JS(renderer_javascript)

  # and return the table
  return(hot)
}

