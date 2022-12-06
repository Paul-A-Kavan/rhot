listener_jsFunction_data <- "function(changes, source){
        // register as global but treat it as a local
        // this allows access to initialization hooks without listening on other tables
        if(this.rootElement.id != '{-{id}-}') return;

        columns = this.getSettings().columns;
        cnames = columns.map(x=>x.data);
        rtypes = columns.map(x=>x.R_type);
        data = [];
        for(c in columns){
          data.push(this.getDataAtProp(cnames[c]));
        }

        var result = {
          'cnames': cnames,
          'rtypes': rtypes,
          'data': data
        }
        Shiny.onInputChange(this.rootElement.id + '_data:rhot.returnDataDeserializer', result);
    }"


#' Turn on predefined hook listeners
#'
#' @export
hot_listeners <- function(hot, data = FALSE, selected = FALSE){
  if(data){
    hot <- hot_add_listener(
      hot,
      hook = "afterChange",
      glue(listener_jsFunction_data, id = hot$elementId, .open = "{-{", .close = "}-}"),
      local_or_global = "global"
    )
  }
  if(selected){
    #TODO
  }

  hot
}

#' Set event listeners on the hot object
#'
#' @param hot
#' @param hook The name of the handsontable hook you're attaching the listener to.
#' @param jsFunction Your javascript function passed as a string.
#'
#' @export
hot_add_listener <- function(hot, hook, jsFunction, local_or_global = 'local') {

  new_index <- 1 + length(hot$x$hooks)
  new_entry <- list(key = hook, callback = jsFunction)

  if(local_or_global == 'local'){
    hot$x$hooksLocal[[new_index]] <- new_entry
  }
  if(local_or_global == 'global'){
    hot$x$hooksGlobal[[new_index]] <- new_entry
  }

  hot
}
