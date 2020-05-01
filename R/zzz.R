.onLoad <- function(...) {

  tryCatch( {
    shiny::registerInputHandler("rhandsontable.customSelectDeserializer",
                                function(x, session, inputName) {
                                  result <- x
                                  result$select$rAll <- unlist(x$select$rAll)
                                  result$select$cAll <- unlist(x$select$cAll)
                                  result},
                                force = TRUE )
  }, error = function(err) {})






  # shiny::registerInputHandler("rhotDeserializer_edit",
  #                             function(x, session, inputName) {
  #                               return(x)
  #                             })

}
