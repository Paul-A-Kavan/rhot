.onLoad <- function(...) {

  tryCatch( {
    shiny::registerInputHandler("rhandsontable.customSelectDeserializer",
                                function(x, session, inputName) {
                                  result <- x
                                  result$select$rAll <- unlist(x$select$rAll)
                                  result$select$cAll <- unlist(x$select$cAll)
                                  result},
                                force = TRUE )
    shiny::registerInputHandler("rhot_deserializer1",
                                function(x, session, inputName) {
                                  temp <- print(x$changes[[1]])
                                  result <- list(
                                    'row' =       as.numeric(temp[[1]]) + 1,
                                    'column' =    temp[[2]],
                                    'old_value' = temp[[3]],
                                    'new_value' = temp[[4]]
                                  )
                                  result},
                                force = TRUE )
  }, error = function(err) {})






  # shiny::registerInputHandler("rhotDeserializer_edit",
  #                             function(x, session, inputName) {
  #                               return(x)
  #                             })

}
