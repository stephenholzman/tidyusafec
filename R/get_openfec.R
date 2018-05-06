#' Make a request to the OpenFEC API
#'
#' Constructs a full URL and sends a GET request to the OpenFEC API. Returns the JSON response as a list if successful. OpenFEC ends points and parameters are documented at https://api.open.fec.gov/developers/.
#' @param path A character string of the OpenFEC endpoint.
#' @param query_parameters A list with values of the parameters to use in the request.
#'

get_openfec <- function(path, query_parameters = list()) {

  #Wrangle the list to turn queries with multiple arguments in a vector into a compatible list with multiple items in the list with the same name.
  query_parameters <- query_parameters %>% unlist()

  names(query_parameters) <- str_remove_all(names(query_parameters), pattern = '[0123456789]')

  query_parameters <- as.list(query_parameters)

  #Set user agent
  useragent <- httr::user_agent("https://github.com/stephenholzman/tidyfec")

  url <- httr::modify_url('https://api.open.fec.gov/', path = paste0("/v1",path), query = query_parameters)

  response <- httr::GET(url, useragent)

  parsed_response <- jsonlite::fromJSON(httr::content(response, "text", encoding="UTF-8"), simplifyVector = FALSE)

  if (httr::status_code(response) != 200) {
    stop(
      if(!is.null(parsed_response$message)){
        sprintf(
          "OpenFEC API request failed. [%s]\n%s\n<%s>",
          status_code(response),
          paste0("OpenFEC message: ", parsed_response$message),
          response$url
        )
      }else{
        sprintf(
          "OpenFEC API request failed. [%s]\n%s\n%s\n<%s>",
          status_code(response),
          paste0("OpenFEC code: ", parsed_response$error$code),
          paste0("OpenFEC message: ", parsed_response$error$message),
          response$url
        )
      }
,
      call. = FALSE
    )
  }

  all <- list(
    raw = response,
    parsed = parsed_response
  )

 all

}

