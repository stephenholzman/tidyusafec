#' Make a request to the OpenFEC API
#'
#' Constructs a full URL and sends a GET request to the OpenFEC API. Returns the JSON response as a list if successful. OpenFEC ends points and parameters are documented at https://api.open.fec.gov/developers/.
#' @param path A character string of the OpenFEC endpoint.
#' @param query A list with values of the parameters to use in the request.
#'

get_openfec <- function(path, query = list()) {

  url <- httr::modify_url('https://api.open.fec.gov/', path = paste0("/v1",path), query = query)

  response <- httr::GET(url)

  if (httr::status_code(response) != 200) {
    stop(
      sprintf(
        "OpenFEC request failed. [%s]\n%s\n<%s>",
        status_code(response),
        response$response
      ),
      call. = FALSE
    )
  }

 parsed_response <- jsonlite::fromJSON(httr::content(response, "text", encoding="UTF-8"), simplifyVector = FALSE)

 parsed_response

}
