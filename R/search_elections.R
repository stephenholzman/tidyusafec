#' Search For Elections
#'
#' List elections by cycle, office, state, and district. Includes information about incumbents.
#'
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param cycle Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the election_full flag.
#' @param office "house", "senate", or "president."
#' @param sort_hide_null Hide null values on sorted column(s).
#' @param zip Zip code
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param district Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
#' @param state US state or territory where a candidate runs for office
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null
#'
#' @return
#' @export
#'
#' @examples
search_elections <- function(
  data_structure = "tidy",
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  cycle = NULL,
  office = NULL,
  sort_hide_null = NULL,
  zip = NULL,
  sort = NULL,
  district = NULL,
  state = NULL,
  sort_null_only = NULL
){

  if (is.null(api_key)) {

    stop('An API key is required. Obtain one at https://api.data.gov/signup. If you have one, use data_gov_api_key() to save it.')

  }

  query_parameters <- list(
    api_key = api_key,
    cycle = cycle,
    office = office,
    sort_hide_null = sort_hide_null,
    zip = zip,
    sort = sort,
    district = district,
    state = state,
    per_page = 100,
    page = 1
  )

  query_parameters <- query_parameters[!sapply(query_parameters, is.null)]


  #Set up for responses to our requests.
  responses <- list()

  responses[[1]] <- get_openfec(path = "/elections/search/", query_parameters = query_parameters)

  total_pages <- responses[[1]][["parsed"]][["pagination"]][["pages"]]

  total_count <- responses[[1]][["parsed"]][["pagination"]][["count"]]

  message(paste0("Elections found: ", total_count))

  #Automate Pagination, only run if necessary
  if(total_pages > 1){

    message(paste0("There are ",total_pages," pages of results to get."))

    for(i in 2:total_pages){

      if(i == 2 | i %% 10 == 0 | i == total_pages){
        message("On page ", i,"/",total_pages)
      }

      ### Rate Limit Controls
      Sys.sleep(.5) #With an upgraded key, max limit is 120 calls per minute.

      #Check the last response
      #tk

      #Update the page in our query and send another request
      query_parameters$page <- i

      responses[[i]] <- get_openfec(path = "/elections/search/", query_parameters = query_parameters)

    }

  }

  tidy_elections <- responses %>%
    purrr::map(function(x) x$parsed$results) %>%
    unlist(recursive = F) %>%
    tibble(
      cycle = map_chr(. , "cycle", .default = NA),
      office = map_chr(. , "office", .default = NA),
      district = map_chr(. , "district", .default = NA),
      incumbent_id = map_chr(. , "incumbent_id", .default = NA),
      state = map_chr(. , "state", .default = NA),
      incumbent_name = map_chr(. , "incumbent_name", .default = NA)
    )

  results_to_return <- list(
    tidy = tidy_elections,
    raw_responses = responses
  )

  if(data_structure == 'tidy'){
    return(results_to_return$tidy)
  }else if(data_structure == 'list'){
    return(results_to_return$raw_responses)
  }else if(data_structure == 'both'){
    return(results_to_return)
  }else{
    return(results_to_return)
    warning("data_structure not specified, returned both tidy and raw list.")
  }

}
