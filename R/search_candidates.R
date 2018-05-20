#' Search FEC records for Candidates
#'
#' Search for candidates. If data_structure is the default 'tidy', returns a tibble with each row describing a unique candidate and principal committee pair. Some candidates will have no principal committee (maybe they just got in, didn't do well, or didn't raise much) and others will have multiple principal committees (they formed new committees for new elections).
#'
#' OpenFEC Documentation: Fetch basic information about candidates and their principal committees.
#'
#' Each result reflects a unique FEC candidate ID. That ID is assigned to the candidate for a particular office sought. If a candidate runs for the same office over time, that ID stays the same. If the same person runs for multiple offices — for example, a House candidate runs for a Senate office — that candidate will get a unique ID for each office.
#'
#' The candidate endpoints primarily use data from FEC registration Form 1, for candidate information, and Form 2, for committees information, with additional information to provide context.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param candidate_id A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
#' @param name Name (candidate or committee) to search for.
#' @param max_first_file_date Selects all candidates whose first filing was received by the FEC before this date.
#' @param year See records pertaining to a particular election year. The list of election years is based on a candidate filing a statement of candidacy (F2) for that year.
#' @param party Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param candidate_status One-letter code explaining if the candidate is: 'C' present candidate, 'F' future candidate, 'N' not yet a candidate, 'P' prior candidate
#' @param state Two letter US state or territory abbreviation where a candidate runs for office. Either a character or character vector for selecting multiple states.
#' @param federal_funds_flag A boolean the describes if a presidential candidate has accepted federal funds. The flag will be false for House and Senate candidates.
#' @param has_raised_funds A boolean that describes if a candidate's committee has ever received any receipts for their campaign for this particular office. (Candidates have separate candidate IDs for each office.)
#' @param sort_hide_null Hide null values on sorted column(s). Boolean.
#' @param office Federal office candidate runs for: H, S or P
#' @param election_year Year of election.
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null.
#' @param incumbent_challenge One-letter code ('I', 'C', 'O') explaining if the candidate is an incumbent, a challenger, or if the seat is open.
#' @param cycle Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the election_full flag.
#' @param min_first_file_date Selects all candidates whose first filing was received by the FEC after this date
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param district Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
#'
#' @import purrr dplyr magrittr
#' @return
#' @export
#'
#' @examples
#'
#'
#'
search_candidates <- function(
  data_structure = 'tidy',
  candidate_id = NULL,
  name = NULL,
  max_first_file_date = NULL,
  year = NULL,
  party = NULL,
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  candidate_status = NULL,
  state = NULL,
  federal_funds_flag = NULL,
  has_raised_funds = NULL,
  sort_hide_null = NULL,
  office = NULL,
  election_year = NULL,
  sort_null_only = NULL,
  incumbent_challenge = NULL,
  cycle = NULL,
  min_first_file_date = NULL,
  sort = NULL,
  district = NULL){

  if (is.null(api_key)) {

    stop('An API key is required. Obtain one at https://api.data.gov/signup. If you have one, use data_gov_api_key() to save it.')

  }

  #We need to combine the arguments into a unified list, drop anything that is NULL.
  query_parameters <- list(
    candidate_id = candidate_id,
    name = name,
    max_first_file_date = max_first_file_date,
    page = 1,
    year = year,
    party = party,
    api_key = api_key,
    candidate_status = candidate_status,
    state = state,
    federal_funds_flag = federal_funds_flag,
    has_raised_funds = has_raised_funds,
    per_page = 100,
    sort_hide_null = sort_hide_null,
    office = office,
    election_year = election_year,
    sort_null_only = sort_null_only,
    incumbent_challenge = incumbent_challenge,
    cycle = cycle,
    min_first_file_date = min_first_file_date,
    sort = sort,
    district = district
  )

  query_parameters <- query_parameters[!sapply(query_parameters, is.null)]


  #Set up for responses to our requests.
  responses <- list()

  responses[[1]] <- get_openfec(path = "/candidates/search/", query_parameters = query_parameters)

  total_pages <- responses[[1]][["parsed"]][["pagination"]][["pages"]]

  total_count <- responses[[1]][["parsed"]][["pagination"]][["count"]]

  message(paste0("Candidates found: ", total_count))

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

     responses[[i]] <- get_openfec(path = "/candidates/search/", query_parameters = query_parameters)

    }

  }

  #Tidy wrangling

  tidy_candidates <- purrr::map(responses, function(x) x$parsed$results) %>%
    unlist(recursive = F) %>%
    tibble::tibble(
      load_date = map_chr(. , "load_date", .default = NA),
      last_file_date = map_chr(. , "last_file_date", .default = NA),
      candidate_id = map_chr(. , "candidate_id", .default = NA),
      election_years = map(. , "election_years", .default = NA),
      party = map_chr(. , "party", .default = NA),
      candidate_status = map_chr(. , "candidate_status", .default = NA),
      office_full = map_chr(. , "office_full", .default = NA),
      state = map_chr(. , "state", .default = NA),
      federal_funds_flag = map_lgl(. , "federal_funds_flag", .default = NA),
      has_raised_funds = map_lgl(. , "has_raised_funds", .default = NA),
      office = map_chr(. , "office", .default = NA),
      election_districts = map(. , "election_districts", .default = NA),
      name = map_chr(., "name", .default = NA),
      principal_committees = map(. , "principal_committees"),
      district_number = map_int(. , "district_number", .default = NA),
      cycles = map(. , "cycles", .default = NA),
      incumbent_challenge = map_chr(. , "incumbent_challenge", .default = NA),
      party_full = map_chr(. , "party_full", .default = NA),
      first_file_date = map_chr(. , "first_file_date", .default = NA),
      active_through = map_int(. , "active_through", .default = NA),
      incumbent_challenge_full = map_chr(. , "incumbent_challenge_full", .default = NA),
      last_f2_date = map_chr(. , "last_f2_date", .default = NA),
      district = map_chr(. , "district", .default = NA)
    ) %>%
    #for candidates with no principal committee, we still want a list filled with NA values so other functions don't freak.
    tidyr::replace_na(list(principal_committees = list(list(list(organization_type_full = NA,
                                                                 designation = NA,
                                                                 state = NA,
                                                                 cycles = NA,
                                                                 party_full = NA,
                                                                 committee_type_full = NA,
                                                                 organization_type = NA,
                                                                 committee_id = NA,
                                                                 first_file_date = NA,
                                                                 party = NA,
                                                                 committee_type = NA,
                                                                 last_file_date = NA,
                                                                 candidate_id = NA,
                                                                 designation_full = NA,
                                                                 last_f1_date = NA,
                                                                 treasurer_name = NA,
                                                                 name = NA))))) %>%
    #We want to unnest the principal committees so we have a row for every canidate-committee pair, but keep some other lists preserved (for now at least. Eventually need to check info about committees to narrow these down further).
    tidyr::unnest(principal_committees, .preserve = c("election_years", "cycles", "election_districts")) %>%
    mutate(committee_id = map_chr(principal_committees, function(x) x$committee_id),
           committee_name = map_chr(principal_committees, function(x) x$name),
           treasurer_name = map_chr(principal_committees, function(x) x$treasurer_name),
           earliest_cycle = map_int(principal_committees, function(x) x$cycles %>% unlist() %>% min()),
           latest_cycle = map_int(principal_committees, function(x) x$cycles %>% unlist() %>% max()),
           earliest_election_year = map_int(election_years, function(x) x %>% unlist() %>% min()),
           latest_election_year = map_int(election_years, function(x) x %>% unlist() %>% max())
           )

  message("Total Principal Committees associated with these candidates: ",length(levels(as.factor(tidy_candidates$committee_id))))

  results_to_return <- list(
    tidy = tidy_candidates,
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
