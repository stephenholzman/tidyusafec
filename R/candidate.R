#' Search FEC records for Candidates
#'
#' tidycensus Documentation: Search for candidates. If tidy is TRUE, each row describes a candidate during a specific election cycle. The same candidate may have rows for multiple elections depending on search paramters.
#'
#' OpenFEC Documentation: Fetch basic information about candidates and their principal committees.
#'
#' Each result reflects a unique FEC candidate ID. That ID is assigned to the candidate for a particular office sought. If a candidate runs for the same office over time, that ID stays the same. If the same person runs for multiple offices — for example, a House candidate runs for a Senate office — that candidate will get a unique ID for each office.
#'
#' The candidate endpoints primarily use data from FEC registration Form 1, for candidate information, and Form 2, for committees information, with additional information to provide context.
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null.
#' @param cycle Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the election_full flag.
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param designation The one-letter designation code of the organization: - A: authorized by a candidate - J: joint fundraising committee - P: principal campaign committee of a candidate - U: unauthorized - B: lobbyist/registrant PAC - D: leadership PAC
#' @param type The one-letter type code of the organization:  - C: communication cost - D: delegate - E: electioneering communication - H: House - I: independent expenditor (person or group) - N: PAC - nonqualified - O: independent expenditure-only (super PACs) - P: presidential - Q: PAC - qualified - S: Senate - U: single candidate independent expenditure - V: PAC with non-contribution account, nonqualified - W: PAC with non-contribution account, qualified - X: party, nonqualified - Y: party, qualified - Z: national party non-federal account
#' @param full_election Get totals for full election period. Boolean.
#' @param candidate_id A character string of the OpenFEC endpoint.
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.

get_candidate_totals <- function(data_structure = 'both', candidate_id = NULL,
                              sort_null_only = NULL, cycle = NULL, sort = NULL, designation = NULL, type = NULL,
                              full_election = NULL, api_key = NULL){

  if (is.null(api_key)) {

    stop('An API key is required. Obtain one at https://api.data.gov/signup.')

  }

  query_parameters <- list(
    sort_null_only = sort_null_only, cycle = cycle, sort = sort, designation = designation, type = type,
    full_election = full_election, api_key = api_key
  )

  query_parameters <- query_parameters[!sapply(query_parameters, is.null)]

  responses <- list()

  responses[[1]] <- get_openfec(path = paste0("/candidate/",candidate_id, "/totals/"), query = query_parameters)

  total_pages <- responses[[1]][["pagination"]][["pages"]]

  total_count <- responses[[1]][["pagination"]][["count"]]

  if(total_count > 100){

  }

  if(total_pages > 1){

    for(i in 2:total_pages){

      query_parameters$page <- i

      responses[[i]] <- get_openfec(path = "/candidates/search/", query = query_parameters)
      print(i)

    }

  }

  # tidy_candidates <- purrr::map(responses, function(x) x$results) %>%
  #   unlist(recursive = F) %>%
  #   tibble(
  #     load_date = map_chr(. , "load_date", .default = NA),
  #     last_file_date = map_chr(. , "last_file_date", .default = NA),
  #     candidate_id = map_chr(. , "candidate_id", .default = NA),
  #     election_years = map(. , "election_years", .default = NA),
  #     party = map_chr(. , "party", .default = NA),
  #     candidate_status = map_chr(. , "candidate_status", .default = NA),
  #     office_full = map_chr(. , "office_full", .default = NA),
  #     state = map_chr(. , "state", .default = NA),
  #     federal_funds_flag = map_lgl(. , "federal_funds_flag", .default = NA),
  #     has_raised_funds = map_lgl(. , "has_raised_funds", .default = NA),
  #     office = map_chr(. , "office", .default = NA),
  #     election_districts = map(. , "election_districts", .default = NA),
  #     name = map_chr(., "name", .default = NA),
  #     principal_committees = map(. , "principal_committees"),
  #     district_number = map_int(. , "district_number", .default = NA),
  #     cycles = map(. , "cycles", .default = NA),
  #     incumbent_challenge = map_chr(. , "incumbent_challenge", .default = NA),
  #     party_full = map_chr(. , "party_full", .default = NA),
  #     first_file_date = map_chr(. , "first_file_date", .default = NA),
  #     active_through = map_int(. , "active_through", .default = NA),
  #     incumbent_challenge_full = map_chr(. , "incumbent_challenge_full", .default = NA),
  #     last_f2_date = map_chr(. , "last_f2_date", .default = NA),
  #     district = map_chr(. , "district", .default = NA)
  #   ) %>%
  #   tidyr::replace_na(list(principal_committees = list(list(list(organization_type_full = NA, designation = NA, state = NA,
  #                                                                cycles = NA, party_full = NA, committee_type_full = NA,
  #                                                                organization_type = NA, committee_id = NA, first_file_date = NA,
  #                                                                party = NA, committee_type = NA, last_file_date = NA, candidate_ids = NA,
  #                                                                designation_full = NA, last_f1_date = NA, treasurer_name = NA, name = NA))))) %>%
  #   tidyr::unnest(principal_committees, .preserve = c("election_years", "cycles", "election_districts")) %>%
  #   mutate(committee_id = map(principal_committees, `[`, "committee_id") %>% unlist() %>% as.vector(),
  #          committee_name = map(principal_committees, `[`, "name") %>% unlist() %>% as.vector())
  #
  object_to_return <- list(
    #tidy = tidy_candidates,
    raw_responses = responses
  )

  if(data_structure == 'tidy'){
    return(object_to_return$tidy)
  }else if(data_structure == 'list'){
    return(object_to_return$raw_responses)
  }else if(data_structure == 'both'){
    return(object_to_return)
  }else{
    return(object_to_return)
    warning("data_structure not specified, returned both tidy and raw list.")
  }

}
