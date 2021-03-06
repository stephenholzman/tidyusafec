#' Get Election Summary
#'
#' @param data A dataframe or tibble. Usually this will be the returned result of search_elections(). If columns called cycle, office, district, and state are in the table, . Either this argument or candidate_id is required.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param district Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
#' @param cycle Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the election_full flag.
#' @param office Federal office candidate runs for: H, S or P
#' @param election_full Aggregate values over full election period
#' @param state US state or territory where a candidate runs for office
#'
#' @return
#' @export
#'
#' @examples
#'
get_election_summary <- function(
  data = NULL,
  data_structure = "tidy",
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  district = NULL,
  cycle = NULL,
  office = NULL,
  election_full = NULL,
  state = NULL
){

  if (is.null(api_key)) {

    stop('An API key is required. Obtain one at https://api.data.gov/signup. If you have one, use data_gov_api_key() to save it.')

  }


  ### Define function for just getting one summary. get_election_summary will do this for every row of the data argument if it exists and contains columns for cycle, district, state, and office. Otherwise, it will just return one election on the condition all required arguments are provided and are of length 1.

  get_one_election_summary <- function(district = NULL,
                                       cycle = NULL,
                                       office = NULL,
                                       state = NULL){

    query_parameters <- list(
      api_key = api_key,
      district = district,
      cycle = cycle,
      office = office,
      election_full = election_full,
      state = state
    )

    response <- get_openfec(path = paste0("/elections/summary/"), query_parameters = query_parameters)

    return(response)

  }


  if (!is.null(data)){

    req_var_check <- unique(names(data)) %>%
      str_detect("cycle|district|state|office") %>%
      sum()

    if (req_var_check == 4){

      new_data <- data %>%
        as.tibble() %>%
        mutate(
          office = case_when(
            office == "H" ~ "house",
            office == "S" ~ "senate",
            office == "P" ~ "president"
            )
        ) %>%
        rowwise() %>%
        mutate(summary = list(get_one_election_summary(
                 district = district,
                 cycle = cycle,
                 office = office,
                 state = state)),
               receipts = ifelse(!is.null(summary$parsed$receipts), summary$parsed$receipts, 0),
               count = ifelse(!is.null(summary$parsed$count), summary$parsed$count, 0),
               independent_expenditures = ifelse(!is.null(summary$parsed$independent_expenditures), summary$parsed$independent_expenditures, 0),
               disbursements = ifelse(!is.null(summary$parsed$disbursements), summary$parsed$disbursements, 0)) %>%
        gather(key = "type_of_funds", value = "amount", receipts, disbursements, independent_expenditures) %>%
        select(-., -summary)

      return(new_data)

    }else{

      print("fail")

    }

  }else{

    response <- get_one_election_summary(
      district = district,
      cycle = cycle,
      office = office,
      state = state
      )

    tidy_election_summary <- response$parsed %>%
      as.tibble() %>%
      mutate(state = state,
             cycle = cycle,
             district = district,
             office = office) %>%
      gather(key = "type_of_funds", value = "amount", disbursements, independent_expenditures, receipts)

    object_to_return <- list(
      tidy = tidy_election_summary,
      raw_responses = response
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

}
