#' Get Financial Totals for Candidates
#'
#' Get financial totals. If data_structure is the default 'tidy', returns a tibble with notable columns 'amount' and 'type_of_funds'. Use tidyfec_filters to more conveniently filter on 'type_of_funds'.
#'
#' OpenFEC Documentation: This endpoint provides information about a committee's Form 3, Form 3X, or Form 3P financial reports, which are aggregated by two-year period. We refer to two-year periods as a 'cycle'. The cycle is named after the even-numbered year and includes the year before it. To see totals from 2013 and 2014, you would use 2014. In odd-numbered years, the current cycle is the next year — for example, in 2015, the current cycle is 2016. For presidential and Senate candidates, multiple two-year cycles exist between elections.
#'
#' @param data A dataframe or tibble. Usually this will be the returned result of search_candidates(). If a column is called 'candidate_id', get_candidate_totals() will return results for all IDs in that column and will attempt to join data to the result by candidate_id. Either this argument or candidate_id is required.
#' @param candidate_id A character vector of candidate ids to get financial totals for. Either this argument or a dataframe containing a column called 'candidate_id' passed in the 'data' argument is required.
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null.
#' @param cycle Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the election_full flag.
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param designation The one-letter designation code of the organization: - A: authorized by a candidate - J: joint fundraising committee - P: principal campaign committee of a candidate - U: unauthorized - B: lobbyist/registrant PAC - D: leadership PAC
#' @param type The one-letter type code of the organization:  - C: communication cost - D: delegate - E: electioneering communication - H: House - I: independent expenditor (person or group) - N: PAC - nonqualified - O: independent expenditure-only (super PACs) - P: presidential - Q: PAC - qualified - S: Senate - U: single candidate independent expenditure - V: PAC with non-contribution account, nonqualified - W: PAC with non-contribution account, qualified - X: party, nonqualified - Y: party, qualified - Z: national party non-federal account
#' @param election_full Get totals for full election period. Boolean.
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param sort_nulls_last Toggle that sorts null values last.
#' @param sort_hide_null Hide null values on sorted column(s).
#' @param message Control message updates as the request progresses.
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
get_candidate_totals <- function(
  data = NULL,
  candidate_id = NULL,
  data_structure = 'tidy',
  sort_nulls_last = NULL,
  sort_null_only = NULL,
  sort_hide_null = NULL,
  cycle = NULL,
  sort = NULL,
  designation = NULL,
  type = NULL,
  election_full = TRUE,
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  message = TRUE
  ){

  if (is.null(api_key)) {

    stop('An API key is required. Obtain one at https://api.data.gov/signup.')

  }

  #If candidate_id is set explicitly, go with that. If not check for a candidate_id column in the data argument. If neither exists, stop the presses because it's required.
  if (is.null(candidate_id)) {

    if('candidate_id' %in% names(data)){

      candidate_id <- data[['candidate_id']]

    }else{

      stop('Argument for candidate_id or a candidate_id column in the data argument is required')

    }

  }

  candidate_id <- candidate_id %>%
    unique()

  responses <- list()

  #Because this endpoint depends on a candidate ID, we have to make requests separately for each one.

  for(c_id in candidate_id){

    Sys.sleep(.5) #With an upgraded key, max limit is 120 calls per minute. May be giving up a little performance for now in favor of being nice to FEC servers.


    if(message == TRUE){
      if(!is.null(data)){

        c_name <- data$name[match(c_id,data$candidate_id)]
        state_message <- data$state[match(c_id,data$candidate_id)]
        office_message <- data$office_full[match(c_id,data$candidate_id)]
        district_message <- data$district[match(c_id,data$candidate_id)]
        party_message <- data$party[match(c_id,data$candidate_id)]

        message("Getting totals for: ", c_name, " (",party_message,") | ", office_message," ",state_message,district_message," | ",c_id)

      }else{

        message("Getting totals for: ",c_id)

      }
    }



    query_parameters <- list(
      sort_null_only = sort_null_only,
      cycle = cycle,
      sort = sort,
      designation = designation,
      type = type,
      election_full = election_full,
      api_key = api_key,
      page = 1,
      per_page = 100,
      sort_nulls_last = sort_nulls_last,
      sort_hide_null = sort_hide_null
    )

    query_parameters <- query_parameters[!sapply(query_parameters, is.null)]

    responses[[c_id]][[1]] <- get_openfec(path = paste0("/candidate/",c_id, "/totals/"), query_parameters = query_parameters)

    #This is almost definitely just a one page query, but just in case.
    total_pages <- responses[[c_id]][[1]][["parsed"]][["pagination"]][["pages"]]

    total_count <- responses[[c_id]][[1]][["parsed"]][["pagination"]][["count"]]

    if(total_pages > 1){

      for(i in 2:total_pages){

        query_parameters$page <- i

        responses[[c_id]][[i]] <- get_openfec(path = paste0("/candidate/",c_id, "/totals/"), query = query_parameters)
        print(i)

      }

    }

  }

  tidy_candidates <- responses %>%
    unlist(recursive = F) %>%
    purrr::map(function(x) x$parsed$results) %>%
    unname() %>%
    unlist(recursive = F) %>%
    tibble(
      loan_repayments = map_dbl(. , "loan_repayments", .default = NA),
      candidate_id = map_chr(. , "candidate_id", .default = NA),
      last_net_operating_expenditures = map_dbl(. , "last_net_operating_expenditures", .default = NA),
      transfers_from_other_authorized_committee = map_dbl(. , "transfers_from_other_authorized_committee", .default = NA),
      receipts = map_dbl(. , "receipts", .default = NA),
      offsets_to_fundraising_expenditures = map_dbl(. , "offsets_to_fundraising_expenditures", .default = NA),
      other_receipts = map_dbl(. , "other_receipts", .default = NA),
      candidate_contribution = map_dbl(. , "candidate_contribution", .default = NA),
      individual_unitemized_contributions =map_dbl(. , "individual_unitemized_contributions", .default = NA),
      operating_expenditures = map_dbl(. , "operating_expenditures", .default = NA),
      individual_contributions = map_dbl(. , "individual_contributions", .default = NA),
      fundraising_disbursements = map_dbl(. , "fundraising_disbursements", .default = NA),
      political_party_committee_contributions = map_dbl(. , "political_party_committee_contributions", .default = NA),
      coverage_start_date = map_chr(. , "coverage_start_date", .default = NA),
      exempt_legal_accounting_disbursement = map_dbl(. , "exempt_legal_accounting_disbursement", .default = NA),
      election_full = map_lgl(. , "election_full", .default = NA),
      loan_repayments_candidate_loans = map_dbl(. , "loan_repayments_candidate_loans", .default = NA),
      offsets_to_legal_accounting = map_dbl(. , "offsets_to_legal_accounting", .default = NA),
      last_debts_owed_to_committee = map_dbl(. , "last_debts_owed_to_committee", .default = NA),
      transfers_to_other_authorized_committee = map_dbl(. , "transfers_to_other_authorized_committee", .default = NA),
      refunded_other_political_committee_contributions = map_dbl(. , "refunded_other_political_committee_contributions", .default = NA),
      last_debts_owed_by_committee = map_dbl(. , "last_debts_owed_to_committee", .default = NA),
      net_contributions = map_dbl(. , "net_contributions", .default = NA),
      refunded_political_party_committee_contributions = map_dbl(. , "refunded_political_party_committee_contributions", .default = NA),
      federal_funds = map_dbl(. , "federal_funds", .default = NA),
      other_disbursements = map_dbl(. , "other_disbursements", .default = NA),
      coverage_end_date = map_chr(. , "coverage_end_date", .default = NA),
      last_net_contributions = map_dbl(. , "last_net_contributions", .default = NA),
      offsets_to_operating_expenditures = map_dbl(. , "offsets_to_operating_expenditures", .default = NA),
      last_report_year = map_int(. , "last_report_year", .default = NA),
      other_political_committee_contributions = map_dbl(. , "other_political_committee_contributions", .default = NA),
      individual_itemized_contributions = map_dbl(. , "individual_itemized_contributions", .default = NA),
      loans = map_dbl(. , "loans", .default = NA),
      cycle = map_int(. , "cycle", .default = NA),
      candidate_election_year = map_int(. , "candidate_election_year", .default = NA),
      last_report_type_full = map_chr(. , "last_report_type_full", .default = NA),
      last_cash_on_hand_end_period = map_dbl(. , "last_cash_on_hand_end_period", .default = NA),
      loan_repayments_other_loans = map_dbl(. , "loan_repayments_other_loans", .default = NA),
      loans_made_by_candidate = map_dbl(. , "loans_made_by_candidate", .default = NA),
      net_operating_expenditures = map_dbl(. , "net_operating_expenditures", .default = NA),
      contributions = map_dbl(. , "contributions", .default = NA),
      contribution_refunds = map_dbl(. , "contribution_refunds", .default = NA),
      total_offsets_to_operating_expenditures = map_dbl(. , "total_offsets_to_operating_expenditures", .default = NA),
      disbursements = map_dbl(. , "disbursements", .default = NA),
      last_beginning_image_number = map_chr(. , "last_beginning_image_number", .default = NA),
      all_other_loans = map_dbl(. , "all_other_loans", .default = NA)
    ) %>%
    gather(key = "type_of_funds", value = "amount",
           loan_repayments,
           last_net_operating_expenditures,
           transfers_from_other_authorized_committee,
           receipts, offsets_to_fundraising_expenditures,
           other_receipts,
           candidate_contribution,
           individual_unitemized_contributions,
           operating_expenditures,
           individual_contributions,
           fundraising_disbursements,
           political_party_committee_contributions,
           exempt_legal_accounting_disbursement,
           loan_repayments_candidate_loans,
           offsets_to_legal_accounting,
           last_debts_owed_to_committee,
           transfers_to_other_authorized_committee,
           refunded_other_political_committee_contributions,
           last_debts_owed_by_committee,
           net_contributions,
           refunded_political_party_committee_contributions,
           federal_funds,
           other_disbursements,
           last_net_contributions,
           offsets_to_operating_expenditures,
           other_political_committee_contributions,
           individual_itemized_contributions,
           loans,
           last_cash_on_hand_end_period,
           loan_repayments_other_loans,
           loans_made_by_candidate,
           net_operating_expenditures,
           contributions,
           contribution_refunds,
           total_offsets_to_operating_expenditures,
           disbursements,
           all_other_loans)

  if("candidate_id" %in% names(data)){
    tidy_candidates <- left_join(tidy_candidates, data, by = c("candidate_id" = "candidate_id"))
  }

  object_to_return <- list(
    tidy = tidy_candidates,
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
