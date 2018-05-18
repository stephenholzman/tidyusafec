#' Get Debts Owed
#'
#' Get specific debt information reported by committees. Schedule D shows all debts owed. If you just need total debts, use get_candidate_totals().
#'
#' Schedule D shows debts and obligations owed to or by the committee that are required to be disclosed.
#'
#' @param data A dataframe or tibble. Usually this will be the returned result of search_candidates(). If a column is called 'candidate_id', get_candidate_totals() will return results for all IDs in that column and will attempt to join data to the result by candidate_id. Either this argument or candidate_ids is required
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param min_amount_incurred
#' @param min_payment_period
#' @param max_payment_period
#' @param creditor_debtor_name
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null
#' @param nature_of_debt
#' @param sort_hide_null Hide null values on sorted column(s).
#' @param max_date Maximum date
#' @param candidate_id A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
#' @param max_amount_incurred
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param min_amount Filter for all amounts greater than a value.
#' @param min_date Minimum date
#' @param min_image_number
#' @param image_number The image number of the page where the schedule item is reported.
#' @param max_amount Filter for all amounts less than a value.
#' @param committee_id A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
#' @param max_image_number
#' @param line_number Filter for form and line number using the following format: FORM-LINENUMBER. For example an argument such as F3X-16 would filter down to all entries from form F3X line number 16.
#'
#' @return
#' @export
#'
#' @examples
#'
get_itemized_debts <- function(
  data = NULL,
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  data_structure = "tidy",
  min_amount_incurred = NULL,
  min_payment_period = NULL,
  max_payment_period = NULL,
  creditor_debtor_name = NULL,
  sort_null_only = NULL,
  nature_of_debt = NULL,
  sort_hide_null = NULL,
  max_date = NULL,
  candidate_id = NULL,
  max_amount_incurred = NULL,
  sort = NULL,
  min_amount = NULL,
  min_date = NULL,
  min_image_number = NULL,
  image_number = NULL,
  max_amount = NULL,
  committee_id = NULL,
  max_image_number = NULL,
  line_number = NULL
){
  #Check for API Key
  if (is.null(api_key)) {

    stop('An API key is required. Obtain one at https://api.data.gov/signup.')

  }

  #Use the committee_id column if data has it and it hasn't been supplied (usually a search_candidates() result)
  if (!is.null(data)){

    if (is.null(committee_id)){

      if ('committee_id' %in% names(data)){

        committee_id <- data[['committee_id']]

      }

    }

  }

  committee_id <- committee_id %>%
    unique()

  responses <- list()

  query_parameters <- list(
    min_amount_incurred = min_amount_incurred,
    min_payment_period = min_payment_period,
    max_payment_period = max_payment_period,
    creditor_debtor_name = creditor_debtor_name,
    sort_null_only = sort_null_only,
    nature_of_debt = nature_of_debt,
    sort_hide_null = sort_hide_null,
    max_date = max_date,
    candidate_id = candidate_id,
    max_amount_incurred = max_amount_incurred,
    sort = sort,
    min_amount = min_amount,
    min_date = min_date,
    min_image_number = min_image_number,
    image_number = image_number,
    max_amount = max_amount,
    committee_id = committee_id,
    max_image_number = max_image_number,
    line_number = line_number,
    api_key = api_key,
    per_page = 100
  )

  responses[[1]] <- get_openfec(path = paste0("/schedules/schedule_d/"), query_parameters = query_parameters)

  total_pages <- responses[[1]][["parsed"]][["pagination"]][["pages"]]

  total_count <- responses[[1]][['parsed']][["pagination"]][["count"]]

  message("Debts found: ", total_count)

  if(total_pages > 1){

    message(paste0("There are about ", total_pages, " pages of results to get containing approximately ", total_count, " debts."))

    i <- 2

    more_results <- TRUE

    while(more_results){

      if(i == 2 | i %% 10 == 0 | i == total_pages){
        message("On page ", i,"/",total_pages)
      }

      ### Rate Limit Controls
      Sys.sleep(.5) #With an upgraded key, max limit is 120 calls per minute.

      #Check the last response
      #tk

      for(last_index in names(responses[[i - 1]][['parsed']][["pagination"]][["last_indexes"]])){

        query_parameters[[last_index]] <- responses[[i - 1]][['parsed']][['pagination']][['last_indexes']][[last_index]]

      }

      responses[[i]] <- get_openfec(path = paste0("/schedules/schedule_d/"), query_parameters = query_parameters)

      if(length(responses[[i]]$parsed$results) == 0){
        more_results <- FALSE
      }

      i <- i + 1

    }

  }
  #
  #
  #   %>%
  tidy_debts <- responses %>%
  purrr::map(function(x) x$parsed$results) %>%
    unlist(recursive = F) %>%
    tibble(
      report_year = map_chr(. , "report_year", .default = NA),
      creditor_debtor_suffix = map_chr(. , "creditor_debtor_suffix", .default = NA),
      conduit_committee_name = map_chr(. , "conduit_committee_name", .default = NA),
      candidate_office_state = map_chr(. , "candidate_office_state", .default = NA),
      sub_id = map_chr(. , "sub_id", .default = NA),
      committee_name = map_chr(. , "committee_name", .default = NA),
      original_sub_id = map_chr(. , "original_sub_id", .default = NA),
      election_cycle = map_chr(. , "election_cycle", .default = NA),
      outstanding_balance_close_of_period = map_dbl(. , "outstanding_balance_close_of_period", .default = NA),
      conduit_committee_city = map_chr(. , "conduit_committee_city", .default = NA),
      creditor_debtor_first_name = map_chr(. , "creditor_debtor_first_name", .default = NA),
      creditor_debtor_middle_name = map_chr(. , "creditor_debtor_middle_name", .default = NA),
      candidate_first_name = map_chr(. , "candidate_first_name", .default = NA),
      creditor_debtor_id = map_chr(. , "creditor_debtor_id", .default = NA),
      creditor_debtor_state = map_chr(. , "creditor_debtor_state", .default = NA),
      creditor_debtor_street1 = map_chr(. , "creditor_debtor_street1", .default = NA),
      file_number = map_chr(. , "file_number", .default = NA),
      line_number = map_chr(. , "line_number", .default = NA),
      conduit_committee_state = map_chr(. , "conduit_committee_state", .default = NA),
      conduit_committee_zip = map_chr(. , "conduit_committee_zip", .default = NA),
      pdf_url = map_chr(. , "pdf_url", .default = NA),
      entity_type = map_chr(. , "entity_type", .default = NA),
      creditor_debtor_city = map_chr(. , "creditor_debtor_city", .default = NA),
      conduit_committee_street1 = map_chr(. , "conduit_committee_street1", .default = NA),
      link_id = map_chr(. , "link_id", .default = NA),
      action_code_full = map_chr(. , "action_code_full", .default = NA),
      conduit_committee_id = map_chr(. , "conduit_committee_id", .default = NA),
      schedule_type_full = map_chr(. , "schedule_type_full", .default = NA),
      action_code = map_chr(. , "action_code", .default = NA),
      canidate_name = map_chr(. , "canidate_name", .default = NA),
      schedule_type = map_chr(. , "schedule_type", .default = NA),
      report_type = map_chr(. , "report_type", .default = NA),
      payment_period = map_chr(. , "payment_period", .default = NA),
      conduit_committee_street2 = map_chr(. , "conduit_committee_street2", .default = NA),
      load_date = map_chr(. , "load_date", .default = NA),
      committee = map(. , "committee", .default = NA),
      outstanding_balance_beginning_of_period = map_dbl(. , "outstanding_balance_beginning_of_period", .default = NA),
      candidate_office_district = map_chr(. , "candidate_office_district", .default = NA),
      creditor_debtor_last_name = map_chr(. , "creditor_debtor_last_name", .default = NA),
      filing_form = map_chr(. , "filing_form", .default = NA),
      nature_of_debt = map_chr(. , "nature_of_debt", .default = NA),
      amount_incurred_period = map_chr(. , "amount_incurred_period", .default = NA),
      creditor_debtor_name = map_chr(. , "creditor_debtor_name", .default = NA),
      candidate_id = map_chr(. , "candidate_id", .default = NA),
      candidate_last_name = map_chr(. , "candidate_last_name", .default = NA),
      image_number = map_chr(. , "image_number", .default = NA),
      committee_id = map_chr(. , "committee_id", .default = NA),
      transaction_id = map_chr(. , "transaction_id", .default = NA),
      creditor_debtor_prefix = map_chr(. , "creditor_debtor_prefix", .default = NA),
      creditor_debtor_street2 = map_chr(. , "creditor_debtor_street2", .default = NA),
      candidate_office_state_full = map_chr(. , "candidate_office_state_full", .default = NA),
      candidate_office = map_chr(. , "candidate_office", .default = NA)
    )
  #
  object_to_return <- list(
    tidy = tidy_debts,
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

#' @rdname get_committee_debts
#' @export
get_schedule_d <- get_itemized_debts
