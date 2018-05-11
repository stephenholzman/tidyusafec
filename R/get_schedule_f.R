#' Get Party Expenditures for Federal Candidates
#'
#' Schedule F shows all special expenditures a national or state party committee makes in connection with the general election campaigns of federal candidates
#'
#' These coordinated party expenditures do not count against the contribution limits but are subject to other limits, these limits are detailed in Chapter 7 of the FEC Campaign Guide for Political Party Committees.
#'
#' @param data A dataframe or tibble. Usually this will be the returned result of search_candidates(). If a column is called 'candidate_id', get_candidate_totals() will return results for all IDs in that column and will attempt to join data to the result by candidate_id. Either this argument or candidate_ids is required
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param payee_name
#' @param cycle Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null
#' @param sort_hide_null Hide null values on sorted column(s).
#' @param max_date Maximum date
#' @param candidate_id A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param min_amount Filter for all amounts greater than a value.
#' @param min_date Minimum date
#' @param min_image_number
#' @param image_number The image number of the page where the schedule item is reported
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
get_party_expenditures <- function(
  data = NULL,
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  data_structure = "tidy",
  payee_name = NULL,
  cycle = NULL,
  sort_null_only = NULL,
  sort_hide_null = NULL,
  max_date = NULL,
  candidate_id = NULL,
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
    payee_name = payee_name,
    cycle = cycle,
    sort_null_only = sort_null_only,
    sort_hide_null = sort_hide_null,
    max_date = max_date,
    candidate_id = candidate_id,
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

  responses[[1]] <- get_openfec(path = paste0("/schedules/schedule_f/"), query_parameters = query_parameters)

  total_pages <- responses[[1]][["parsed"]][["pagination"]][["pages"]]

  total_count <- responses[[1]][['parsed']][["pagination"]][["count"]]

  message("Party expenditures found: ", total_count)

  if(total_pages > 1){

    message(paste0("There are about ", total_pages, " pages of results to get containing approximately ", total_count, " itemized party expenditures."))

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

      responses[[i]] <- get_openfec(path = paste0("/schedules/schedule_f/"), query_parameters = query_parameters)

      if(length(responses[[i]]$parsed$results) == 0){
        more_results <- FALSE
      }

      i <- i + 1

    }

  }

  tidy_expenditures <- responses %>%
    purrr::map(function(x) x$parsed$results) %>%
    unlist(recursive = F) %>%
    #Help for creating tibble
    #cat(paste0(tidy_donations[[1]] %>% names(),' = map_chr(. , "', tidy_donations[[1]] %>% names(), '", .default = NA),', collapse = "\n"), sep = '\n')
    tibble(
      report_year = map_chr(. , "report_year", .default = NA),
      unlimited_spending_flag = map_chr(. , "unlimited_spending_flag", .default = NA),
      conduit_committee_name = map_chr(. , "conduit_committee_name", .default = NA),
      candidate_office_state = map_chr(. , "candidate_office_state", .default = NA),
      payee_middle_name = map_chr(. , "payee_middle_name", .default = NA),
      election_cycle = map_chr(. , "election_cycle", .default = NA),
      conduit_committee_city = map_chr(. , "conduit_committee_city", .default = NA),
      payee_last_name = map_chr(. , "payee_last_name", .default = NA),
      memo_code = map_chr(. , "memo_code", .default = NA),
      candidate_first_name = map_chr(. , "candidate_first_name", .default = NA),
      candidate_office_state_full = map_chr(. , "candidate_office_state_full", .default = NA),
      conduit_committee_state = map_chr(. , "conduit_committee_state", .default = NA),
      file_number = map_chr(. , "file_number", .default = NA),
      line_number = map_chr(. , "line_number", .default = NA),
      designated_committee_name = map_chr(. , "designated_committee_name", .default = NA),
      candidate_middle_name = map_chr(. , "candidate_middle_name", .default = NA),
      link_id = map_chr(. , "link_id", .default = NA),
      payee_name = map_chr(. , "payee_name", .default = NA),
      conduit_committee_street1 = map_chr(. , "conduit_committee_street1", .default = NA),
      candidate_suffix = map_chr(. , "candidate_suffix", .default = NA),
      back_reference_schedule_name = map_chr(. , "back_reference_schedule_name", .default = NA),
      expenditure_amount = map_dbl(. , "expenditure_amount", .default = NA),
      candidate_office_full = map_chr(. , "candidate_office_full", .default = NA),
      report_type = map_chr(. , "report_type", .default = NA),
      load_date = map_chr(. , "load_date", .default = NA),
      committee = map(. , "committee", .default = NA),
      candidate_office_district = map_chr(. , "candidate_office_district", .default = NA),
      candidate_name = map_chr(. , "candidate_name", .default = NA),
      unlimited_spending_flag_full = map_chr(. , "unlimited_spending_flag_full", .default = NA),
      aggregate_general_election_expenditure = map_chr(. , "aggregate_general_election_expenditure", .default = NA),
      candidate_prefix = map_chr(. , "candidate_prefix", .default = NA),
      expenditure_purpose_full = map_chr(. , "expenditure_purpose_full", .default = NA),
      expenditure_date = map_chr(. , "expenditure_date", .default = NA),
      back_reference_transaction_id = map_chr(. , "back_reference_transaction_id", .default = NA),
      candidate_id = map_chr(. , "candidate_id", .default = NA),
      catolog_code = map_chr(. , "catolog_code", .default = NA),
      designated_committee_id = map_chr(. , "designated_committee_id", .default = NA),
      subordinate_committee = map_chr(. , "subordinate_committee", .default = NA),
      committee_name = map_chr(. , "committee_name", .default = NA),
      expenditure_type = map_chr(. , "expenditure_type", .default = NA),
      expenditure_type_full = map_chr(. , "expenditure_type_full", .default = NA),
      original_sub_id = map_chr(. , "original_sub_id", .default = NA),
      conduit_committee_zip = map_chr(. , "conduit_committee_zip", .default = NA),
      memo_text = map_chr(. , "memo_text", .default = NA),
      pdf_url = map_chr(. , "pdf_url", .default = NA),
      entity_type = map_chr(. , "entity_type", .default = NA),
      action_code_full = map_chr(. , "action_code_full", .default = NA),
      payee_first_name = map_chr(. , "payee_first_name", .default = NA),
      conduit_committee_id = map_chr(. , "conduit_committee_id", .default = NA),
      catolog_code_full = map_chr(. , "catolog_code_full", .default = NA),
      schedule_type_full = map_chr(. , "schedule_type_full", .default = NA),
      action_code = map_chr(. , "action_code", .default = NA),
      schedule_type = map_chr(. , "schedule_type", .default = NA),
      subordinate_committee_id = map_chr(. , "subordinate_committee_id", .default = NA),
      conduit_committee_street2 = map_chr(. , "conduit_committee_street2", .default = NA),
      filing_form = map_chr(. , "filing_form", .default = NA),
      memo_code_full = map_chr(. , "memo_code_full", .default = NA),
      committee_designated_coordinated_expenditure_indicator = map_chr(. , "committee_designated_coordinated_expenditure_indicator", .default = NA),
      candidate_last_name = map_chr(. , "candidate_last_name", .default = NA),
      image_number = map_chr(. , "image_number", .default = NA),
      committee_id = map_chr(. , "committee_id", .default = NA),
      transaction_id = map_chr(. , "transaction_id", .default = NA),
      entity_type_desc = map_chr(. , "entity_type_desc", .default = NA),
      sub_id = map_chr(. , "sub_id", .default = NA),
      candidate_office = map_chr(. , "candidate_office", .default = NA),
    )

  object_to_return <- list(
    tidy = tidy_expenditures,
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

#' @rdname get_party_expenditures
#'
get_schedule_f <- get_party_expenditures
