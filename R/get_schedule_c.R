#' Get Loans
#'
#' Get specific loan information reported by committees. Schedule C shows all loans, endorsements and loan guarantees a committee receives or makes. If you just need total loans, use get_candidate_totals().
#'
#' The committee continues to report the loan until it is repaid.
#'
#' @param data A dataframe or tibble. Usually this will be the returned result of search_candidates(). If a column is called 'candidate_id', get_candidate_totals() will return results for all IDs in that column and will attempt to join data to the result by candidate_id. Either this argument or candidate_ids is required
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param max_payment_to_date Maximum payment to date
#' @param min_payment_to_date Minimum payment to date
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null
#' @param sort_hide_null Hide null values on sorted column(s).
#' @param max_date Maximum date
#' @param candidate_name Name of candidate running for office
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param min_amount Filter for all amounts greater than a value.
#' @param min_date Minimum date
#' @param min_image_number
#' @param image_number The image number of the page where the schedule item is reported
#' @param max_amount Filter for all amounts less than a value.
#' @param loaner_name Source of the loan (i.e., bank loan, brokerage account, credit card, home equity line of credit,other line of credit, or personal funds of the candidate
#' @param committee_id A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
#' @param max_image_number
#' @param line_number Filter for form and line number using the following format: FORM-LINENUMBER. For example an argument such as F3X-16 would filter down to all entries from form F3X line number 16.
#'
#' @return
#' @export
#'
#' @examples
#'
#'
get_committee_loans <- function(
  data = NULL,
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  data_structure = "tidy",
  max_payment_to_date = NULL,
  min_payment_to_date = NULL,
  sort_null_only = NULL,
  sort_hide_null = NULL,
  max_date = NULL,
  candidate_name = NULL,
  sort = NULL,
  min_amount = NULL,
  min_date = NULL,
  min_image_number = NULL,
  image_number = NULL,
  max_amount = NULL,
  loaner_name = NULL,
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
    max_payment_to_date = max_payment_to_date,
    min_payment_to_date = min_payment_to_date,
    sort_null_only = sort_null_only,
    sort_hide_null = sort_hide_null,
    max_date = max_date,
    candidate_name = candidate_name,
    sort = sort,
    min_amount = min_amount,
    min_date = min_date,
    min_image_number = min_image_number,
    image_number = image_number,
    max_amount = max_amount,
    loaner_name = loaner_name,
    committee_id = committee_id,
    max_image_number = max_image_number,
    line_number = line_number,
    api_key = api_key,
    per_page = 100
  )

  responses[[1]] <- get_openfec(path = paste0("/schedules/schedule_c/"), query_parameters = query_parameters)

  total_pages <- responses[[1]][["parsed"]][["pagination"]][["pages"]]

  total_count <- responses[[1]][['parsed']][["pagination"]][["count"]]

  message("Loans found: ", total_count)

  if(total_pages > 1){

    message(paste0("There are about ", total_pages, " pages of results to get containing approximately ", total_count, " loans."))

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

      responses[[i]] <- get_openfec(path = paste0("/schedules/schedule_c/"), query_parameters = query_parameters)

      if(length(responses[[i]]$parsed$results) == 0){
        more_results <- FALSE
      }

      i <- i + 1

    }

  }


  tidy_loans <- responses %>%
    purrr::map(function(x) x$parsed$results) %>%
    unlist(recursive = F) %>%
    #Help for creating tibble
    #cat(paste0(tidy_loans[[1]] %>% names(),' = map_chr(. , "', tidy_loans[[1]] %>% names(), '", .default = NA),', collapse = "\n"), sep = '\n')
    tibble(
      loan_source_city = map_chr(. , "loan_source_city", .default = NA),
      loan_source_street_2 = map_chr(. , "loan_source_street_2", .default = NA),
      filing_form = map_chr(. , "filing_form", .default = NA),
      cycle = map_chr(. , "cycle", .default = NA),
      schedule_type = map_chr(. , "schedule_type", .default = NA),
      interest_rate_terms = map_chr(. , "interest_rate_terms", .default = NA),
      candidate_office_district = map_chr(. , "candidate_office_district", .default = NA),
      load_date = map_chr(. , "load_date", .default = NA),
      candidate_id = map_chr(. , "candidate_id", .default = NA),
      personally_funded = map_chr(. , "personally_funded", .default = NA),
      image_number = map_chr(. , "image_number", .default = NA),
      candidate_office_full = map_chr(. , "candidate_office_full", .default = NA),
      link_id = map_chr(. , "link_id", .default = NA),
      loan_source_street_1 = map_chr(. , "loan_source_street_1", .default = NA),
      action_code = map_chr(. , "action_code", .default = NA),
      payment_to_date = map_chr(. , "payment_to_date", .default = NA),
      loan_source_state = map_chr(. , "loan_source_state", .default = NA),
      report_year = map_chr(. , "report_year", .default = NA),
      original_sub_id = map_chr(. , "original_sub_id", .default = NA),
      loan_source_first_name = map_chr(. , "loan_source_first_name", .default = NA),
      candidate_name = map_chr(. , "candidate_name", .default = NA),
      pdf_url = map_chr(. , "pdf_url", .default = NA),
      loan_source_last_name = map_chr(. , "loan_source_last_name", .default = NA),
      action_code_full = map_chr(. , "action_code_full", .default = NA),
      entity_type = map_chr(. , "entity_type", .default = NA),
      committee = map(. , "committee", .default = NA),
      election_type = map_chr(. , "election_type", .default = NA),
      sub_id = map_chr(. , "sub_id", .default = NA),
      incurred_date = map_chr(. , "incurred_date", .default = NA),
      report_type = map_chr(. , "report_type", .default = NA),
      file_number = map_chr(. , "file_number", .default = NA),
      entity_type_full = map_chr(. , "entity_type_full", .default = NA),
      schedule_type_full = map_chr(. , "schedule_type_full", .default = NA),
      loan_source_suffix = map_chr(. , "loan_source_suffix", .default = NA),
      candidate_last_name = map_chr(. , "candidate_last_name", .default = NA),
      candidate_middle_name = map_chr(. , "candidate_middle_name", .default = NA),
      original_loan_amount = map_dbl(. , "original_loan_amount", .default = NA),
      fec_election_type_year = map_chr(. , "fec_election_type_year", .default = NA),
      candidate_office_state_full = map_chr(. , "candidate_office_state_full", .default = NA),
      line_number = map_chr(. , "line_number", .default = NA),
      committee_id = map_chr(. , "committee_id", .default = NA),
      fec_election_type_full = map_chr(. , "fec_election_type_full", .default = NA),
      candidate_first_name = map_chr(. , "candidate_first_name", .default = NA),
      loan_source_prefix = map_chr(. , "loan_source_prefix", .default = NA),
      memo_text = map_chr(. , "memo_text", .default = NA),
      loan_source_zip = map_chr(. , "loan_source_zip", .default = NA),
      loan_balance = map_dbl(. , "loan_balance", .default = NA),
      candidate_office_state = map_chr(. , "candidate_office_state", .default = NA),
      secured_ind = map_chr(. , "secured_ind", .default = NA),
      due_date_terms = map_chr(. , "due_date_terms", .default = NA),
      election_type_full = map_chr(. , "election_type_full", .default = NA),
      loan_source_middle_name = map_chr(. , "loan_source_middle_name", .default = NA),
      fec_committee_id = map_chr(. , "fec_committee_id", .default = NA),
      loan_source_name = map_chr(. , "loan_source_name", .default = NA),
      candidate_prefix = map_chr(. , "candidate_prefix", .default = NA),
      candidate_suffix = map_chr(. , "candidate_suffix", .default = NA),
      candidate_office = map_chr(. , "candidate_office", .default = NA),
      transaction_id = map_chr(. , "transaction_id", .default = NA),
      memo_code = map_chr(. , "memo_code", .default = NA),
    )

  object_to_return <- list(
    tidy = tidy_loans,
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

#' @rdname get_committee_loans
#'
get_schedule_c <- get_committee_loans
