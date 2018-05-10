#' Get Itemized Campaign Disbursements
#'
#' Get itemized disbursements (Schedule B reported on Forms F3, F3X and F3P). Returns information about spending. If you just need total disbursements, use get_candidate_totals().
#'
#' OpenFEC Documentation: Schedule B filings describe itemized disbursements. This data explains how committees and other filers spend their money. These figures are reported as part of forms F3, F3X and F3P.
#'
#' The data is divided in two-year periods, called two_year_transaction_period, which is derived from the disbursement_date. If no value is supplied, the results will default to the most recent two-year period that is named after the ending, even-numbered year.
#'
#' @param data A dataframe or tibble. Usually this will be the returned result of search_candidates(). If a column is called 'candidate_id', get_candidate_totals() will return results for all IDs in that column and will attempt to join data to the result by candidate_id. Either this argument or candidate_ids is required
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param recipient_state State of recipient
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null
#' @param recipient_city City of recipient
#' @param sort_hide_null Hide null values on sorted column(s).
#' @param max_date Maximum date
#' @param disbursement_purpose_category Disbursement purpose category
#' @param last_disbursement_amount When sorting by disbursement_amount, this is populated with the disbursement_amount of the last result. However, you will need to pass the index of that last result to last_index to get the next page.
#' @param recipient_name Name of recipient
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param min_amount Filter for all amounts greater than a value.
#' @param min_date Minimum date
#' @param last_disbursement_date When sorting by disbursement_date, this is populated with the disbursement_date of the last result. However, you will need to pass the index of that last result to last_index to get the next page.
#' @param min_image_number
#' @param image_number The image number of the page where the schedule item is reported
#' @param max_amount Filter for all amounts less than a value.
#' @param recipient_committee_id The FEC identifier should be represented here if the contributor is registered with the FEC.
#' @param committee_id A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
#' @param max_image_number
#' @param disbursement_description Description of disbursement
#' @param two_year_transaction_period This is a two-year period that is derived from the year a transaction took place in the Itemized Schedule A and Schedule B tables. In cases where we have the date of the transaction (contribution_receipt_date in schedules/schedule_a, disbursement_date in schedules/schedule_b) the two_year_transaction_period is named after the ending, even-numbered year. If we do not have the date of the transaction, we fall back to using the report year (report_year in both tables) instead, making the same cycle adjustment as necessary. If no transaction year is specified, the results default to the most current cycle.
#' @param line_number Filter for form and line number using the following format: FORM-LINENUMBER. For example an argument such as F3X-16 would filter down to all entries from form F3X line number 16.
#'
get_itemized_disbursements <- function(
  data = NULL,
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  data_structure = "tidy",
  recipient_state = NULL,
  sort_null_only = NULL,
  recipient_city = NULL,
  sort_hide_null = NULL,
  max_date = NULL,
  disbursement_purpose_category = NULL,
  last_disbursement_amount = NULL,
  recipient_name = NULL,
  sort = NULL,
  min_amount = NULL,
  min_date = NULL,
  last_disbursement_date = NULL,
  min_image_number = NULL,
  image_number = NULL,
  max_amount = NULL,
  recipient_committee_id = NULL,
  committee_id = NULL,
  max_image_number = NULL,
  disbursement_description = NULL,
  two_year_transaction_period = NULL,
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
    recipient_state = recipient_state,
    sort_null_only = sort_null_only,
    recipient_city = recipient_city,
    sort_hide_null = sort_hide_null,
    max_date = max_date,
    disbursement_purpose_category = disbursement_purpose_category,
    last_disbursement_amount = last_disbursement_amount,
    recipient_name = recipient_name,
    sort = sort,
    min_amount = min_amount,
    min_date = min_date,
    last_disbursement_date = last_disbursement_date,
    min_image_number = min_image_number,
    image_number = image_number,
    max_amount = max_amount,
    recipient_committee_id = recipient_committee_id,
    committee_id = committee_id,
    max_image_number = max_image_number,
    disbursement_description = disbursement_description,
    two_year_transaction_period = two_year_transaction_period,
    line_number = line_number,
    api_key = api_key,
    per_page = 100
  )

  responses[[1]] <- get_openfec(path = paste0("/schedules/schedule_b/"), query_parameters = query_parameters)

  total_pages <- responses[[1]][["parsed"]][["pagination"]][["pages"]]

  total_count <- responses[[1]][['parsed']][["pagination"]][["count"]]

  message("Itemized disbursements found: ", total_count)

  if(total_pages > 1){

    message(paste0("There are about ", total_pages, " pages of results to get containing approximately ", total_count, " itemized disbursements"))

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

      responses[[i]] <- get_openfec(path = paste0("/schedules/schedule_b/"), query_parameters = query_parameters)

      if(length(responses[[i]]$parsed$results) == 0){
        more_results <- FALSE
      }

      i <- i + 1

    }

  }

  tidy_disbursements <- responses %>%
    purrr::map(function(x) x$parsed$results) %>%
    unlist(recursive = F) %>%
    #Help for creating tibble
    #cat(paste0(tidy_donations[[1]] %>% names(),' = map_chr(. , "', tidy_donations[[1]] %>% names(), '", .default = NA),', collapse = "\n"), sep = '\n')
    tibble(
      disbursement_description = map_chr(. , "disbursement_description", .default = NA),
      entity_type_desc = map_chr(. , "entity_type_desc", .default = NA),
      unused_recipient_committee_id = map_chr(. , "unused_recipient_committee_id", .default = NA),
      candidate_office_state = map_chr(. , "candidate_office_state", .default = NA),
      fec_election_year = map_chr(. , "fec_election_year", .default = NA),
      candidate_office_district = map_chr(. , "candidate_office_district", .default = NA),
      report_type = map_chr(. , "report_type", .default = NA),
      recipient_name = map_chr(. , "recipient_name", .default = NA),
      payee_suffix = map_chr(. , "payee_suffix", .default = NA),
      candidate_first_name = map_chr(. , "candidate_first_name", .default = NA),
      national_committee_nonfederal_account = map_chr(. , "national_committee_nonfederal_account", .default = NA),
      recipient_committee_id = map_chr(. , "recipient_committee_id", .default = NA),
      category_code = map_chr(. , "category_code", .default = NA),
      semi_annual_bundled_refund = map_dbl(. , "semi_annual_bundled_refund", .default = NA),
      timestamp = map_chr(. , "timestamp", .default = NA),
      committee_id = map_chr(. , "committee_id", .default = NA),
      election_type = map_chr(. , "election_type", .default = NA),
      conduit_committee_zip = map_chr(. , "conduit_committee_zip", .default = NA),
      comm_dt = map_chr(. , "comm_dt", .default = NA),
      conduit_committee_street2 = map_chr(. , "conduit_committee_street2", .default = NA),
      payee_middle_name = map_chr(. , "payee_middle_name", .default = NA),
      schedule_type = map_chr(. , "schedule_type", .default = NA),
      load_date = map_chr(. , "load_date", .default = NA),
      two_year_transaction_period = map_chr(. , "two_year_transaction_period", .default = NA),
      file_number = map_chr(. , "file_number", .default = NA),
      election_type_full = map_chr(. , "election_type_full", .default = NA),
      candidate_last_name = map_chr(. , "candidate_last_name", .default = NA),
      disbursement_type_description = map_chr(. , "disbursement_type_description", .default = NA),
      cycle = map_chr(. , "cycle", .default = NA),
      beneficiary_committee_name = map_chr(. , "beneficiary_committee_name", .default = NA),
      payee_occupation = map_chr(. , "payee_occupation", .default = NA),
      conduit_committee_city = map_chr(. , "conduit_committee_city", .default = NA),
      ref_disp_excess_flg = map_chr(. , "ref_disp_excess_flg", .default = NA),
      recipient_committee = map(. , "recipient_committee", .default = NA),
      conduit_committee_state = map_chr(. , "conduit_committee_state", .default = NA),
      sub_id = map_chr(. , "sub_id", .default = NA),
      payee_prefix = map_chr(. , "payee_prefix", .default = NA),
      amendment_indicator_desc = map_chr(. , "amendment_indicator_desc", .default = NA),
      payee_first_name = map_chr(. , "payee_first_name", .default = NA),
      recipient_zip = map_chr(. , "recipient_zip", .default = NA),
      conduit_committee_name = map_chr(. , "conduit_committee_name", .default = NA),
      memo_code_full = map_chr(. , "memo_code_full", .default = NA),
      candidate_office_description = map_chr(. , "candidate_office_description", .default = NA),
      candidate_suffix = map_chr(. , "candidate_suffix", .default = NA),
      amendment_indicator = map_chr(. , "amendment_indicator", .default = NA),
      recipient_city = map_chr(. , "recipient_city", .default = NA),
      memo_text = map_chr(. , "memo_text", .default = NA),
      disbursement_purpose_category = map_chr(. , "disbursement_purpose_category", .default = NA),
      payee_employer = map_chr(. , "payee_employer", .default = NA),
      back_reference_transaction_id = map_chr(. , "back_reference_transaction_id", .default = NA),
      memoed_subtotal = map_dbl(. , "memoed_subtotal", .default = NA),
      candidate_name = map_chr(. , "candidate_name", .default = NA),
      line_number_label = map_chr(. , "line_number_label", .default = NA),
      filing_form = map_chr(. , "filing_form", .default = NA),
      back_reference_schedule_id = map_chr(. , "back_reference_schedule_id", .default = NA),
      candidate_middle_name = map_chr(. , "candidate_middle_name", .default = NA),
      disbursement_amount = map_dbl(. , "disbursement_amount", .default = NA),
      original_sub_id = map_chr(. , "original_sub_id", .default = NA),
      memo_code = map_chr(. , "memo_code", .default = NA),
      line_number = map_chr(. , "line_number", .default = NA),
      conduit_committee_street1 = map_chr(. , "conduit_committee_street1", .default = NA),
      transaction_id = map_chr(. , "transaction_id", .default = NA),
      recipient_state = map_chr(. , "recipient_state", .default = NA),
      disbursement_date = map_chr(. , "disbursement_date", .default = NA),
      schedule_type_full = map_chr(. , "schedule_type_full", .default = NA),
      candidate_prefix = map_chr(. , "candidate_prefix", .default = NA),
      report_year = map_chr(. , "report_year", .default = NA),
      pdf_url = map_chr(. , "pdf_url", .default = NA),
      disbursement_type = map_chr(. , "disbursement_type", .default = NA),
      candidate_id = map_chr(. , "candidate_id", .default = NA),
      candidate_office_state_full = map_chr(. , "candidate_office_state_full", .default = NA),
      link_id = map_chr(. , "link_id", .default = NA),
      fec_election_type_desc = map_chr(. , "fec_election_type_desc", .default = NA),
      candidate_office = map_chr(. , "candidate_office", .default = NA),
      committee = map(. , "committee", .default = NA),
      entity_type = map_chr(. , "entity_type", .default = NA),
      payee_last_name = map_chr(. , "payee_last_name", .default = NA),
      image_number = map_chr(. , "image_number", .default = NA),
      category_code_full = map_chr(. , "category_code_full", .default = NA)
    )

  object_to_return <- list(
    tidy = tidy_disbursements,
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

#' @rdname get_schedule_b

get_schedule_b <- get_itemized_disbursements
