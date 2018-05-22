#' Get line item expenditures for independent expenditures.
#'
#' Get specific line item expenditures for independent expenditures. Schedule E covers "outside spending".
#'
#' @param data A dataframe or tibble. Usually this will be the returned result of search_candidates(). If a column is called 'candidate_id', get_candidate_totals() will return results for all IDs in that column and will attempt to join data to the result by candidate_id. Either this argument or candidate_ids is required
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param is_notice Record filed as 24- or 48-hour notice
#' @param payee_name Name of the entity that received the payment
#' @param filing_form Filing form
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null
#' @param cycle Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
#' @param sort_hide_null Hide null values on sorted column(s).
#' @param max_date Maximum date
#' @param candidate_id A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param min_amount Filter for all amounts greater than a value.
#' @param support_oppose_indicator Support or opposition
#' @param min_date Minimum date
#' @param min_image_number
#' @param image_number The image number of the page where the schedule item is reported
#' @param max_amount Filter for all amounts less than a value.
#' @param last_expenditure_amount When sorting by expenditure_amount, this is populated with the expenditure_amount of the last result. However, you will need to pass the index of that last result to last_index to get the next page.
#' @param committee_id A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
#' @param last_office_total_ytd When sorting by office_total_ytd, this is populated with the office_total_ytd of the last result. However, you will need to pass the index of that last result to last_index to get the next page.
#' @param max_image_number
#' @param last_expenditure_date When sorting by expenditure_date, this is populated with the expenditure_date of the last result. However, you will need to pass the index of that last result to last_index to get the next page.
#' @param line_number Filter for form and line number using the following format: FORM-LINENUMBER. For example an argument such as F3X-16 would filter down to all entries from form F3X line number 16.
#'
#' @return
#' @export
#'
#' @examples
get_independent_expenditures <- function(
  data = NULL,
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  data_structure = "tidy",
  is_notice = NULL,
  payee_name = NULL,
  filing_form = NULL,
  sort_null_only = NULL,
  cycle = NULL,
  sort_hide_null = NULL,
  max_date = NULL,
  candidate_id = NULL,
  sort = NULL,
  min_amount = NULL,
  support_oppose_indicator = NULL,
  min_date = NULL,
  min_image_number = NULL,
  image_number = NULL,
  max_amount = NULL,
  last_expenditure_amount = NULL,
  committee_id = NULL,
  last_office_total_ytd = NULL,
  max_image_number = NULL,
  last_expenditure_date = NULL,
  line_number = NULL
){
  #Check for API Key
  if (is.null(api_key)) {

    stop('An API key is required. Obtain one at https://api.data.gov/signup.')

  }

  #Use the candidate_id column if data has it and it hasn't been supplied (usually a search_candidates() result)
  if (!is.null(data)){

    if (is.null(candidate_id)){

      if ('candidate_id' %in% names(data)){

        candidate_id <- data[['candidate_id']]

      }

    }

  }

  candidate_id <- candidate_id %>%
    unique()

  responses <- list()

  query_parameters <- list(
    is_notice = is_notice,
    payee_name = payee_name,
    filing_form = filing_form,
    sort_null_only = sort_null_only,
    cycle = cycle,
    sort_hide_null = sort_hide_null,
    max_date = max_date,
    candidate_id = candidate_id,
    sort = sort,
    min_amount = min_amount,
    support_oppose_indicator = support_oppose_indicator,
    min_date = min_date,
    min_image_number = min_image_number,
    image_number = image_number,
    max_amount = max_amount,
    last_expenditure_amount = last_expenditure_amount,
    committee_id = committee_id,
    last_office_total_ytd = last_office_total_ytd,
    max_image_number = max_image_number,
    last_expenditure_date = last_expenditure_date,
    line_number = line_number,
    api_key = api_key,
    per_page = 100
  )

  responses[[1]] <- get_openfec(path = paste0("/schedules/schedule_e/"), query_parameters = query_parameters)

  total_pages <- responses[[1]][["parsed"]][["pagination"]][["pages"]]

  total_count <- responses[[1]][['parsed']][["pagination"]][["count"]]

  message("Itemized independent expenditures found: ", total_count)

  if(total_pages > 1){

    message(paste0("There are about ", total_pages, " pages of results to get containing approximately ", total_count, " itemized independent expenditures."))

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

      responses[[i]] <- get_openfec(path = paste0("/schedules/schedule_e/"), query_parameters = query_parameters)

      if(length(responses[[i]]$parsed$results) == 0){
        more_results <- FALSE
      }

      i <- i + 1

    }

  }

  tidy_donations <- responses %>%
    purrr::map(function(x) x$parsed$results) %>%
    unlist(recursive = F) %>%
    #Help for creating tibble
    #cat(paste0(tidy_donations[[1]] %>% names(),' = map_chr(. , "', tidy_donations[[1]] %>% names(), '", .default = NA),', collapse = "\n"), sep = '\n')
    tibble(
      committee_id = map_chr(. , "committee_id", .default = NA),
      expenditure_amount = map_dbl(. , "expenditure_amount", .default = NA),
      memo_code_full = map_chr(. , "memo_code_full", .default = NA),
      back_reference_schedule_name = map_chr(. , "back_reference_schedule_name", .default = NA),
      election_type_full = map_chr(. , "election_type_full", .default = NA),
      schedule_type_full = map_chr(. , "schedule_type_full", .default = NA),
      line_number = map_chr(. , "line_number", .default = NA),
      expenditure_description = map_chr(. , "expenditure_description", .default = NA),
      conduit_committee_id = map_chr(. , "conduit_committee_id", .default = NA),
      conduit_committee_city = map_chr(. , "conduit_committee_city", .default = NA),
      notary_commission_expiration_date = map_chr(. , "notary_commission_expiration_date", .default = NA),
      expenditure_date = map_chr(. , "expenditure_date", .default = NA),
      conduit_committee_street1 = map_chr(. , "conduit_committee_street1", .default = NA),
      payee_name = map_chr(. , "payee_name", .default = NA),
      is_notice = map_chr(. , "is_notice", .default = NA),
      action_code_full = map_chr(. , "action_code_full", .default = NA),
      conduit_committee_zip = map_chr(. , "conduit_committee_zip", .default = NA),
      conduit_committee_street2 = map_chr(. , "conduit_committee_street2", .default = NA),
      office_total_ytd = map_chr(. , "office_total_ytd", .default = NA),
      payee_street_2 = map_chr(. , "payee_street_2", .default = NA),
      payee_zip = map_chr(. , "payee_zip", .default = NA),
      schedule_type = map_chr(. , "schedule_type", .default = NA),
      committee = map(. , "committee", .default = NA),
      conduit_committee_name = map_chr(. , "conduit_committee_name", .default = NA),
      payee_state = map_chr(. , "payee_state", .default = NA),
      independent_sign_name = map_chr(. , "independent_sign_name", .default = NA),
      candidate_first_name = map_chr(. , "candidate_first_name", .default = NA),
      independent_sign_date = map_chr(. , "independent_sign_date", .default = NA),
      file_number = map_chr(. , "file_number", .default = NA),
      filer_first_name = map_chr(. , "filer_first_name", .default = NA),
      image_number = map_chr(. , "image_number", .default = NA),
      candidate_office = map_chr(. , "candidate_office", .default = NA),
      pdf_url = map_chr(. , "pdf_url", .default = NA),
      candidate_prefix = map_chr(. , "candidate_prefix", .default = NA),
      payee_suffix = map_chr(. , "payee_suffix", .default = NA),
      notary_sign_name = map_chr(. , "notary_sign_name", .default = NA),
      candidate_id = map_chr(. , "candidate_id", .default = NA),
      category_code_full = map_chr(. , "category_code_full", .default = NA),
      conduit_committee_state = map_chr(. , "conduit_committee_state", .default = NA),
      memo_code = map_chr(. , "memo_code", .default = NA),
      original_sub_id = map_chr(. , "original_sub_id", .default = NA),
      filing_form = map_chr(. , "filing_form", .default = NA),
      notary_sign_date = map_chr(. , "notary_sign_date", .default = NA),
      memo_text = map_chr(. , "memo_text", .default = NA),
      payee_middle_name = map_chr(. , "payee_middle_name", .default = NA),
      payee_street_1 = map_chr(. , "payee_street_1", .default = NA),
      report_year = map_chr(. , "report_year", .default = NA),
      link_id = map_chr(. , "link_id", .default = NA),
      candidate = map(. , "candidate", .default = NA),
      support_oppose_indicator = map_chr(. , "support_oppose_indicator", .default = NA),
      payee_city = map_chr(. , "payee_city", .default = NA),
      filer_prefix = map_chr(. , "filer_prefix", .default = NA),
      filer_last_name = map_chr(. , "filer_last_name", .default = NA),
      sub_id = map_chr(. , "sub_id", .default = NA),
      filer_suffix = map_chr(. , "filer_suffix", .default = NA),
      candidate_last_name = map_chr(. , "candidate_last_name", .default = NA),
      cand_office_state = map_chr(. , "cand_office_state", .default = NA),
      filer_middle_name = map_chr(. , "filer_middle_name", .default = NA),
      candidate_name = map_chr(. , "candidate_name", .default = NA),
      action_code = map_chr(. , "action_code", .default = NA),
      payee_last_name = map_chr(. , "payee_last_name", .default = NA),
      payee_prefix = map_chr(. , "payee_prefix", .default = NA),
      memoed_subtotal = map_chr(. , "memoed_subtotal", .default = NA),
      cand_office_district = map_chr(. , "cand_office_district", .default = NA),
      report_type = map_chr(. , "report_type", .default = NA),
      candidate_middle_name = map_chr(. , "candidate_middle_name", .default = NA),
      dissemination_date = map_chr(. , "dissemination_date", .default = NA),
      election_type = map_chr(. , "election_type", .default = NA),
      payee_first_name = map_chr(. , "payee_first_name", .default = NA),
      candidate_suffix = map_chr(. , "candidate_suffix", .default = NA),
      category_code = map_chr(. , "category_code", .default = NA),
      back_reference_transaction_id = map_chr(. , "back_reference_transaction_id", .default = NA)
    )

  object_to_return <- list(
    tidy = tidy_donations,
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

#' @rdname get_independent_expenditures
#' @export
get_schedule_e <- get_independent_expenditures
