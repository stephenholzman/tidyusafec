#' Get Itemized Campaign Contributions
#'
#' Get itemized contributions (those reported on Form F3, F3X and F3P Schedule A). Returns information about donors, their residence, occupation, and donation amount. If you just need total itemized contributions, use get_candidate_totals().
#'
#' OpenFEC Documentation: Schedule A records describe itemized receipts reported by a committee. This is where you can look for individual contributors. If you are interested in individual donors, /schedules/schedule_a will be the endpoint you use.
#'
#' Once a person gives more than a total of $200, the donations of that person must be reported by committees that file F3, F3X and F3P forms.
#' Contributions $200 and under are not required to be itemized, but you can find the total amount of these small donations by looking up the "unitemized" field in the /reports or /totals endpoints.
#'
#' When comparing the totals from reports to line items. the totals will not match unless you only look at items where "is_individual":true since the same transaction is in the data multiple ways to explain the way it may move though different committees as an earmark.
#'
#' For the Schedule A aggregateshttps://api.open.fec.gov/developers/#!/party-coordinated_expenditures/get_schedules_schedule_f, such as by_occupation and by_state, include only unique individual contributions. See below for full methodology.
#'
#' @param data A dataframe or tibble. Usually this will be the returned result of search_candidates(). If a column is called 'committee_id', get_itemized_contributions() will return results for all IDs in that column and will attempt to join data to the result by committee_id.
#' @param api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param contributor_employer Employer of contributor, filers need to make an effort to gather this information
#' @param contributor_name Name of contributor.
#' @param contributor_occupation Occupation of contributor, filers need to make an effort to gather this information.
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null.
#' @param sort_null_hide Hide null values on sorted column(s).
#' @param contributor_id The FEC identifier should be represented here if the contributor is registered with the FEC.
#' @param max_date Maximum date.
#' @param last_contribution_receipt_date When sorting by contribution_receipt_date, this is populated with the contribution_receipt_date of the last result. However, you will need to pass the index of that last result to last_index to get the next page.
#' @param contributor_city City of contributor.
#' @param contributor_type Filters individual or committee contributions based on line number.
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param min_amount Filter for all amounts greater than a value.
#' @param contributor_zip Zip code of contributor. Only 5-digit zip code is allowed.
#' @param min_date Minimum date.
#' @param contributor_state State of contributor.
#' @param last_contributor_aggregate_ytd When sorting by contributor_aggregate_ytd, this is populated with the contributor_aggregate_ytd of the last result. However, you will need to pass the index of that last result to last_index to get the next page.
#' @param image_number The image number of the page where the schedule item is reported.
#' @param max_amount Filter for all amounts less than a value.
#' @param committee_id A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
#' @param last_contribution_receipt_amount When sorting by contribution_receipt_amount, this is populated with the contribution_receipt_amount of the last result. However, you will need to pass the index of that last result to last_index to get the next page.
#' @param is_individual Restrict to non-earmarked individual contributions where memo code is true. Filtering individuals is useful to make sure contributions are not double reported and in creating breakdowns of the amount of money coming from individuals.
#' @param two_year_transaction_period This is a two-year period that is derived from the year a transaction took place in the Itemized Schedule A and Schedule B tables. In cases where we have the date of the transaction (contribution_receipt_date in schedules/schedule_a, disbursement_date in schedules/schedule_b) the two_year_transaction_period is named after the ending, even-numbered year. If we do not have the date of the transaction, we fall back to using the report year (report_year in both tables) instead, making the same cycle adjustment as necessary. If no transaction year is specified, the results default to the most current cycle.
#' @param line_number Filter for form and line number using the following format: FORM-LINENUMBER. For example an argument such as F3X-16 would filter down to all entries from form F3X line number 16.
#'
#' @export
#'

get_itemized_contributions <- function(
  data = NULL,
  api_key = Sys.getenv('DATAGOV_API_KEY'),
  data_structure = 'tidy',
  contributor_employer = NULL,
  contributor_name = NULL,
  contributor_occupation = NULL,
  sort_null_only = NULL,
  sort_hide_null = NULL,
  contributor_id = NULL,
  max_date = NULL,
  last_contribution_receipt_date = NULL,
  contributor_city = NULL,
  contributor_type = NULL,
  sort = NULL,
  min_amount = NULL,
  contributor_zip = NULL,
  min_date = NULL,
  contributor_state = NULL,
  min_image_number = NULL,
  last_contributor_aggregate_ytd = NULL,
  image_number = NULL,
  max_amount = NULL,
  committee_id = NULL,
  last_contribution_receipt_amount = NULL,
  is_individual = NULL,
  max_image_number = NULL,
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
    contributor_employer = contributor_employer,
    contributor_name = contributor_name,
    contributor_occupation = contributor_occupation,
    sort_null_only = sort_null_only,
    sort_hide_null = sort_hide_null,
    contributor_id = contributor_id,
    max_date = max_date,
    last_contribution_receipt_date = last_contribution_receipt_date,
    contributor_city = contributor_city,
    contributor_type = contributor_type,
    sort = sort,
    min_amount = min_amount,
    contributor_zip = contributor_zip,
    min_date = min_date,
    committee_id = committee_id,
    contributor_state = contributor_state,
    min_image_number = min_image_number,
    last_contributor_aggregate_ytd = last_contributor_aggregate_ytd,
    image_number = image_number,
    max_amount = max_amount,
    last_contribution_receipt_amount = last_contribution_receipt_amount,
    is_individual = is_individual,
    max_image_number = max_image_number,
    two_year_transaction_period = two_year_transaction_period,
    line_number = line_number,
    api_key = api_key,
    per_page = 100
  )

  responses[[1]] <- get_openfec(path = paste0("/schedules/schedule_a/"), query_parameters = query_parameters)

  total_pages <- responses[[1]][["parsed"]][["pagination"]][["pages"]]

  total_count <- responses[[1]][['parsed']][["pagination"]][["count"]]

  message("Itemized contributions found: ", total_count)

  if(total_pages > 1){

    message(paste0("There are about ", total_pages, " pages of results to get containing approximately ", total_count, " itemized contributions."))

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

      responses[[i]] <- get_openfec(path = paste0("/schedules/schedule_a/"), query_parameters = query_parameters)

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
      schedule_type_full = map_chr(. , "schedule_type_full", .default = NA),
      increased_limit = map_chr(. , "increased_limit", .default = NA),
      election_type = map_chr(. , "election_type", .default = NA),
      sub_id = map_chr(. , "sub_id", .default = NA),
      report_type = map_chr(. , "report_type", .default = NA),
      file_number = map_chr(. , "file_number", .default = NA),
      conduit_committee_zip = map_chr(. , "conduit_committee_zip", .default = NA),
      image_number = map_chr(. , "image_number", .default = NA),
      back_reference_schedule_name = map_chr(. , "back_reference_schedule_name", .default = NA),
      link_id = map_chr(. , "link_id", .default = NA),
      contributor_id = map_chr(. , "contributor_id", .default = NA),
      candidate_middle_name = map_chr(. , "candidate_middle_name", .default = NA),
      receipt_type_full = map_chr(. , "receipt_type_full", .default = NA),
      candidate_office_district = map_chr(. , "candidate_office_district", .default = NA),
      candidate_name = map_chr(. , "candidate_name", .default = NA),
      line_number = map_chr(. , "line_number", .default = NA),
      original_sub_id = map_chr(. , "original_sub_id", .default = NA),
      committee_name = map_chr(. , "committee_name", .default = NA),
      memoed_subtotal = map_chr(. , "memoed_subtotal", .default = NA),
      contributor_first_name = map_chr(. , "contributor_first_name", .default = NA),
      candidate_office_state_full = map_chr(. , "candidate_office_state_full", .default = NA),
      candidate_id = map_chr(. , "candidate_id", .default = NA),
      back_reference_transaction_id = map_chr(. , "back_reference_transaction_id", .default = NA),
      cycle = map_chr(. , "cycle", .default = NA),
      contributor_middle_name = map_chr(. , "contributor_middle_name", .default = NA),
      candidate_first_name = map_chr(. , "candidate_first_name", .default = NA),
      memo_text = map_chr(. , "memo_text", .default = NA),
      line_number_label = map_chr(. , "line_number_label", .default = NA),
      conduit_committee_id = map_chr(. , "conduit_committee_id", .default = NA),
      candidate_suffix = map_chr(. , "candidate_suffix", .default = NA),
      contributor_state = map_chr(. , "contributor_state", .default = NA),
      is_individual = map_chr(. , "is_individual", .default = NA),
      conduit_committee_street2 = map_chr(. , "conduit_committee_street2", .default = NA),
      report_year = map_chr(. , "report_year", .default = NA),
      contributor_prefix = map_chr(. , "contributor_prefix", .default = NA),
      election_type_full = map_chr(. , "election_type_full", .default = NA),
      candidate_prefix = map_chr(. , "candidate_prefix", .default = NA),
      contributor_city = map_chr(. , "contributor_city", .default = NA),
      amendment_indicator = map_chr(. , "amendment_indicator", .default = NA),
      contributor_occupation = map_chr(. , "contributor_occupation", .default = NA),
      committee = map(. , "committee", .default = NA),
      pdf_url = map_chr(. , "pdf_url", .default = NA),
      candidate_last_name = map_chr(. , "candidate_last_name", .default = NA),
      entity_type = map_chr(. , "entity_type", .default = NA),
      conduit_committee_state = map_chr(. , "conduit_committee_state", .default = NA),
      donor_committee_name = map_chr(. , "donor_committee_name", .default = NA),
      entity_type_desc = map_chr(. , "entity_type_desc", .default = NA),
      memo_code_full = map_chr(. , "memo_code_full", .default = NA),
      contributor = map(. , "contributor", .default = NA),
      fec_election_type_desc = map_chr(. , "fec_election_type_desc", .default = NA),
      candidate_office_full = map_chr(. , "candidate_office_full", .default = NA),
      contributor_name = map_chr(. , "contributor_name", .default = NA),
      load_date = map_chr(. , "load_date", .default = NA),
      contributor_suffix = map_chr(. , "contributor_suffix", .default = NA),
      contributor_street_2 = map_chr(. , "contributor_street_2", .default = NA),
      conduit_committee_street1 = map_chr(. , "conduit_committee_street1", .default = NA),
      national_committee_nonfederal_account = map_chr(. , "national_committee_nonfederal_account", .default = NA),
      unused_contbr_id = map_chr(. , "unused_contbr_id", .default = NA),
      candidate_office = map_chr(. , "candidate_office", .default = NA),
      fec_election_year = map_chr(. , "fec_election_year", .default = NA),
      contribution_receipt_date = map_chr(. , "contribution_receipt_date", .default = NA),
      contributor_aggregate_ytd = map_chr(. , "contributor_aggregate_ytd", .default = NA),
      two_year_transaction_period = map_chr(. , "two_year_transaction_period", .default = NA),
      contributor_employer = map_chr(. , "contributor_employer", .default = NA),
      contribution_receipt_amount = map_dbl(. , "contribution_receipt_amount", .default = NA),
      candidate_office_state = map_chr(. , "candidate_office_state", .default = NA),
      filing_form = map_chr(. , "filing_form", .default = NA),
      amendment_indicator_desc = map_chr(. , "amendment_indicator_desc", .default = NA),
      contributor_zip = map_chr(. , "contributor_zip", .default = NA),
      committee_id = map_chr(. , "committee_id", .default = NA),
      schedule_type = map_chr(. , "schedule_type", .default = NA),
      receipt_type = map_chr(. , "receipt_type", .default = NA),
      conduit_committee_name = map_chr(. , "conduit_committee_name", .default = NA),
      memo_code = map_chr(. , "memo_code", .default = NA),
      receipt_type_desc = map_chr(. , "receipt_type_desc", .default = NA),
      contributor_last_name = map_chr(. , "contributor_last_name", .default = NA),
      conduit_committee_city = map_chr(. , "conduit_committee_city", .default = NA),
      transaction_id = map_chr(. , "transaction_id", .default = NA),
      contributor_street_1 = map_chr(. , "contributor_street_1", .default = NA),
      timestamp = map_chr(. , "timestamp", .default = NA)
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

#' @rdname get_itemized_contributions
get_schedule_a <- get_itemized_contributions
