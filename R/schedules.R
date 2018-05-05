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
  data,
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

}

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
  data,
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

}

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
get_committee_debts <- function(
  data,
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

}

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
  data,
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

}

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
  data,
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

}
