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

#' @rdname get_committee_loans
#'
get_schedule_c <- get_committee_loans
