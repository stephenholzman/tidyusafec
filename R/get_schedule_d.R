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

#' @rdname get_committee_debts
#'
get_schedule_d <- get_committee_debts
