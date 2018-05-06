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

#' @rdname get_independent_expenditures
#'
get_schedule_e <- get_independent_expenditures
