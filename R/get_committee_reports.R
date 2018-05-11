#' Get Committee Reports
#'
#' Each report represents the summary information from FEC Form 3, Form 3X and Form 3P. These reports have key statistics that illuminate the financial status of a given committee. Things like cash on hand, debts owed by committee, total receipts, and total disbursements are especially helpful for understanding a committee's financial dealings.
#'
#' By default, this endpoint includes both amended and final versions of each report. To restrict to only the final versions of each report, use is_amended=false; to view only reports that have been amended, use is_amended=true.
#' Several different reporting structures exist, depending on the type of organization that submits financial information. To see an example of these reporting requirements, look at the summary and detailed summary pages of FEC Form 3, Form 3X, and Form 3P.
#'
#' @param data A dataframe or tibble. Usually this will be the returned result of search_candidates(). If a column is called 'committee_id', get_itemized_contributions() will return results for all IDs in that column and will attempt to join data to the result by committee_id.
#' @param api_key api_key API key for https://api.data.gov. Get one at https://api.data.gov/signup.
#' @param data_structure A character describing how you want OpenFEC results. Options are 'tidy', 'list', or 'both.' Choose list for raw results.
#' @param committee_id A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
#' @param sort_null_only Toggle that filters out all rows having sort column that is non-null
#' @param min_cash_on_hand_end_period_amount Filter for all amounts greater than a value.
#' @param beginning_image_number Unique identifier for the electronic or paper report. This number is used to construct PDF URLs to the original document.
#' @param report_type Report type; prefix with "-" to exclude. Name of report where the underlying data comes from:
#'
#'- 10D Pre-Election
#'- 10G Pre-General
#'- 10P Pre-Primary
#'- 10R Pre-Run-Off
#'- 10S Pre-Special
#'- 12C Pre-Convention
#'- 12G Pre-General
#'- 12P Pre-Primary
#'- 12R Pre-Run-Off
#'- 12S Pre-Special
#'- 30D Post-Election
#'- 30G Post-General
#'- 30P Post-Primary
#'- 30R Post-Run-Off
#'- 30S Post-Special
#'- 60D Post-Convention
#'- M1  January Monthly
#'- M10 October Monthly
#'- M11 November Monthly
#'- M12 December Monthly
#'- M2  February Monthly
#'- M3  March Monthly
#'- M4  April Monthly
#'- M5  May Monthly
#'- M6  June Monthly
#'- M7  July Monthly
#'- M8  August Monthly
#'- M9  September Monthly
#'- MY  Mid-Year Report
#'- Q1  April Quarterly
#'- Q2  July Quarterly
#'- Q3  October Quarterly
#'- TER Termination Report
#'- YE  Year-End
#'- 90S Post Inaugural
#'Supplement
#'- 90D Post Inaugural
#'- 48  48 Hour Notification
#'- 24  24 Hour Notification
#'- M7S July Monthly/
#'  Semi-Annual
#'- MSA Monthly Semi-Annual
#'(MY)
#'- MYS Monthly Year End/
#'  Semi-Annual
#'- Q2S July Quarterly/
#'  Semi-Annual
#'- QSA Quarterly Semi-Annual
#'(MY)
#'- QYS Quarterly Year End/
#'  Semi-Annual
#'- QYE Quarterly Semi-Annual
#'(YE)
#'- QMS Quarterly Mid-Year/
#'  Semi-Annual
#'- MSY Monthly Semi-Annual
#'(YE)
#' @param year Year that the record applies to. Sometimes records are amended in subsequent years so this can differ from underlying form's receipt date.
#' @param sort_hide_null Hide null values on sorted column(s).
#' @param max_cash_on_hand_end_period_amount Filter for all amounts less than a value.
#' @param min_independent_expenditures Filter for all amounts greater than a value.
#' @param min_debts_owed_amount Filter for all amounts greater than a value.
#' @param max_total_contributions Filter for all amounts less than a value.
#' @param type The one-letter type code of the organization: - C communication cost
#'- D delegate
#'- E electioneering communication
#'- H House
#'- I independent expenditor (person or group)
#'- N PAC - nonqualified
#'- O independent expenditure-only (super PACs)
#'- P presidential
#'- Q PAC - qualified
#'- S Senate
#'- U single candidate independent expenditure
#'- V PAC with non-contribution account, nonqualified
#'- W PAC with non-contribution account, qualified
#'- X party, nonqualified
#'- Y party, qualified
#'- Z national party non-federal account
#' @param is_amended Report has been amended
#' @param max_independent_expenditures Filter for all amounts less than a value.
#' @param cycle Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
#' @param candidate_id A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
#' @param min_disbursements_amount Filter for all amounts greater than a value.
#' @param min_receipts_amount Filter for all amounts greater than a value.
#' @param max_receipts_amount Filter for all amounts less than a value.
#' @param sort Provide a field to sort by. Use - for descending order.
#' @param max_disbursements_amount Filter for all amounts less than a value.
#' @param min_party_coordinated_expenditures Filter for all amounts greater than a value.
#' @param max_debts_owed_expenditures Filter for all amounts less than a value.
#' @param max_party_coordinated_expenditures Filter for all amounts less than a value.
#' @param min_total_contributions Filter for all amounts greater than a value.
#'
#' @return
#' @export
#'
#' @examples
get_committee_reports <- function(
  data = NULL,
  api_key = Sys.getenv("DATAGOV_API_KEY"),
  data_structure = 'tidy',
  committee_id = NULL,
  sort_null_only = NULL,
  min_cash_on_hand_end_period_amount = NULL,
  beginning_image_number = NULL,
  report_type = NULL,
  year = NULL,
  sort_hide_null = NULL,
  max_cash_on_hand_end_period_amount = NULL,
  min_independent_expenditures = NULL,
  min_debts_owed_amount = NULL,
  max_total_contributions = NULL,
  type = NULL,
  is_amended = NULL,
  max_independent_expenditures = NULL,
  cycle = NULL,
  candidate_id = NULL,
  min_disbursements_amount = NULL,
  min_receipts_amount = NULL,
  max_receipts_amount = NULL,
  sort = NULL,
  max_disbursements_amount = NULL,
  min_party_coordinated_expenditures = NULL,
  max_debts_owed_expenditures = NULL,
  max_party_coordinated_expenditures = NULL,
  min_total_contributions = NULL
){

  if (is.null(api_key)) {

    stop('An API key is required. Obtain one at https://api.data.gov/signup.')

  }

  #If committee_id is set explicitly, go with that. If not check for a committee_id column in the data argument. If neither exists, stop the presses because it's required.
  if (is.null(committee_id)) {

    if('committee_id' %in% names(data)){

      committee_id <- data[['committee_id']]

    }else{

      stop('Argument for committee_id or a committee_id column in the data argument is required')

    }

  }

  committee_id <- committee_id %>%
    unique()

  responses <- list()

  #Because this endpoint depends on a candidate ID, we have to make requests separately for each one.
  #
  for(c_id in committee_id){

    Sys.sleep(.5) #With an upgraded key, max limit is 120 calls per minute. May be giving up a little performance for now in favor of being nice to FEC servers.
    message("Getting totals for: ", c_id)

    query_parameters <- list(
      committee_id = committee_id,
      sort_null_only = sort_null_only,
      min_cash_on_hand_end_period_amount = min_cash_on_hand_end_period_amount,
      beginning_image_number = beginning_image_number,
      report_type = report_type,
      year = year,
      sort_hide_null = sort_hide_null,
      max_cash_on_hand_end_period_amount = max_cash_on_hand_end_period_amount,
      min_independent_expenditures = min_independent_expenditures,
      min_debts_owed_amount = min_debts_owed_amount,
      max_total_contributions = max_total_contributions,
      type = type,
      is_amended = is_amended,
      max_independent_expenditures = max_independent_expenditures,
      cycle = cycle,
      candidate_id = candidate_id,
      min_disbursements_amount = min_disbursements_amount,
      min_receipts_amount = min_receipts_amount,
      max_receipts_amount = max_receipts_amount,
      sort = sort,
      max_disbursements_amount = max_disbursements_amount,
      min_party_coordinated_expenditures = min_party_coordinated_expenditures,
      max_debts_owed_expenditures = max_debts_owed_expenditures,
      max_party_coordinated_expenditures = max_party_coordinated_expenditures,
      min_total_contributions = min_total_contributions,
      api_key = api_key,
      page = 1,
      per_page = 100
    )

    query_parameters <- query_parameters[!sapply(query_parameters, is.null)]

    responses[[c_id]][[1]] <- get_openfec(path = paste0("/committee/", c_id, "/reports/"), query_parameters = query_parameters)

    #This is almost definitely just a one page query, but just in case.
    total_pages <- responses[[c_id]][[1]][["parsed"]][["pagination"]][["pages"]]

    total_count <- responses[[c_id]][[1]][["parsed"]][["pagination"]][["count"]]

    if(total_pages > 1){

      for(i in 2:total_pages){

        query_parameters$page <- i

        responses[[c_id]][[i]] <- get_openfec(path = paste0("/committee/",c_id, "/reports/"), query = query_parameters)
        print(i)

      }

    }

  }
  #
  tidy_reports <- responses %>%
    unlist(recursive = F) %>%
    purrr::map(function(x) x$parsed$results) %>%
    unname() %>%
    unlist(recursive = F) %>%
    tibble(
      amendment_chain = map(. , "amendment_chain", .default = NA),
      transfers_from_other_authorized_committee_period = map_dbl(. , "transfers_from_other_authorized_committee_period", .default = NA),
      candidate_contribution_period = map_dbl(. , "candidate_contribution_period", .default = NA),
      net_operating_expenditures_ytd = map_dbl(. , "net_operating_expenditures_ytd", .default = NA),
      total_contribution_refunds_period = map_dbl(. , "total_contribution_refunds_period", .default = NA),
      receipt_date = map_chr(. , "receipt_date", .default = NA),
      total_loans_received_ytd = map_dbl(. , "total_loans_received_ytd", .default = NA),
      loan_repayments_candidate_loans_ytd = map_dbl(. , "loan_repayments_candidate_loans_ytd", .default = NA),
      amendment_indicator_full = map_chr(. , "amendment_indicator_full", .default = NA),
      total_individual_contributions_period = map_dbl(. , "total_individual_contributions_period", .default = NA),
      committee_name = map_chr(. , "committee_name", .default = NA),
      total_operating_expenditures_ytd = map_dbl(. , "total_operating_expenditures_ytd", .default = NA),
      pdf_url = map_chr(. , "pdf_url", .default = NA),
      debts_owed_to_committee = map_dbl(. , "debts_owed_to_committee", .default = NA),
      report_type = map_chr(. , "report_type", .default = NA),
      refunded_individual_contributions_ytd = map_dbl(. , "refunded_individual_contributions_ytd", .default = NA),
      individual_unitemized_contributions_ytd = map_dbl(. , "individual_unitemized_contributions_ytd", .default = NA),
      individual_itemized_contributions_ytd = map_dbl(. , "individual_itemized_contributions_ytd", .default = NA),
      total_contributions_ytd = map_dbl(. , "total_contributions_ytd", .default = NA),
      loans_made_by_candidate_ytd = map_dbl(. , "loans_made_by_candidate_ytd", .default = NA),
      aggregate_contributions_personal_funds_primary = map_dbl(. , "aggregate_contributions_personal_funds_primary", .default = NA),
      transfers_from_other_authorized_committee_ytd = map_dbl(. , "transfers_from_other_authorized_committee_ytd", .default = NA),
      total_contribution_refunds_ytd = map_dbl(. , "total_contribution_refunds_ytd", .default = NA),
      transfers_to_other_authorized_committee_ytd = map_dbl(. , "transfers_to_other_authorized_committee_ytd", .default = NA),
      political_party_committee_contributions_period = map_dbl(. , "political_party_committee_contributions_period", .default = NA),
      file_number = map_chr(. , "file_number", .default = NA),
      coverage_end_date = map_chr(. , "coverage_end_date", .default = NA),
      individual_unitemized_contributions_period = map_dbl(. , "individual_unitemized_contributions_period", .default = NA),
      fec_url = map_chr(. , "fec_url", .default = NA),
      loan_repayments_candidate_loans_period = map_dbl(. , "loan_repayments_candidate_loans_period", .default = NA),
      gross_receipt_authorized_committee_primary = map_dbl(. , "gross_receipt_authorized_committee_primary", .default = NA),
      total_loan_repayments_made_ytd = map_dbl(. , "total_loan_repayments_made_ytd", .default = NA),
      means_filed = map_chr(. , "means_filed", .default = NA),
      total_contributions_column_total_period = map_dbl(. , "total_contributions_column_total_period", .default = NA),
      total_disbursements_ytd = map_dbl(. , "total_disbursements_ytd", .default = NA),
      report_type_full = map_chr(. , "report_type_full", .default = NA),
      operating_expenditures_period = map_dbl(. , "operating_expenditures_period", .default = NA),
      fec_file_id = map_chr(. , "fec_file_id", .default = NA),
      total_contributions_period = map_dbl(. , "total_contributions_period", .default = NA),
      report_form = map_chr(. , "report_form", .default = NA),
      report_year = map_chr(. , "report_year", .default = NA),
      aggregate_amount_personal_contributions_general = map_dbl(. , "aggregate_amount_personal_contributions_general", .default = NA),
      most_recent = map_chr(. , "most_recent", .default = NA),
      csv_url = map_chr(. , "csv_url", .default = NA),
      all_other_loans_period = map_dbl(. , "all_other_loans_period", .default = NA),
      total_loans_received_period = map_dbl(. , "total_loans_received_period", .default = NA),
      total_receipts_ytd = map_dbl(. , "total_receipts_ytd", .default = NA),
      loan_repayments_other_loans_ytd = map_dbl(. , "loan_repayments_other_loans_ytd", .default = NA),
      net_contributions_ytd = map_dbl(. , "net_contributions_ytd", .default = NA),
      total_individual_contributions_ytd = map_dbl(. , "total_individual_contributions_ytd", .default = NA),
      cash_on_hand_beginning_period = map_dbl(. , "cash_on_hand_beginning_period", .default = NA),
      document_description = map_chr(. , "document_description", .default = NA),
      loan_repayments_other_loans_period = map_dbl(. , "loan_repayments_other_loans_period", .default = NA),
      individual_itemized_contributions_period = map_dbl(. , "individual_itemized_contributions_period", .default = NA),
      committee_id = map_chr(. , "committee_id", .default = NA),
      beginning_image_number = map_chr(. , "beginning_image_number", .default = NA),
      gross_receipt_authorized_committee_general = map_dbl(. , "gross_receipt_authorized_committee_general", .default = NA),
      net_contributions_period = map_dbl(. , "net_contributions_period", .default = NA),
      offsets_to_operating_expenditures_ytd = map_dbl(. , "offsets_to_operating_expenditures_ytd", .default = NA),
      cycle = map_chr(. , "cycle", .default = NA),
      most_recent_file_number = map_chr(. , "most_recent_file_number", .default = NA),
      total_receipts_period = map_dbl(. , "total_receipts_period", .default = NA),
      refunded_political_party_committee_contributions_period = map_dbl(. , "refunded_political_party_committee_contributions_period", .default = NA),
      candidate_contribution_ytd = map_dbl(. , "candidate_contribution_ytd", .default = NA),
      total_offsets_to_operating_expenditures_period = map_dbl(. , "total_offsets_to_operating_expenditures_period", .default = NA),
      is_amended = map_chr(. , "is_amended", .default = NA),
      debts_owed_by_committee = map_dbl(. , "debts_owed_by_committee", .default = NA),
      other_receipts_ytd = map_dbl(. , "other_receipts_ytd", .default = NA),
      previous_file_number = map_chr(. , "previous_file_number", .default = NA),
      refunded_individual_contributions_period = map_dbl(. , "refunded_individual_contributions_period", .default = NA),
      all_other_loans_ytd = map_dbl(. , "all_other_loans_ytd", .default = NA),
      refunds_total_contributions_col_total_ytd = map_dbl(. , "refunds_total_contributions_col_total_ytd", .default = NA),
      gross_receipt_minus_personal_contributions_primary = map_dbl(. , "gross_receipt_minus_personal_contributions_primary", .default = NA),
      amendment_indicator = map_chr(. , "amendment_indicator", .default = NA),
      total_contribution_refunds_col_total_period = map_dbl(. , "total_contribution_refunds_col_total_period", .default = NA),
      offsets_to_operating_expenditures_period = map_dbl(. , "offsets_to_operating_expenditures_period", .default = NA),
      coverage_start_date = map_chr(. , "coverage_start_date", .default = NA),
      committee_type = map_chr(. , "committee_type", .default = NA),
      refunded_other_political_committee_contributions_ytd = map_dbl(. , "refunded_other_political_committee_contributions_ytd", .default = NA),
      total_operating_expenditures_period = map_dbl(. , "total_operating_expenditures_period", .default = NA),
      total_disbursements_period = map_dbl(. , "total_disbursements_period", .default = NA),
      total_loan_repayments_made_period = map_dbl(. , "total_loan_repayments_made_period", .default = NA),
      operating_expenditures_ytd = map_dbl(. , "operating_expenditures_ytd", .default = NA),
      cash_on_hand_end_period = map_dbl(. , "cash_on_hand_end_period", .default = NA),
      total_offsets_to_operating_expenditures_ytd = map_dbl(. , "total_offsets_to_operating_expenditures_ytd", .default = NA),
      other_political_committee_contributions_ytd = map_dbl(. , "other_political_committee_contributions_ytd", .default = NA),
      html_url = map_chr(. , "html_url", .default = NA),
      refunded_political_party_committee_contributions_ytd = map_dbl(. , "refunded_political_party_committee_contributions_ytd", .default = NA),
      gross_receipt_minus_personal_contribution_general = map_dbl(. , "gross_receipt_minus_personal_contribution_general", .default = NA),
      transfers_to_other_authorized_committee_period = map_dbl(. , "transfers_to_other_authorized_committee_period", .default = NA),
      other_receipts_period = map_dbl(. , "other_receipts_period", .default = NA),
      loans_made_by_candidate_period = map_dbl(. , "loans_made_by_candidate_period", .default = NA),
      refunded_other_political_committee_contributions_period = map_dbl(. , "refunded_other_political_committee_contributions_period", .default = NA),
      end_image_number = map_chr(. , "end_image_number", .default = NA),
      subtotal_period = map_dbl(. , "subtotal_period", .default = NA),
      other_disbursements_ytd = map_dbl(. , "other_disbursements_ytd", .default = NA),
      political_party_committee_contributions_ytd = map_dbl(. , "political_party_committee_contributions_ytd", .default = NA),
      other_disbursements_period = map_dbl(. , "other_disbursements_period", .default = NA),
      net_operating_expenditures_period = map_dbl(. , "net_operating_expenditures_period", .default = NA),
      other_political_committee_contributions_period = map_dbl(. , "other_political_committee_contributions_period", .default = NA)
    ) %>%
    gather(key = "type_of_funds", value = "amount",
           transfers_from_other_authorized_committee_period,
           candidate_contribution_period,
           net_operating_expenditures_ytd,
           total_contribution_refunds_period,
           total_loans_received_ytd,
           loan_repayments_candidate_loans_ytd,
           total_individual_contributions_period,
           total_operating_expenditures_ytd,
           debts_owed_to_committee,
           refunded_individual_contributions_ytd,
           individual_unitemized_contributions_ytd,
           individual_itemized_contributions_ytd,
           total_contributions_ytd,
           loans_made_by_candidate_ytd,
           aggregate_contributions_personal_funds_primary,
           transfers_from_other_authorized_committee_ytd,
           total_contribution_refunds_ytd,
           transfers_to_other_authorized_committee_ytd,
           political_party_committee_contributions_period,
           individual_unitemized_contributions_period,
           loan_repayments_candidate_loans_period,
           gross_receipt_authorized_committee_primary,
           total_loan_repayments_made_ytd,
           total_contributions_column_total_period,
           total_disbursements_ytd,
           operating_expenditures_period,
           total_contributions_period,
           aggregate_amount_personal_contributions_general,
           all_other_loans_period,
           total_loans_received_period,
           total_receipts_ytd,
           loan_repayments_other_loans_ytd,
           net_contributions_ytd,
           total_individual_contributions_ytd,
           cash_on_hand_beginning_period,
           loan_repayments_other_loans_period,
           individual_itemized_contributions_period,
           gross_receipt_authorized_committee_general,
           net_contributions_period,
           offsets_to_operating_expenditures_ytd,
           total_receipts_period,
           refunded_political_party_committee_contributions_period,
           candidate_contribution_ytd,
           total_offsets_to_operating_expenditures_period,
           debts_owed_by_committee,
           other_receipts_ytd,
           refunded_individual_contributions_period,
           all_other_loans_ytd,
           refunds_total_contributions_col_total_ytd,
           gross_receipt_minus_personal_contributions_primary,
           total_contribution_refunds_col_total_period,
           offsets_to_operating_expenditures_period,
           refunded_other_political_committee_contributions_ytd,
           total_operating_expenditures_period,
           total_disbursements_period,
           total_loan_repayments_made_period,
           operating_expenditures_ytd,
           cash_on_hand_end_period,
           total_offsets_to_operating_expenditures_ytd,
           other_political_committee_contributions_ytd,
           refunded_political_party_committee_contributions_ytd,
           gross_receipt_minus_personal_contribution_general,
           transfers_to_other_authorized_committee_period,
           other_receipts_period,
           loans_made_by_candidate_period,
           refunded_other_political_committee_contributions_period,
           subtotal_period,
           other_disbursements_ytd,
           political_party_committee_contributions_ytd,
           other_disbursements_period,
           net_operating_expenditures_period,
           other_political_committee_contributions_period)

  if("committee_id" %in% names(data)){
    tidy_reports <- left_join(tidy_reports, data, by = c("committee_id" = "committee_id"))
  }

  object_to_return <- list(
    tidy = tidy_reports,
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
