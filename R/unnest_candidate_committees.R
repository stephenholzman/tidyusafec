#' Unnest Candidate Committees
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
unnest_candidate_committees <- function(data){

  data <- data %>%
    #We want to unnest the principal committees so we have a row for every canidate-committee pair, but keep some other lists preserved.
    tidyr::unnest(principal_committees, .preserve = c("election_years", "cycles", "election_districts")) %>%
    mutate(committee_id = map_chr(principal_committees, function(x) x$committee_id),
           committee_name = map_chr(principal_committees, function(x) x$name),
           treasurer_name = map(principal_committees, function(x) x$treasurer_name),
           earliest_cycle = map_int(principal_committees, function(x) x$cycles %>% unlist() %>% min()),
           latest_cycle = map_int(principal_committees, function(x) x$cycles %>% unlist() %>% max()),
           earliest_election_year = map_int(election_years, function(x) x %>% unlist() %>% min()),
           latest_election_year = map_int(election_years, function(x) x %>% unlist() %>% max()),
           committee_first_file_date = map_chr(principal_committees, function(x) x$first_file_date),
           committee_last_file_date = map_chr(principal_committees, function(x) x$last_file_date)
    )

  message("While Unnesting: \n\tTotal Candidates: ",length(levels(as.factor(data$candidate_id))),"\n\tTotal Principal Committees: ",length(levels(as.factor(data$committee_id))),"\n\tNumber of rows: ",nrow(data))

  return(data)

}
