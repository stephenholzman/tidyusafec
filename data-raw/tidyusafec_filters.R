tidyusafec_filters <- list(
  candidate_totals = list(
    type_of_funds = list(
      top_level = c(
        "receipts",
        "disbursements",
        "last_cash_on_hand_end_period"
        ),
      receipts_smallest_components = c(
        "individual_itemized_contributions",
        "individual_unitemized_contributions",
        "political_party_committee_contributions",
        "other_political_committee_contributions",
        "candidate_contribution",
        "transfers_from_other_authorized_committee",
        "loans_made_by_candidate",
        "all_other_loans",
        "offsets_to_operating_expenditures",
        "other_receipts"
        ),
      disbursements_smallest_components = c(
        "operating_expenditures",
        "loan_repayment_other_loans",
        "loan_repayment_candidate_loans",
        "contribution_refunds",
        "refunded_other_political_committee_contributions",
        "refunded_political_party_committee_contributions",
        "other_disbursements",
        "transfers_to_other_authorized_committee"
      )
    )
  )
)

usethis::use_data(tidyusafec_filters, overwrite = TRUE)
# loan_repayments,
# last_net_operating_expenditures,
# transfers_from_other_authorized_committee,
# receipts, offsets_to_fundraising_expenditures,
# other_receipts,
# candidate_contribution,
# individual_unitemized_contributions,
# operating_expenditures,
# individual_contributions,
# fundraising_disbursements,
# political_party_committee_contributions,
# exempt_legal_accounting_disbursement,
# loan_repayments_candidate_loans,
# offsets_to_legal_accounting,
# last_debts_owed_to_committee,
# transfers_to_other_authorized_committee,
# refunded_other_political_committee_contributions,
# last_debts_owed_by_committee,
# net_contributions,
# refunded_political_party_committee_contributions,
# federal_funds,
# other_disbursements,
# last_net_contributions,
# offsets_to_operating_expenditures,
# other_political_committee_contributions,
# individual_itemized_contributions,
# loans,
# last_cash_on_hand_end_period,
# loan_repayments_other_loans,
# loans_made_by_candidate,
# net_operating_expenditures,
# contributions,
# contribution_refunds,
# total_offsets_to_operating_expenditures,
# disbursements,
# all_other_loans
