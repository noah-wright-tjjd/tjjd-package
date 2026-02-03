#' Pull formatted records of pharmacy orders in UTMB data
#' @usage df <- pharmacy_orders_get()
#'
#' @section Source: UTMB Database in IRD035, mostly
#'   UTMB_DATA_PHARMACY_ORDER_DETAIL
#'
#' @note
#'
#' Highly truncated (original is 200+ columns) query of drugs prescribed to
#' youth.
#'
#' Includes all youth prescriptions in which the first dosage date is on or
#' after 1/1/2014.
#'
#' The "START_REASON" column is blank for most entries because it only applies
#' to non-formulary doses.

pharmacy_orders_get<-function(source = "formatted") {

  source<-tolower(source)

  ahfs_drug_class_lookup <- import("\\\\tjjd4avresaus1\\public\\hr and analytics\\lookups\\therapeutic_class_lookup.xlsx")

  ird035 <- ird035_connection()

  if (source == "formatted")  {sqlQuery(ird035,
                                        "select tycno as tjjd_number, order_id, detail_id, brand_name, label_name, gnn as generic_name, first_dose_date_time, stop_date_time, start_reason, stop_reason, therapeutic_class
                                 from utmb_data_pharmacy_order_detail
                                 inner join utmb_data_patient_id
                                 on utmb_data_pharmacy_order_detail.patient_id=utmb_data_patient_id.patient_id
                                 where first_dose_date_time>='2014-01-01'") %>%
      left_join(ahfs_drug_class_lookup)
  }
  else if (tolower(source) == "sql") {
    sqlQuery(ird035,
             "select *
                            from utmb_data_pharmacy_order_detail
                            inner join utmb_data_patient_id
                            on utmb_data_encounter_proc_diag.patient_id=utmb_data_patient_id.patient_id")
  } else {
    "please enter an accepted source: 'formatted', 'sql'"
  }
}
