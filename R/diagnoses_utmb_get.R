#' Pull formatted records of DSM diagnoses in UTMB data
#' @usage df <- diagnoses_utmb_get()
#'
#' @section Source: UTMB Database in IRD035, mostly
#'   UTMB_DATA_ENCOUNTER_PROC_DIAG
#'
#' @note
#'
#' All youth diagnoses made on or after 1/1/2014.
#'
#' Diagnostic list does not include medical diagnoses on form 060, which account
#' for a smaller number of youth mental health diagnoses.

diagnoses_utmb_get<-function(source = "formatted") {
  source<-tolower(source)
  dsm_category_lookup <- import("\\\\tjjd4avresaus1\\public\\hr and analytics\\lookups\\icd10_diagnoses_and_categories.xlsx")
  ird035 <- ird035_connection()

  if (source == "formatted")  {sqlQuery(ird035,
                                        "select tycno as tjjd_number, order_id, diag_id, procedure_date as diag_date, procedure_time as diag_time, diag_description, icd9_code as icd10
                            from utmb_data_encounter_proc_diag
                            inner join utmb_data_patient_id
                            on utmb_data_encounter_proc_diag.patient_id=utmb_data_patient_id.patient_id
                            where procedure_date>='2014-01-01'") %>%
      inner_join(dsm_category_lookup)
  }
  else if (tolower(source) == "sql") {
    sqlQuery(ird035,
             "select *
                            from utmb_data_encounter_proc_diag
                            inner join utmb_data_patient_id
                            on utmb_data_encounter_proc_diag.patient_id=utmb_data_patient_id.patient_id")
  } else {
    "please enter an accepted source: 'formatted', 'sql'"
  }
}
