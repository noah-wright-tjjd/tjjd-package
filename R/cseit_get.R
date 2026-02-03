#' Pull formatted records of highest youth CSE-IT (trafficking risk) level
#' @usage df <- cseit_Get()
#' @section Source: Tables in NSGCMS_RESEARCH, the Noble database.
#'
#' @note
#'
#' A CSE-IT risk score is shaped largely by fixed factors. This query pulls the
#' highest CSE-IT risk score in a youth's record.
#'
#' In other words, if a youth registers as a "clear concern" the first time they
#' are scored and "no concern" the second time, this query will return the value
#' "clear concern".
#'
#' Data filtered on "completed" status.

cseit_get<-function(source = "formatted") {
  source<-tolower(source)
  noble <- noble_connection()

  if (source == "formatted")  {cseit_fields <- sqlQuery(noble,
                                                        "select eid as tjjd_number, form_instance_id, date_completed, score_classification_text as continuum_of_concern
                    from nsg_form_instance_score fis
                      inner join nsg_form_instance fi on
                        (fis.form_instance_id_fk = fi.form_instance_id)
                      inner join nsg_subject su on
                        (fi.subject_id_fk = su.subject_id)
                       where fi.form_id_fk = 136") %>%
    mutate(date_completed=as_date(date_completed),
           continuum_concern=as.character(continuum_of_concern)) %>%
    select(tjjd_number,
           form_instance_id,
           date_completed,
           continuum_concern)

  return(cseit_fields)

  }
  else if (tolower(source) == "sql") {
    sqlQuery(noble,
             "select eid as tjjd_number, form_instance_id, date_completed, score_classification_text as continuum_of_concern
                    from nsg_form_instance_score fis
                      inner join nsg_form_instance fi on
                        (fis.form_instance_id_fk = fi.form_instance_id)
                      inner join nsg_subject su on
                        (fi.subject_id_fk = su.subject_id)
                       where fi.form_id_fk = 136")
    } else {
    "please enter an accepted source: 'formatted', 'sql'"
  }
  }

