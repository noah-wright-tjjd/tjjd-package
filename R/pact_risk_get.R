#' Pull formatted records of PACT and RPACT risk history for youth
#' @usage df <- pact_risk_get()
#' @section Source: Noble tables from "NSGCMS_Research".
#'
#' @note
#'
#' This only pulls the overall PACT risk without any breakdown of risk factors.

pact_risk_get<-function(source = "formatted") {
  source<-tolower(source)
  noble <- noble_connection()

  if (source == "formatted")  {sqlQuery(noble,
                                        "
  select s.eid as tjjd_number, form_instance_id, date_completed as date_completed, f.name as form, fll.name as form_admin_type, score_classification_text as score
          from   nsg_form_instance fi
                 left join nsg_form_instance_score fis
                         on ( fis.form_instance_id_fk = fi.form_instance_id )
                 left join nsg_form_scoring_form_score_target fst
                         on ( fis.form_score_target_id_fk = fst.form_score_target_id )
                 inner join nsg_subject s
                         on ( s.subject_id = fi.subject_id_fk )
                 inner join nsg_user u
                         on ( u.user_id = fi.completed_by_user_id_fk )
                 inner join nsg_form f
                         on ( fi.form_id_fk = f.form_id )
                 left outer join nsg_form_label_lookup fll
                              on ( fi.form_label_id_fk = fll.form_label_id )
                              where (form_id=121 or form_id=125)
                              and (fst.name='risk level' or fst.name is null)

")
  }
  else if (tolower(source) == "sql") {
    sqlQuery(noble,
             "select *
          from   [nsg_form_instance_score] fis
                 inner join nsg_form_instance fi
                         on ( fis.form_instance_id_fk = fi.form_instance_id )
                 inner join nsg_form_scoring_form_score_target fst
                         on ( fis.form_score_target_id_fk = fst.form_score_target_id )
                 inner join nsg_subject s
                         on ( s.subject_id = fi.subject_id_fk )
                 inner join nsg_user u
                         on ( u.user_id = fi.completed_by_user_id_fk )
                 inner join nsg_form f
                         on ( fi.form_id_fk = f.form_id )
                 left outer join nsg_form_label_lookup fll
                              on ( fi.form_label_id_fk = fll.form_label_id )
                              where (form_id=121 or form_id=125)
                              and fst.name='risk level'")
  } else {
    "please enter an accepted source: 'formatted', 'sql'"
  }
}
