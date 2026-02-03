#' Pull formatted records of PACT and RPACT risk history for youth
#' @usage df <- pact_risk_factor_get()
#' @section Source: Noble tables from "NSGCMS_Research".
#'
#' @note
#'
#' This only pulls risk factors, ranks, and their percent of maximum.
#'
pact_risk_factor_get<-function(source = "formatted") {
  source<-tolower(source)
  if (source == "formatted")  {
    noble <- noble_connection()
    ###this is the query in sql, but it's much slower for some reason:

    #sqlQuery(noble,
    #"select subject_id,
    #form_instance_id,
    #           factor_score_id,
    #           ffi.score_group_id_fk as score_group_id,
    #           eid as tjjd_number,
    #           fo.name as form_name,
    #           date_assigned,
    #           date_completed,
    #           fa.name as factor_category,
    #           ft.display_name as factor_type,
    #           percent_of_max as percent_of_max_factor_score,
    #           rank
    #
    #           from nsg_subject_cp_factor_score_group_form_instance ffi
    #           inner join nsg_form_instance fi on fi.form_instance_id = ffi.form_instance_id_fk
    #           inner join nsg_form fo on fo.form_id = fi.form_id_fk
    #           inner join nsg_subject_cp_factor_score fs on ffi.score_group_id_fk = fs.score_group_id_fk
    #           inner join nsg_cp_factor fa on fs.factor_id_fk = fa.factor_id
    #           inner join nsg_cp_factor_type_lookup ft on fa.factor_type_id_fk=ft.factor_type_id
    #           inner join nsg_subject su on su.subject_id = fi.subject_id_fk")

    form_instance<-sqlQuery(noble, "select subject_id_fk as subject_id, form_id_fk as form_id, form_instance_id, created_date, modified_date
                       from nsg_form_instance")

    subject_information<-sqlQuery(noble, "select subject_id, eid as tjjd_number
                             from nsg_subject")
    form_lookup<-sqlQuery(noble, "select form_id, name as form_name
                     from nsg_form")
    factor_score_form_instance<-sqlQuery(noble, "select score_group_form_instance_id, score_group_id_fk as score_group_id, form_instance_id_fk as form_instance_id
                     from nsg_subject_cp_factor_score_group_form_instance")
    factor_score<-sqlQuery(noble, "select factor_score_id, score_group_id_fk as score_group_id, factor_id_fk as factor_id, subject_id_fk as subject_id, percent_of_max as percent_of_max_factor_score, rank
                     from nsg_subject_cp_factor_score")
    factor_lookup<-sqlQuery(noble, "select factor_id, factor_type_id_fk as factor_type_id, factor_category_id_fk as factor_category_id, name as factor_category from
          nsg_cp_factor")
    factor_category_lookup<-sqlQuery(noble, "select factor_type_id, display_name as factor_type from
          nsg_cp_factor_type_lookup")

    return(form_instance %>%
             inner_join(subject_information) %>%
             inner_join(form_lookup) %>%
             inner_join(factor_score_form_instance) %>%
             inner_join(factor_score)  %>%
             inner_join(factor_lookup) %>%
             inner_join(factor_category_lookup) |>
             group_by(form_instance_id) |>
             filter(score_group_id == max(score_group_id)) |>
             ungroup() |>
             select(-form_id, -factor_type_id,-factor_category_id,-factor_id))
  }
  else if (tolower(source) == "sql") {
    sqlQuery(noble,
             "select * from nsg_subject_cp_factor_score_group_form_instance ffi
  inner join nsg_form_instance fi on fi.form_instance_id = ffi.form_instance_id_fk
  inner join nsg_form fo on fo.form_id = fi.form_id_fk
  inner join nsg_subject_cp_factor_score fs on ffi.score_group_id_fk = fs.score_group_id_fk
  inner join nsg_cp_factor fa on fs.factor_id_fk = fa.factor_id
  inner join nsg_cp_factor_type_lookup ft on fa.factor_type_id_fk=ft.factor_type_id
  inner join nsg_subject su on su.subject_id = fi.subject_id_fk")
  } else {
    "please enter an accepted source: 'formatted', 'sql'"
  }
}
