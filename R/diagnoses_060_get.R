#' Pull formatted records of DSM diagnoses on CCF060D
#' @usage df<-diagnoses_060_Get()
#'
#' @section Source: Reporting data warehouse copy of Psychological Services Manager tables
#'
#' @note
#'
#' Diagnostic list does not include UTMB medical diagnoses, which account for
#' majority of youth mental health diagnoses

diagnoses_060_get<-function(source = "formatted") {

  warehouse <- warehouse_connection()

  source<-tolower(source)

  idtable <- sqlQuery(warehouse,
                      "select tjjdnumber as tjjd_number, diagid, reviewdate, entrydatetime from psm.tccf060d")

  jointable <- sqlQuery(warehouse,
                        "select * from psm.tccf060d_dsm5list
inner join psm.tccf060d_dsmstatus on psm.tccf060d_dsm5list.statid=psm.tccf060d_dsmstatus.statid
inner join psm.tdsm5typeseverity on psm.tccf060d_dsm5list.dsmtypesevid=psm.tdsm5typeseverity.dsmtypesevid
inner join psm.tdsm5_code on psm.tdsm5typeseverity.dsmid=psm.tdsm5_code.dsmid
left join psm.tdsm5_severity on psm.tdsm5typeseverity.sevid=psm.tdsm5_severity.sevid") |>
    select(-Archived)

  colnames(jointable) <- tolower(colnames(jointable))

  combinedtable <- idtable |>
    inner_join(jointable)

  if (source == "formatted")  {

combinedtableformat <- combinedtable %>%
      mutate(all_archive = archived + archived.1 + archived.2,
             review_date = as_date(reviewdate)) %>%
      filter(all_archive == 0) %>%
      select(tjjd_number,
             dsm_diagnoses_id = dsmlistid,
             form060_uid = diagid,
             review_date,
             entry_date = entrydatetime,
             icd10,
             description,
             severity,
             status = statusdescription)

return(combinedtableformat)

  }
  else if (tolower(source) == "sql") {

    return(combinedtable)

  } else {
    "please enter an accepted source: 'formatted', 'sql'"
  }
}
