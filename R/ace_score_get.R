#' Pull highest ACE score from ACE questions
#' @usage df<-ace_score_get(source, timeframe)
#' @section Source: Tables in NSGCMS_RESEARCH, the Noble database.
#'
#' @note
#'
#' In accordance with agency practice this query pulls the highest value
#' recorded for each ACE question. In other words, if a youth indicates
#' emotional abuse in one interview and no emotional abuse in the next, the
#' "emotional abuse" question will still be counted toward their final score.
#'
#' This pulls direct responses from the ACE questions asked as part of the PACT.
#' These have only recently been implemented, so some older youth are lacking
#' scores. For historical youth, ACE responses extrapolated from other PACT
#' questions can be obtained using the argument source = "pact"
#'
#' For Ace + 10, use the source = "plus ten" to get the ten ACE questions as well
#' as the additional 10 ACE questions that constitute the expanded assessment.

ace_score_get<-function(source = "standard", timeframe = "recent") {

  source <- tolower(source)
  timeframe <- tolower(timeframe)

  noble <- noble_connection()

  forms<-sqlQuery(noble,
                  "select form_id, page_id, section_id, question_id, answer_id, fo.name as form, pa.name as page, se.name as section, qu.sequence as question_sequence, question_text, answer_text
          from nsg_form fo
          inner join nsg_form_page pa on
            (fo.form_id = pa.form_id_fk)
          inner join nsg_form_section se on
            (pa.page_id = se.page_id_fk)
          inner join nsg_form_question qu on
            (se.section_id = qu.section_id_fk)
          inner join nsg_form_answer an on
            (qu.question_id = an.question_id_fk)")

  ace_lookup <-  data.frame(question_sequence = seq.int(0,9),
                            ace_question=c('emotional_abuse',
                                           'physical_abuse',
                                           'sexual_abuse',
                                           'emotional_neglect',
                                           'physical_neglect',
                                           'parents_separated_divorced',
                                           'family_violence',
                                           'household_substance_abuse',
                                           'household_mental_illness',
                                           'incarcerated_household_member'))

  instance_query_base <- "select s.eid as tjjd_number, form_instance_id, fi.date_completed as date_completed, fo.name as form, fll.name as form_admin_type
          from   nsg_form_instance fi
                 inner join nsg_subject s on s.subject_id = fi.subject_id_fk
                 inner join nsg_form fo on fi.form_id_fk = fo.form_id
                 inner join nsg_form_label_lookup fll on fi.form_label_id_fk=fll.form_label_id
                           where fi.status=400"

  if (source == "pact") {

    answers_query <- "select form_instance_id, answer_id_fk as answer_id
from nsg_form_instance_answer_value fiav
  inner join nsg_form_instance fi on
    fiav.form_instance_id_fk = fi.form_instance_id
  inner join nsg_subject s on
    s.subject_id = fi.subject_id_fk
                           where
                           fi.status = 400 and
                           answer_id_fk in (9812, 9814, 10616, 10617, 9799, 10584,
              9964, 9966, 9967, 9968, 9969, 10797, 10799, 10800, 10801, 10802,
              9975, 9977, 10808, 10810,
              9798, 9799, 10582, 10583, 10584, 9807, 10609,
              9978, 10811,
              9812, 9814, 9815, 10616, 10617, 10618, 9970, 9972, 10803, 10805,
              9780, 9782, 9788, 9789, 10595, 10597, 10603, 10604,
              9784, 9791, 10599, 10606,
              9774, 9776, 9777, 9778, 9779, 9849, 9851, 9852, 9853, 9854, 10589, 10591, 10592, 10593, 10594, 10654, 10656, 10657, 10658, 10659)"

  }

  else if (source == "standard")  {

    answers_query <- "select form_instance_id_fk as form_instance_id, question_id_fk as question_id, answer_id_fk as answer_id
          from nsg_form_instance_answer_value
                           where question_id_fk>=3557 and question_id_fk<=3566"
  }

  else if (source == "plus ten") {
    answers_query <- "select form_instance_id_fk as form_instance_id, question_id_fk as question_id, answer_id_fk as answer_id
          from nsg_form_instance_answer_value
                           where question_id_fk>=3557 and question_id_fk<=3576"
  }

  else{
    "please enter an accepted source value: 'standard', 'plus ten', or 'pact'"
  }

 if (timeframe == "recent") {

    instance_query <- str_c(instance_query_base, " and s.eid > 1200000", sep = " ")

  } else if (timeframe == "historical") {

    instance_query <- instance_query_base
  }

  else{
    "please enter an accepted timeframe: 'recent', or 'historical'"
  }

  form_instances <- sqlQuery(noble,
                             instance_query)

  ace_answers <- sqlQuery(noble,
                          answers_query)

  if(source == "pact") {


    parental_status_fields<-sqlQuery(noble,
                                     "select tycno as tjjd_number, mfptr as master_file_pointer_number, pms as parental_status from popmfixed")

    parental_status_lookup<-data.frame(parental_status_code=c('1',
                                                              '2',
                                                              '3',
                                                              '4',
                                                              '5',
                                                              '6',
                                                              '7',
                                                              '?'),
                                       parental_status=c('Never married',
                                                         'Married',
                                                         'Divorced',
                                                         'Separated',
                                                         'Mother deceased',
                                                         'Father deceased',
                                                         'Both parents deceased',
                                                         'Unknown')
    )

    most_recent_parental_status_field<-parental_status_fields %>%
      group_by(tjjd_number) %>%
      summarize(parental_status=last(parental_status)) %>%
      mutate(parental_status=if_else(is.na(parental_status)==true,"?",as.character(parental_status)))

    extrapolated_matched <- form_instances %>%
      left_join(ace_answers) %>%
      left_join(forms) %>%
      mutate(answer_id = replace_na(answer_id, 0))

    extrapolated_ace_scores <- extrapolated_matched %>%
      inner_join(most_recent_parental_status_field) %>%
      filter(tolower(form_admin_type) != "pre-screen") |>
      group_by(tjjd_number, form_instance_id, date_completed, form, form_admin_type) %>%
      summarize(emotional_abuse = max(answer_id %in% c(9812, 9814, 10616, 10617, 9799, 10584)),
                physical_abuse = max(answer_id %in% c(9964, 9966, 9967, 9968, 9969, 10797, 10799, 10800, 10801, 10802)),
                sexual_abuse = max(answer_id %in% c(9975, 9977, 10808, 10810)),
                emotional_neglect = max(answer_id %in% c(9798, 9799, 10582, 10583, 10584, 9807, 10609)),
                physical_neglect = max(answer_id %in% c(9978, 10811)),
                family_violence = max(answer_id %in% c(9812, 9814, 9815, 10616, 10617, 10618, 9970, 9972, 10803, 10805)),
                household_substance_abuse = max(answer_id %in% c(9780, 9782, 9788, 9789, 10595, 10597, 10603, 10604)),
                household_mental_illness = max(answer_id %in% c(9784, 9791, 10599, 10606)),
                parents_separated_divorced = max(if_else(parental_status!=2&parental_status!='?',1,0)),
                incarcerated_household_member = max(answer_id %in% c(9774, 9776, 9777, 9778, 9779, 9849, 9851, 9852, 9853, 9854, 10589, 10591, 10592, 10593, 10594, 10654, 10656, 10657, 10658, 10659))) %>%
      mutate(ace_score = rowSums(across(emotional_abuse:incarcerated_household_member))) |>
      ungroup()

    return(extrapolated_ace_scores)

  }
  else {

    ace_table <- ace_answers %>%
      inner_join(form_instances) %>%
      inner_join(forms) %>%
      inner_join(ace_lookup) %>%
      mutate(answer_numeric = if_else(answer_text == "Yes", 1, 0)) %>%
      group_by(tjjd_number, form_instance_id, date_completed, form, form_admin_type, ace_question) %>%
      summarize(answer = max(answer_numeric)) %>%
      ungroup() %>%
      pivot_wider(names_from = ace_question, values_from = answer, values_fill = 0) %>%
      mutate(ace_score = rowSums(across(emotional_abuse:sexual_abuse)))

    return(ace_table)

  }
}
