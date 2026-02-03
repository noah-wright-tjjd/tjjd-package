#'Converts formatted CAPPS job history to a summary of all spells of employment
#'(hire and termination) at TJJD.
#'
#'@usage df <- capps_employment_spells()
#'
#'@section This query converts the reformatted CAPPS job history into a format
#'  with a unique value for each spell of employment (hire and termination).
#'
#'  For employees who began before HR data begins (2011) the 2011 conversion date
#'  is used as their default start date.
#'
#'@note
#'
#'UID is Empl_ID combined with Hire Month.
#'
#'For name, title, location, position ID, and supervisor position ID, the query
#'defaults to being effective as of the end of the month. In other words, if a
#'JCO transfers from RJ to Mart in the month of June, this table will list their
#'location as being Mart.

capps_employment_spells <- function() {

  job_history <- read_excel("\\\\tjjd4avresaus1\\public\\HR and Analytics\\Job History.xlsx")
  capps_facility_code_lookup <- read_excel("\\\\tjjd4avresaus1\\public\\HR and Analytics\\Lookups\\capps_facility_code_lookup.xlsx")


  colnames(job_history) <- colnames(job_history) %>%
    str_replace_all(" ", "_") |>
    tolower()

  job_history <- job_history |>
    arrange(empl_id, eff_date, seq)

    temporary_starts <- job_history %>%
      filter(rsn_cd %in% c("041")) %>%
      group_by(empl_id) %>%
      mutate(temp_number = row_number()) %>%
      ungroup() %>%
      select(empl_id, temp_number, temp_start = eff_date)

    temporary_ends <- job_history %>%
      filter(rsn_cd %in% c("042")) %>%
      group_by(empl_id) %>%
      mutate(temp_number = row_number()) %>%
      ungroup() %>%
      select(empl_id, temp_number, temp_end = eff_date)

    temporary_spells <- temporary_starts %>%
      left_join(temporary_ends) %>%
      mutate(temp_end = replace_na(temp_end, today())) %>%
      rename(emplo_id = empl_id)

    temporary_entries <- sqldf("select * from job_history
                              inner join temporary_spells
                              on empl_id = emplo_id
                           and temp_start <= eff_date
                           and temp_end >= eff_date")

    permanent_job_history <- job_history %>%
      anti_join(temporary_entries) %>%
      filter(!str_detect(tolower(functional_job_title), "counsel sub")) %>%
      mutate(across(contains(c("date", "dt")), ~as_date(.x)),
             hire = if_else(tolower(action) %in% c("reh", "hir", "add"), 1, 0),
             term = if_else(!is.na(term_dt), 1, 0)) %>%
      left_join(capps_facility_code_lookup) %>%
      group_by(empl_id, eff_date) %>%
      summarize(position_id = last(posn),
                name = last(name),
                job_title = last(functional_job_title),
                facility = last(facility),
                supervisor_position_id = last(rpts_to_posn),
                hire = max(hire),
                term = max(term),
                actions = str_c(reason_descr, collapse = ", "),
                last_start = min(last_start_dt)) %>%
      ungroup() %>%
      mutate(start_date = if_else(term == 1, eff_date - 1, eff_date)) %>%
      group_by(empl_id) %>%
      mutate(entry = row_number()) %>%
      ungroup() %>%
      mutate(start_date = if_else(entry == 1 & hire == 0, last_start, start_date),
             end_date = if_else(term == 1, start_date,
                                if_else(empl_id == lead(empl_id), lead(start_date) - 1, today())),
             start_month = floor_date(start_date, 'month'),
             end_month = floor_date(end_date, 'month'),
             start_fq = state_fq(start_month),
             end_fq = state_fq(end_month),
             start_fy = state_fy(start_month),
             end_fy = state_fy(end_month),
             term_reason = case_when(str_detect(actions, "60") ~ "Voluntary",
                                     str_detect(actions, "63") ~ "At Will",
                                     str_detect(actions, "67") ~ "For Cause",
                                     str_detect(actions, "68") ~ "Retirement",
                                     term == 1 ~ "Other involuntary",
                                     .default = NA)) %>%
      relocate(start_date, .before = end_date) %>%
      select(-eff_date)

  end_reasons <- permanent_job_history %>%
    filter(term == 1) %>%
    select(empl_id, term_date = end_date, term_reason)

  spells <- permanent_job_history %>%
    group_by(empl_id, last_hire = last_start) %>%
    summarize(term = max(term),
              max_end = max(end_date)) %>%
    ungroup() %>%
    mutate(last_hire_month = floor_date(last_hire, "month"),
           term_date = if_else(term == 1, max_end, NA_Date_),
           last_term_month = floor_date(term_date, "month")) %>%
    left_join(end_reasons) %>%
    select(-c(term, max_end)) %>%
    filter(is.na(term_date) | term_date >= "2018-09-01")

  return(spells)
}
