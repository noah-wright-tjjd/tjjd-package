#'Pull and formats CAPPS job history report prepared by HR
#'
#'@usage df <- capps_job_history_get()
#'
#'@section CAPPS does not allow auto-export of data, so HR must run a job
#'  history report monthly that feeds into turnover and other dashboards. This
#'  query takes the history report from its share drive location, removes all
#'  temporary assignments, and reformats the table to be more easily read.
#'
#'@note
#'
#'UID is Empl_ID combined with Start_Date.
#'
#'Query collapses multiple employee actions on the same day to a single row. In
#'other words, if an employee had a status change and a pay raise on the same
#'day, the "Actions" column will include both of these.

capps_job_history_get <- function(source = "formatted") {

source <- tolower(source)

job_history <- read_excel("\\\\tjjd4avresaus1\\public\\HR and Analytics\\Job History.xlsx")
capps_facility_code_lookup <- read_excel("\\\\tjjd4avresaus1\\public\\HR and Analytics\\Lookups\\capps_facility_code_lookup.xlsx")

colnames(job_history) <- colnames(job_history) %>%
  str_replace_all(" ", "_") |>
  tolower()

job_history <- job_history %>%
  arrange(empl_id, eff_date, seq)


if (source == "unformatted") {

return(job_history)

} else if (source == "formatted") {
#remove everything between temporary start and temporary end

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

#clean up remaining entries

#effective date of termination can be same as effective date of another hr change
#but effective date of termination is 1 day after the actual termination happened
#so if i'm using "start date" as my universal, this creates a problem, there's an indefinite action following term
#that mistakenly implies the staffer is still employed

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
         end_fy = state_fy(end_month)) %>%
  relocate(start_date, .before = end_date) %>%
  select(-eff_date)

return(permanent_job_history)

}

else ("please enter an accepted source: 'formatted', 'unformatted'")

}
