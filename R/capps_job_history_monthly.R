#'Converts formatted CAPPS job history to a monthly display.
#'
#'@usage df <- capps_job_history_monthly()
#'
#'@section This query converts the reformatted CAPPS job history into a format
#'  with a unique value for each employee and month.
#'
#'  This employee-month table serves as the basis of several agency Power BI
#'  dashboards.
#'
#'@note
#'
#'UID is Empl_ID combined with Month.
#'
#'Query collapses all employee actions into the month in which they occurred. In
#'other words, if an employee transferred location and received a pay raise on
#'in the same month, the "Actions" column will include both of these.
#'
#'For name, title, location, position ID, and supervisor position ID, the query
#'defaults to being effective as of the end of the month. In other words, if a
#'JCO transfers from RJ to Mart in the month of June, this table will list their
#'location as being Mart.

capps_job_history_monthly <- function() {

  permanent_job_history <- capps_job_history_get()
  leave_by_month <- read_csv("\\\\tjjd4avresaus1\\public\\HR and Analytics\\Leave by Month.csv")

  colnames(leave_by_month) <- tolower(colnames(leave_by_month))

  leave_by_month <- leave_by_month %>%
    mutate(empl_id = as.character(empl_id))

  capps_facility_code_lookup <- read_excel("\\\\tjjd4avresaus1\\public\\HR and Analytics\\Lookups\\capps_facility_code_lookup.xlsx")

  colnames(leave_by_month) <- tolower(colnames(leave_by_month))

  month_table <- data.frame(month = seq.Date(as_date("2018-09-01"), today(), "month"))

  positions_by_month <- sqldf("select * from
                           month_table
                           inner join permanent_job_history
                           on month >= start_month and
                           month <= end_month") %>%
    arrange(empl_id, month) %>%
    group_by(month, empl_id) %>%
    summarize(start_month = min(start_month),
              end_month = max(end_month),
              position_id = last(position_id),
              name = last(name),
              job_title = last(job_title),
              facility = last(facility),
              supervisor_position_id = last(supervisor_position_id),
              hire = max(hire),
              term = max(term)) %>%
    ungroup() %>%
    mutate(hire = if_else(hire == 1 & start_month == month, 1, 0),
           fy = state_fy(month),
           fq = state_fq(month)) %>%
    unique() %>%
    filter(month <= floor_date(today(), "month"))

  actions_by_month <- permanent_job_history %>%
    group_by(empl_id, month = start_month) %>%
    summarize(actions = str_c(actions, collapse = ", "))

  employment_by_month <- positions_by_month %>%
    left_join(actions_by_month) %>%
    left_join(leave_by_month) %>%
    mutate(hours_leave = replace_na(hours_leave, 0))

  return(employment_by_month)
}
