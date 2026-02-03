#'Converts formatted CAPPS job history to a monthly display.
#'
#'@usage df <- capps_leave_by_month()
#'
#'@section This query converts unformatted CAPPS leave records into a format
#'  with a unique value for each employee and month.
#'
#'@note
#'
#'UID is Empl_ID combined with Month.


capps_leave_by_month <- function() {

  former_leave_by_month <- read_csv("\\\\tjjd4avresaus1\\public\\HR and Analytics\\Leave by Month.csv")

  timecode_lookup <- read_excel("\\\\tjjd4avresaus1\\public\\HR and Analytics\\Lookups\\CAPPS Time Code Lookup.xlsx")

  new_reported_time <- read_excel("\\\\tjjd4avresaus1\\public\\HR and Analytics\\Leave Update.xlsx")

  colnames(former_leave_by_month) <- tolower(colnames(former_leave_by_month))
  colnames(new_reported_time) <- tolower(colnames(new_reported_time))
  colnames(timecode_lookup) <- tolower(colnames(timecode_lookup))

  reported_time_by_month <- new_reported_time %>%
    mutate(month = floor_date(as_date(date), 'month'),
           empl_id = as.numeric(`empl id`)) %>%
    group_by(empl_id, month, trc, fmla_id = `fmla id`) %>%
    summarize(hours = sum(quantity)) %>%
    ungroup() %>%
    left_join(timecode_lookup %>%
                select(trc, time_code_description, on_the_job))

  leave_by_month <- reported_time_by_month %>%
    inner_join(timecode_lookup %>%
                 filter(clock_hours == "y",
                        on_the_job == "n") %>%
                 select(trc, time_code_description)) %>%
    group_by(empl_id, month) %>%
    summarize(hours_leave = sum(hours)) %>%
    ungroup() %>%
    bind_rows(former_leave_by_month) %>%
    arrange(empl_id, month) %>%
    group_by(empl_id, month) %>%
    summarize_all(first)

  return(leave_by_month)
}
