#' Pull formatted records of employee demographics
#' @usage df <- employee_summary_get()
#' @section Source: TYC_PG2_JOB_CURR, a table created by IT that summarizes
#'   basic information of current employees, queried from database
#'   "TYCEmployees". This is in turn reduced in size into a view
#'   "vCAPPS_PG2_JOB_CURR" that restricts the query to employees who have been
#'   entered into CAPPS (and this is the table that the formatted query pulls
#'   from).
#'
#'   UID is based on employee ID, meaning that changes over time are not
#'   accounted for. Each row contains only the most recent data for each
#'   employee, meaning that if an employee used to be a JCO and is now a case
#'   manager, their title will only show as "case manager".  Use
#'   CAPPS_Job_History_Get() for
#'
#'   In addition, the formatted query pulls rehire dates for currently-employed
#'   rehires from CAPPS_CEI_Empl also in "TYCEmployees".
#'
#'   The raw 'sql' version of this query pulls the full TYC_PG2_JOB_CURR and no
#'   rehire information from CAPPS_CEI_Empl.
#'
#' @note
#'
#' CCS Location code "MCA" appears in original data when it should be "MCL".
#' Likewise "HNU" appears when it should be "HKH". This query replaces those
#' values.
#'
#' Data filtered on employees with a TINSID (meaning active since 2018 or so).

employee_summary_get <- function(source = "formatted", timeframe = "current") {

  source<-tolower(source)
  location_type_lookup <- import("\\\\tjjd4avresaus1\\public\\hr and analytics\\lookups\\location_type_lookup.xlsx")
  tycemployees <- tycemployees_connection()

  if (source == "formatted")  {cei<-sqlQuery(tycemployees,
                                             "select emplid, hiredate, rehiredate
           from capps_cei_empl") %>%
    mutate(hire_date=as_date(hiredate,format="%m/%d/%Y"),
           rehire_date=as_date(rehiredate,format="%m/%d/%Y")) %>%
    select(tinsid=emplid,
           hire_date,
           rehire_date) %>%
    filter(hire_date!=rehire_date)

  pg2 <- sqlQuery(tycemployees,
                  "select tinsid, fullname, email, lasthiredate, terminationdate, birthdate, ethniccode, gender, senioritydate, jobcodedescription, positioncodedescription, activity, ccsloccode, location, supervisorfullname
                                          from vcapps_pg2_job_curr")

  combined <- pg2 %>%
    left_join(cei) %>%
    mutate(initial_hire_date = as_date(lasthiredate),
           termination_date = as_date(terminationdate),
           state_seniority_date = as_date(senioritydate),
           birth_date = as_date(birthdate),
           ccs_loc_code = if_else(str_detect(tolower(positioncodedescription),"jco") &
                                    str_detect(tolower(positioncodedescription),"trans"),
                                  "TRA",
                                  ccsloccode),
           ccs_loc_code = str_replace_all(ccs_loc_code, c("BOA" = "BWD", "MCA" = "MCL", "HNU" = "HKH"))) %>%
    select(employee_id=tinsid,
           name=fullname,
           email,
           birth_date = birthdate,
           gender,
           ethnicity = ethniccode,
           job_code_description = jobcodedescription,
           position_code_description = positioncodedescription,
           division = activity,
           state_seniority_date,
           initial_hire_date,
           rehire_date,
           termination_date,
           location_code = ccs_loc_code,
           hr_location = location,
           supervisor = supervisorfullname) %>%
    left_join(location_type_lookup, "location_code")

  if(timeframe == "historical") {
    return(combined)
  }

  else if (timeframe == "current") {
    return(combined |>
             filter(is.na(termination_date)))
  }

  else {
    "please enter an accepted timeframe: 'current', 'historical'"
  }
  }
  else if (tolower(source) == "sql") {
    sqlQuery(tycemployees,
             "select * from tyc_pg2_job_curr")
  } else {
    "please enter an accepted source: 'formatted', 'sql'"
  }
}
