#' Pull formatted youth assignments.
#' @usage df <- axon_bodycam_log_get_from_api(date, source)
#' @section Source: Queries Axon's evidence.com API to pull the Evidence Created
#'   report
#'
#' @note
#'
#' Use lapply/map for multiple days. Long load time.

axon_bodycam_log_get_from_api <- function(startdate = today() - 2, enddate = today() - 1, source = "formatted") {

  library(httr)
  library(jsonlite)

  source<-tolower(source)

  secret <- "[secret]"
  partner_id <-  "[partner_id]"
  client_id <- "[client_id]"
  api_path <-  "api.evidence.com"

  evidence_com_api <- function(path) {
    url <- modify_url(paste0("https://", api_path), path = path)
    body <- list(
      grant_type = "client_credentials",
      partner_id = partner_id,
      client_id= client_id,
      client_secret = secret,
      description = "Read_Again")

    POST(
      url,
      body = body,
      encode = "form",
      content_type("application/x-www-form-urlencoded")
    )
  }

  resp <- evidence_com_api(path = "/api/oauth2/token")

  json <- content(resp)

  get_url <- str_c("https://api.evidence.com/api/v1/agencies/,", partner_id, "/reports/data?reportType=evidencecreated&fromDate=", startdate, "&toDate=", enddate, "&pagesize=50&pageoffset=")

  created <- map_df(c(0:49), function(x) {
   increment <- GET(str_c(get_url, x),
        add_headers(authorization = paste0("Bearer ", json$access_token)))

  query_results <- rawToChar(increment$content) %>%
    fromJSON

  return(query_results$data)
  }
  )

  if (source == "formatted")  {

    bodycam_format <- created %>%
      mutate(across(contains("date"),~ymd_hms(str_replace(str_sub(.x,1,-11),"t"," "))),
             employee_id = as.numeric(ownerBadgeId),
             owner_first = ownerFirstName,
             owner_last = ownerLastName,
             owner = str_c(ownerFirstName, " ", ownerLastName),
             clip_start = dateRecordStart,
             clip_end = dateRecordEnd,
             date_clip_start = as_date(dateRecordStart),
             date_clip_end = as_date(dateRecordEnd),
             seconds_duration = as.numeric(durationSeconds),
             hours_duration = as.numeric(durationSeconds)/3600) %>%
      select(evidence_id = evidenceId,
             employee_id,
             owner_first,
             owner_last,
             owner,
             serial_number = serialNumber,
             clip_start,
             clip_end,
             date_clip_start,
             date_clip_end,
             seconds_duration,
             hours_duration) %>%
      mutate(clip_end = if_else(is.na(clip_end), clip_start + seconds_duration, clip_end),
             date_clip_end = as_date(clip_end))

    print(Sys.time())

    return(bodycam_format)
}
  else if (source == "unformatted") {

    return(created)

    }
  else {
    "please enter an accepted source: 'formatted', 'unformatted'"
  }

}
