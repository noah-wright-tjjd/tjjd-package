#' @title Interval Generator
#' @description Converts tables in "UID - Start Date/time - End Date/time" format to "UID - Interval_Start - Interval Time" format
#' @param DF the dataframe
#' @param Start_Col the name of the variable (must be Date or Posix) defining period start
#' @param End_Col the name of the variable (must be Date or Posix) defining period end
#' @param Int_Unit the time interval used (e.g. "year", "month", "day", hour" "min", "sec". Must be in quotes.)
#' @examples
#' Test_DFDate <- data.frame(UID = c(1,2,3),
#'                          Start_Date = as_date(c("2010-01-01", "2010-04-01", "2011-01-01")),
#'                          End_Date = as_date(c("2010-06-30", "2011-12-31", "2011-01-02")))
#'
#' Interval_Convert.Date(Test_DFDate, Start_Date, End_Date, "day", 1)
#' Interval_Convert.Date(Test_DFDate, Start_Date, End_Date, "month", 1)
#' Interval_Convert.Date(Test_DFDate, Start_Date, End_Date, "month", 3)
#' Interval_Convert.Date(Test_DFDate, Start_Date, End_Date, "year", 1)
#'
#' Test_DFDate_Time <- data.frame(UID = c(1,2,3),
#'                              Start_Date_Time = as.POSIXct(c("2010-01-01 01:00:00", "2010-01-01 01:00:00", "2010-01-01 01:00:00")),
#'                              End_Date_Time = as.POSIXct(c("2010-01-01 01:00:10", "2010-01-01 01:10:00", "2010-01-01 02:02:00")))
#'
#' Interval_Convert.Posix(Test_DFDate_Time, Start_Date_Time, End_Date_Time, "sec", 1)
#' Interval_Convert.Posix(Test_DFDate_Time, Start_Date_Time, End_Date_Time, "min", 1)
#' Interval_Convert.Posix(Test_DFDate_Time, Start_Date_Time, End_Date_Time, "hour", 1)

interval_convert.date <- function(df, start_col, end_col, int_unit, int_length = 1, remove_zero_duration = FALSE) {

  start_col2 <- enquo(start_col)
  end_col2 <- enquo(end_col)

  zero_duration <- df %>%
    filter(!!start_col2 == !!end_col2) %>%
    mutate(interval_start = !!start_col2,
           interval_end = !!start_col2,
           single = TRUE,
           duration_within_interval = 0)

  start_end <- df %>%
    ungroup() %>%
    summarize(min_start = min(!!start_col2),
              max_end = max(!!end_col2)) %>%
    mutate(start = floor_date(min_start, int_unit),
           end = ceiling_date(max_end, int_unit))

  df <- df %>%
    mutate(single = !!start_col2 == !!end_col2)

  interval_table <- data.frame(interval_start = seq.Date(start_end$start[1], start_end$end[1], by = str_c(int_length, " ", int_unit))) %>%
    mutate(interval_end = lead(interval_start)) %>%
    filter(!is.na(interval_end))

  by <- join_by(interval_start <= !!end_col2, interval_end >= !!start_col2)

  interval_data_table <- interval_table %>%
    left_join(df, by) %>%
    mutate(duration_within_interval = as.numeric(if_else(!!end_col2 > interval_end, interval_end, !!end_col2) -
                                                   if_else(!!start_col2 < interval_start, interval_start, !!start_col2))) %>%
    filter(as.numeric(duration_within_interval) > 0)

  if (remove_zero_duration) {

    return(interval_data_table)

  } else {
    return(interval_data_table  %>%
             bind_rows(zero_duration) %>%
             arrange(interval_start, interval_end))
  }

}

interval_convert.posix <- function(df, start_col, end_col, int_unit, int_length = 1, remove_zero_duration = FALSE) {

  start_col2 <- enquo(start_col)
  end_col2 <- enquo(end_col)

  zero_duration <- df %>%
    filter(!!start_col2 == !!end_col2) %>%
    mutate(interval_start = !!start_col2,
           interval_end = !!start_col2,
           single = TRUE,
           duration_within_interval = 0)

  start_end <- df %>%
    ungroup() %>%
    summarize(min_start = min(!!start_col2),
              max_end = max(!!end_col2)) %>%
    mutate(start = floor_date(min_start, int_unit),
           end = ceiling_date(max_end, int_unit))

  df <- df %>%
    mutate(single = !!start_col2 == !!end_col2)

  interval_table <- data.frame(interval_start = seq.posixt(start_end$start[1], start_end$end[1], by = str_c(int_length, " ", int_unit))) %>%
    mutate(interval_end = lead(interval_start)) %>%
    filter(!is.na(interval_end))

  by <- join_by(interval_start <= !!end_col2, interval_end >= !!start_col2)

  interval_data_table <- interval_table %>%
    left_join(df, by) %>%
    mutate(duration_within_interval = as.numeric(if_else(!!end_col2 > interval_end, interval_end, !!end_col2) -
                                                   if_else(!!start_col2 < interval_start, interval_start, !!start_col2))) %>%
    filter(as.numeric(duration_within_interval) > 0)

  if (remove_zero_duration) {

    return(interval_data_table)

  } else {
    return(interval_data_table  %>%
             bind_rows(zero_duration) %>%
             arrange(interval_start, interval_end))
  }

}



state_fy <- function (input_date) {
  if_else(month(input_date) %in% c(9,10,11,12), year(input_date) + 1, year(input_date))
}

state_fq <- function(input_date) {
  case_when(month(input_date) %in% c(9:11) ~ 1,
            month(input_date) %in% c(12, 1, 2) ~ 2,
            month(input_date) %in% c(3:5) ~ 3,
            month(input_date) %in% c(6:8) ~ 4)
}

state_fq_date <- function(input_date) {
  case_when(month(input_date) %in% c(9:11) ~ as_date(str_c(year(input_date), "-09-01")),
            month(input_date) == 12 ~ as_date(str_c(year(input_date), "-12-01")),
            month(input_date) %in% c(1:2) ~ as_date(str_c(year(input_date)-1, "-12-01")),
            month(input_date) %in% c(3:5) ~ as_date(str_c(year(input_date), "-03-01")),
            month(input_date) %in% c(6:8) ~ as_date(str_c(year(input_date), "-06-01")))
}

tjjd_style <- function () {
  windowsfonts(calibri=windowsfont("calibri"))
  font <- "calibri"
  ggplot2::theme(plot.title = ggplot2::element_text(family = font, size = 28, face = "bold", color = "#222222"),
                 plot.subtitle = ggplot2::element_text(family = font, size = 22, margin = ggplot2::margin(9, 0, 9, 0)),
                 legend.position = "top", legend.text.align = 0, legend.background = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text (family = font, size = 18, color = "#222222"),
                 axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(family = font, size = 18, color = "#222222"),
                 axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
                 axis.ticks = ggplot2::element_blank(),
                 axis.line = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(color = "#dcdcdc"),
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(fill = "white"),
                 strip.text = ggplot2::element_text(size = 22, hjust = 0))
}

tjjd_colors <-c("#214357", "#74172b", "#0079a6", "#28aada", "#1e4517", "#dcdcdc", "#b4b4b4", "#8c8c8c", "#575757", "#313131")

facilities <- c("Evins", "Gainesville", "Giddings", "Mart", "McLennan", "Ron Jackson")

data_request <- function(request_name){

  request_dir <- str_c("Active Requests/", request_name, "/")

  names(request_dir) <-  "request_directory"

  list2env(as.list(request_dir), envir = .GlobalEnv)

  if (!dir.exists(request_dir)) {

    dir.create(request_dir)

    file.create(str_c(request_dir, request_name, ".R"))

    utils::browseURL(request_dir)

  }

}

to_csv <- function(df) {

  file_path <- str_c(request_directory,
                     str_replace_all(deparse(substitute(df)), "_", " "),
                     ".csv")

  colnames(df) <- str_replace_all(colnames(df), "_", " ")

  write_csv(df, file_path, na = "")
}

file_df_make <- function(dir) {

  files_dir <- data.frame(filename = list.files(dir)) |>
  mutate(filename_no_ext = str_remove_all(filename, "\\..*"),
         filepath = str_c(dir, filename))

  return(files_dir)

}
