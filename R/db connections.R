ird035_old_connection <- function()
{

myServer <- "tjjd4avsql"
myDatabase <- "IRD035_Pre20250322"
myDriver <- "SQL Server"

Connection_String <- paste0(
  "Driver=", myDriver,
  ";Server=", myServer,
  ";Database=", myDatabase)

ird035_old <- odbcDriverConnect(connection=Connection_String,rows_at_time=1)

return(ird035_old)
}

ird035_connection <- function()
{
  myServer <- "tjjd4avsql"
  myDatabase <- "IRD035"
  myDriver <- "SQL Server"

  Connection_String <- paste0(
    "Driver=", myDriver,
    ";Server=", myServer,
    ";Database=", myDatabase)

  ird035 <- odbcDriverConnect(connection=Connection_String,rows_at_time=1)

  return(ird035)
}

jce_connection <- function()
{
  myServer <- "stargazer"
  myDatabase <- "JuvenileCaseExtract"
  myDriver <- "SQL Server"

  Connection_String <- paste0(
    "Driver=", myDriver,
    ";Server=", myServer,
    ";Database=", myDatabase)

  jce <- odbcDriverConnect(connection=Connection_String,rows_at_time=1)

  return(jce)
}

noble_connection <- function()
{
  myServer <- "tjjd4avnobsql1"
  myDatabase <- "NSGCMS_TX_TJJD_JUVENILE"
  myDriver <- "SQL Server"

  Connection_String <- paste0(
    "Driver=", myDriver,
    ";Server=", myServer,
    ";Database=", myDatabase,
    ";Trusted_Connection=yes;TrustServerCertificate=yes")

  noble <- odbcDriverConnect(connection = Connection_String,rows_at_time=1)

  return(noble)
}

warehouse_connection <- function()
{
myServer <- "tjjd4mvdatawrhs"
myDatabase <- "ReportingDW"
myDriver <- "SQL Server"

Connection_String <- paste0(
  "Driver=", myDriver,
  ";Server=", myServer,
  ";Database=", myDatabase)

warehouse <- odbcDriverConnect(connection=Connection_String,rows_at_time=1)

return(warehouse)
}

tycemployees_connection <- function()
{
  myServer <- "tjjd4avsql"
  myDatabase <- "tycemployees"
  myDriver <- "SQL Server"

  Connection_String <- paste0(
    "Driver=", myDriver,
    ";Server=", myServer,
    ";Database=", myDatabase)

  tycemployees <- odbcDriverConnect(connection=Connection_String,rows_at_time=1)

  return(tycemployees)
}
