#' Connect to DB Stack
#'
#' @param dbname str with data base name
#' @param username str with data base user name
#' @param password str of password for respective data base user
#' @param default.file with DB configurations
#'
#' @details Data base configurations must be provided as default.file in global
#' options as "DB_CONFIG".
#'
#' @return MariaDBConnection
#' @export
connectDBstack <- function(dbname, username, password, default.file = getOption("DB_CONFIG")){
  con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                             dbname=dbname,
                             groups="destination",
                             username=username,
                             password=password,
                             default.file = default.file)
  return(con)
}
