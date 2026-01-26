db <- list(dbname = "WorldCup2024", host = "myserver", port = 5432, user = "user", password = "pass")
lang <- "en"

logo_file <- "GoStrong.png"
time_zone <- "Europe/Paris"

checkAccount <- function(user, pass) {
  sql <- paste0("SELECT checklogin FROM checkLogin('", user, "', '", pass, "')")
  getPostgresql(sql)$checklogin > 0   
}
