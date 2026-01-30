db <- list(dbname = "WorldCup2024", host = "myserver", port = 5432, user = "user", password = "pass")

logo_file <- "GoStrong.png"

lang <- coalesce(Sys.getenv("lang", unset = NA), "de")
#time_zone <- "America/Sao_Paulo"
time_zone <- coalesce(Sys.getenv("timezone", unset = NA), "Europe/Paris")

checkAccount <- function(user, pass) {
  sql <- paste0("SELECT checklogin FROM checkLogin('", user, "', '", pass, "')")
  getPostgresql(sql)$checklogin > 0
}
