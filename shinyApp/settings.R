db <- list(
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT")),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode  = Sys.getenv("DB_SSLMODE", unset = "require")
)

lang <- "de"

logo_file <- "GoStrong.png"
time_zone <- "Europe/Paris"
#time_zone <- "America/Sao_Paulo"

checkAccount <- function(user, pass) {
  sql <- "SELECT checklogin FROM checkLogin($1, $2)"
  getPostgresql(sql, params = list(user, pass))$checklogin > 0
}

group_colors <- c(
  "#F5FFFA",  # Mint cream
  "#FFFACD",  # Lemon chiffon
  "#E6E6FA",  # Lavender
  "#FAEBD7",  # Antique white
  "#F0F8FF",  # Alice blue
  "#E0FFFF",  # Light cyan
  "#FFE4E1",  # Misty rose
  "#F5F5DC",  # Beige
  "#E0EEE0",  # Honeydew soft
  "#FFF0F5",  # Lavender blush
  "#F0FFF0",  # Honeydew
  "#E6F2FF"   # Very light blue
)
