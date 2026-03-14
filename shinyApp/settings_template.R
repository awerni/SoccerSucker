db <- list(dbname = "WorldCup2024", host = "myserver", port = 5432, user = "user", password = "pass")
lang <- "en"

logo_file <- "GoStrong.png"
time_zone <- "Europe/Paris"

checkAccount <- function(user, pass) {
  sql <- paste0("SELECT checklogin FROM checkLogin('", user, "', '", pass, "')")
  getPostgresql(sql)$checklogin > 0
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