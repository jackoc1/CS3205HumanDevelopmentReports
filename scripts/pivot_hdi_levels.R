library(stringr)

hdi.df <- read.csv("../data/hdi.csv")

country_code <- rep(levels(hdi.df$Country_code), rep(30, length(hdi.df$Country_code)))

country_by_alphabetical_code_order <- hdi.df$Country_name[hdi.df$Country_code]
country_name <- rep(country_by_alphabetical_code_order, rep(30, length(hdi.df$Country_name)))
country_name <- trimws(country_name)
country_name <- str_sub(country_name, 1, nchar(country_name)-1)

year <- rep(colnames(hdi.df)[2:31], length(hdi.df$Country_code))
year <- substr(year, 2, 5)

hdi <- c()
for (i in 1:length(hdi.df$Country_code)) {
  hdi <- c(hdi, unlist(hdi.df[i, 2:31]))
}

assign.level <- function( hdi ) {
  if (is.na(hdi)) {
    return(NA)
  }
  if (hdi>=0.8) {
    return(1)
  }
  if (hdi>=0.7) {
    return(2)
  }
  if (hdi>=0.55) {
    return(3)
  }
  return(4)
}
hdi_level <- sapply(hdi, assign.level)

hdi.pivot.df <- data.frame(country_code, country_name, year, hdi, hdi_level)
write.csv(hdi.pivot.df, "../data/hdi_pivoted.csv")
