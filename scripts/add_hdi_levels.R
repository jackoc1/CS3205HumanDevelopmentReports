library(stringr)

hdi.df <- read.csv("../data/hdi.csv")

# fix col names and country names
colnames(hdi.df)[2:31] <- substr(colnames(hdi.df)[2:31], 2, 5)
hdi.df$Country_name <- trimws(hdi.df$Country_name)
hdi.df$Country_name <- str_sub(hdi.df$Country_name, 1, nchar(hdi.df$Country_name)-1)

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

hdi.years.df <- hdi.df[2:31]
hdi.years.levels.df <- hdi.years.df
hdi.years.levels.df <- data.frame(apply(hdi.years.df, 1:2, assign.level))
colnames(hdi.years.levels.df) <- c(paste(colnames(hdi.years.df), "_level", sep=""))
hdi.df <- cbind(hdi.df, hdi.years.levels.df)
hdi.df <- hdi.df[,sort(colnames(hdi.df))]

write.csv(hdi.df, "../data/hdi_with_levels.csv")
