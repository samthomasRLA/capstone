#### data cleaning ####

#' Data cleaning function. Reads raw NOAA earthquake data and returns clean dataframe.
#'
#' @param data a .txt file contianin NOAA earthquake data
#'
#' @importFrom readr read_tsv()
#' @importFrom chron julian
#' @importFrom dlpyr %>%
#'
#' @export

eq_clean_dat <- function(data){
  raw_df <- readr::read_tsv(data)
  raw_df$MONTH <- replace(raw_df$MONTH,is.na(raw_df$MONTH),"01") %>% as.numeric()
  raw_df$DAY <- replace(raw_df$DAY,is.na(raw_df$DAY),"01") %>% as.numeric()
  raw_df$Date <- chron::julian(raw_df$MONTH, raw_df$DAY, raw_df$YEAR) %>% as.Date(origin = "1970-01-01")
  return(raw_df)
}

#' Function to clean location column
#'
#' @param df a dataframe of NOAA earthquake data
#'
#' @importFrom dplyr %>% bind_rows filter mutate
#' @importFrom tidyr separate
#' @importFrom tools toTitleCase
#'
#' @export

eq_location_clean <- function(df){
  df_clean <- df %>% tidyr::separate(LOCATION_NAME, into = c("country","Location"), sep = ":", remove = FALSE)
  df_na <- df_clean %>% dplyr::filter(is.na(Location)) %>% dplyr::mutate(Location = country)
  df_remain <- df_clean %>% dplyr::filter(!is.na(Location))
  df_all <- dplyr::bind_rows(df_na, df_remain)
  df_all$Location <- lapply(df_all$Location, tolower)
  df_all$Location <- lapply(df_all$Location, tools::toTitleCase)
  return(df_all)
}

### custom geoms ###

#' geom_timeline creates earthquake timeline
#'



#' geom_timeline_label creates labels for earthquake timeline
