#-----------------------------------------------------------------------------#
#                    Pre-processing temporal data functions                   #
#-----------------------------------------------------------------------------#

# Prepare temporal predictors: julian day, holidays, dust events
prep_calendar <- function(path_calendar, path_dust){
  
  # Prepare calendar grid
  calendar <- data.frame(date = seq.Date(as.Date("2018-01-01"),
                                         as.Date("2020-12-31"), "1 day"))
  
  # Julian day (reference: 2018-01-01)
  calendar$julian <- julian(calendar$date, origin=as.Date("2017-12-31"))
  
  # Weekday and calendar indicators
  calendar$weekday <- weekdays(calendar$date)
  calendar$year <- year(calendar$date)
  calendar$month <- month(calendar$date)
  calendar$day <- day(calendar$date)
  
  # Yearly day
  calendar <- group_by(calendar, year) %>%
    mutate(yday = julian-min(julian)+1) %>%
    ungroup()
  
  # Holidays
  holidays <- read_csv(path_calendar) %>%
    mutate(date = as.Date(Data, format="%d/%m/%Y")) 
  calendar$holiday <- ifelse(calendar$date %in% holidays$date, "Yes", "No")
  calendar$holiday <- ifelse(calendar$weekday %in% c("Saturday", "Sunday"), 
                             "Yes", calendar$holiday)
  
  # Saharian dust
  dust <- read_excel(path_dust) %>%
    mutate(date=as.Date(date))
  calendar <- left_join(calendar, dust, by="date")
  
  # Write to csv and return path
  write_rds(calendar, "database/original/calendar.rds")
  "database/original/calendar.rds"
}