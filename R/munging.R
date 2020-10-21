cleanData = function(data) {
  # replacing NA values with 0 for columns Departure.Delay.in.Minutes, Arrival.Delay.in.Minutes, Flight.time.in.minutes
  if("Departure.Delay.in.Minutes" %in% names(data)) {
    data$Departure.Delay.in.Minutes[is.na(data$Departure.Delay.in.Minutes)] = 0
  }
  if("Arrival.Delay.in.Minutes" %in% names(data)) {
    data$Arrival.Delay.in.Minutes[is.na(data$Arrival.Delay.in.Minutes)] = 0
  }
  if("Flight.time.in.minutes" %in% names(data)) {
    data$Flight.time.in.minutes[is.na(data$Flight.time.in.minutes)] = 0
  }

  # removing columns not needed
  data = data[, !names(data) %in% c("Destination.City", "Origin.City", "Partner.Name", "Flight.date", "freeText")]
  return(data)
}

prepareForAnalysis = function(data, columns) {
  categoricalColumns = c("Airline.Status", "Gender", "Class", "Type.of.Travel", "Partner.Code", "Origin.State", "Destination.State", "Flight.cancelled", "Price.Sensitivity", "Total.Freq.Flyer.Accts", columns)

  # cleaing data
  data = cleanData(data)

  # creating dummy variables for categorical variables
  data = dummy_cols(data, select_columns = categoricalColumns, remove_first_dummy = TRUE)

  # removing columns for which dummy variables were created
  data = data[, !names(data) %in% categoricalColumns]

  return(data)
}

getBinnedData = function(data) {
  # numericalColumns = c("Age", "Price.Sensitivity", "Year.Of.First.Flight", "Flights.Per.Year",  "Loyalty",
  #                      "Total.Freq.Flyer.Accts", "Shopping.Amount.at.Airport", "Eating.and.Drinking.at.Airport", "Day.of.Month",
  #                      "Scheduled.Departure.Hour", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes",
  #                      "Flight.time.in.minutes", "Flight.Distance", "olong", "olat", "dlong", "dlat")

  # clean data
  data = cleanData(data)

  # Age (15-85)
  data = data %>%
    mutate(Age = cut(Age, breaks = c(min(Age), 25, 35, 45, 55, 65, 75, max(Age)), labels = c("<25", "25-35", "35-45", "45-55", "55-65", "65-75", ">75"), include.lowest = TRUE))

  # Year.Of.First.Flight (2003-2012)
  data = data %>%
    mutate(Year.of.First.Flight = cut(Year.of.First.Flight, breaks = c(min(Year.of.First.Flight), 2006, 2009, max(Year.of.First.Flight)), labels = c("before 2006", "2006-09", "after 2009"), include.lowest = TRUE))

  # Flights.Per.Year (0-100)
  data = data %>%
    mutate(Flights.Per.Year = cut(Flights.Per.Year, breaks = c(min(Flights.Per.Year), 40, max(Flights.Per.Year)), labels = c("<40", ">40"), include.lowest = TRUE))

  # Loyalty (-1-1)
  data = data %>%
    mutate(Loyalty = cut(Loyalty, breaks = c(min(Loyalty), -0.5, 0, 0.5, max(Loyalty)), labels = c("<-0.5", "-0.5-0", "0-0.5", ">0.5"), include.lowest = TRUE))

  # Shopping.Amount.at.Airport (0-600)
  data = data %>%
    mutate(Shopping.Amount.at.Airport = cut(Shopping.Amount.at.Airport, breaks = c(min(Shopping.Amount.at.Airport), 100, 200, 300, 400, 500, max(Shopping.Amount.at.Airport)), labels = c("<100", "100-200", "200-300", "300-400", "400-500", ">500"), include.lowest = TRUE))

  # Eating.and.Drinking.at.Airport (0-500)
  data = data %>%
    mutate(Eating.and.Drinking.at.Airport = cut(Eating.and.Drinking.at.Airport, breaks = c(min(Eating.and.Drinking.at.Airport), 100, 200, 300, 400, max(Eating.and.Drinking.at.Airport)), labels = c("<100", "100-200", "200-300", "300-400", ">400"), include.lowest = TRUE))

  # Day.of.Month (1-31)
  data = data %>%
    mutate(Day.of.Month = cut(Day.of.Month, breaks = c(min(Day.of.Month), 15, max(Day.of.Month)), labels = c("before 15th", "after 15th"), include.lowest = TRUE))

  # Scheduled.Departure.Hour (0-24)
  data = data %>%
    mutate(Scheduled.Departure.Hour = cut(Scheduled.Departure.Hour, breaks = c(min(Scheduled.Departure.Hour), 12, max(Scheduled.Departure.Hour)), labels = c("before 12pm", "after 12pm"), include.lowest = TRUE))

  # Departure.Delay.in.Minutes (0-550)
  data = data %>%
    mutate(Departure.Delay.in.Minutes = cut(Departure.Delay.in.Minutes, breaks = c(min(Departure.Delay.in.Minutes), 100, 200, 300, 400, max(Departure.Delay.in.Minutes)), labels = c("<100", "100-200", "200-300", "300-400", ">400"), include.lowest = TRUE))

  # Arrival.Delay.in.Minutes (0-650)
  data = data %>%
    mutate(Arrival.Delay.in.Minutes = cut(Arrival.Delay.in.Minutes, breaks = c(min(Arrival.Delay.in.Minutes), 100, 200, 300, 400, 500, max(Arrival.Delay.in.Minutes)), labels = c("<100", "100-200", "200-300", "300-400", "400-500", ">500"), include.lowest = TRUE))

  # Flight.time.in.minutes (0-400)
  data = data %>%
    mutate(Flight.time.in.minutes = cut(Flight.time.in.minutes, breaks = c(min(Flight.time.in.minutes), 100, 200, 300, max(Flight.time.in.minutes)), labels = c("<100", "100-200", "200-300", ">300"), include.lowest = TRUE))

  # Flight.Distance (0-3000)
  data = data %>%
    mutate(Flight.Distance = cut(Flight.Distance, breaks = c(min(Flight.Distance), 1000, 2000, max(Flight.Distance)), labels = c("<1000", "1000-2000", ">2000"), include.lowest = TRUE))

  # olong (-170 - -60)
  data = data %>%
    mutate(olong = cut(olong, breaks = c(min(olong), -145, -120, -95, max(olong)), labels = c("<-145", "-145 - -120", "-120 - -95", ">-95"), include.lowest = TRUE))

  # olat (15-75)
  data = data %>%
    mutate(olat = cut(olat, breaks = c(min(olat), 30, 45, 60, max(olat)), labels = c("<30", "30-45", "45-60", ">60"), include.lowest = TRUE))

  # dlong (-170 - -60)
  data = data %>%
    mutate(dlong = cut(dlong, breaks = c(min(dlong), -145, -120, -95, max(dlong)), labels = c("<-145", "-145 - -120", "-120 - -95", ">-95"), include.lowest = TRUE))

  # dlat (15-65)
  data = data %>%
    mutate(dlat = cut(dlat, breaks = c(min(dlat), 30, 45, 60, max(dlat)), labels = c("<30", "30-45", "45-60", ">60"), include.lowest = TRUE))

  # Likelihood.to.recommend (1-10)
  data = data %>%
    mutate(Likelihood.to.recommend = cut(Likelihood.to.recommend, breaks = c(min(Likelihood.to.recommend), 7, 8, max(Likelihood.to.recommend)), labels = c("Detractor", "Passive", "Promoter"), include.lowest = TRUE))

  return(data.frame(apply(data, MARGIN = 2, factor)))
}
