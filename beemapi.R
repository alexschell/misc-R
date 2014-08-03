# Some R functions for interacting with the Beeminder API

username <- "username"
auth.token <- "asdf1234"

# Returns date, hr, min, and value of last datapoint of a
# Beeminder goal (numeric)
BmndrLastDatapoint <- function(goal) {
# TODO:
# Broaden this to parse datapoints.json fully
  require("RCurl")
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem",
                                                   package = "RCurl")))

  datapoints.URL <- paste("https://www.beeminder.com/api/v1/users/", username,
                   "/goals/", goal, "/datapoints.json?auth_token=",
				   auth.token, sep = "")
  datapoints <- getURL(datapoints.URL)
  datapoints <- substring(datapoints, 1, 500)
  
  pattern <- "(.*?)\"value\":([0-9.]+),\"comment(.*)"
  val <- as.numeric(sub(pattern, "\\2", datapoints))
  
  pattern <- "(.*?)\"updated_at\":([0-9]+),\"requestid(.*)"
  timestamp <- as.POSIXct(as.numeric(sub(pattern, "\\2", datapoints)),
                          origin = "1970-01-01")
  date <- as.numeric(as.Date(substring(timestamp, 1, 10)))
  hr <- as.numeric(substring(timestamp, 12, 13))
  min <- as.numeric(substring(timestamp, 15, 16))
  
  data.frame(date = date, hr = hr, min = min, val = val)
}

BmndrSubmit <- function(goal, value, comment = "", midnight = 0, date = "") {
  require("RCurl")
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem",
                                                   package = "RCurl")))
  
  if (date == "") {
    hr = as.numeric(substring(Sys.time(), 12, 13))
    if (hr >= midnight) {
      date <- as.character(Sys.Date())
    } else {
      date <- as.character(Sys.Date() - 1)
    }
  }
  
  URL <- paste("https://www.beeminder.com/api/v1/users/alexschell/goals/",
               goal, "/datapoints.json?auth_token=", auth.token, sep = "")
  
  datapoint <- list(
        timestamp = as.numeric(as.POSIXct(paste(date, "12:00:00"))),
        value = as.character(value),
        comment = comment,
        updated_at = as.numeric(as.POSIXct(Sys.time())))

  postForm(URL, style = "post", .params = datapoint)
  # success check: BmndrLastDatapoint()[4] == value
}