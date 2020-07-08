
#' Get Exchange Rate vs EUR from ECB
#'
#' @param cur Currency - three letter standard
#' @param start_date First day of time series
#' @param end_date Last day of time series
#' @param freq Upper case, single letter abbreviation of annual, daily, half-yearly, monthly or quarterly
#' @param type 'A' for average or 'E' for end-of-period
#'
#' @return A tibble with Datum as date index and y.xxx.EUR as values for currency XXX with frequency y
#' @export
#' @details This function implements the API to the ECB Statistical Data Warehouse.
#' @examples
#' get_ecb_fx("CHF")
get_ecb_fx = function(cur, start_date = "1999-01-04", end_date = Sys.Date(), freq = "D", type = "A"){

  fx_pair = stringr::str_c(freq, ".", cur,".EUR")

  # Building blocks for the URL
  entrypoint = 'https://sdw-wsrest.ecb.europa.eu/service/' # Using protocol 'https'
  resource = 'data'           # The resource for data queries is always'data'
  flowRef ='EXR'              # Dataflow describing the data that needs to be returned, exchange rates in this case
  key = stringr::str_c(fx_pair,".SP00.",type)    # Defining the dimension values, explained below

  # Define the parameters
  parameters = list(
    'startPeriod' = start_date,  # Start date of the time series
    'endPeriod' = end_date # End of the time series
  )

  request_url = stringr::str_c(entrypoint , resource , '/' , flowRef , '/' , key)

  # Make the HTTP request
  response = httr::GET(request_url, query = parameters, httr::accept("text/csv"))

  # Check if the response returns succesfully with response code 200
  f = tempfile(fileext = ".csv")
  readr::write_file(rawToChar(response$content),path = f)
  df = readr::read_csv(f, col_types = readr::cols())
  df = dplyr::select(df, Datum = TIME_PERIOD,
           fx = OBS_VALUE)

  names(df)[2] = fx_pair
  df
}
