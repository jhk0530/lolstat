#' @title lolstat common functions
#'
#' @description common functions used in multiple functions
#'
#' @import xml2 rvest jsonlite httr
#' @export
key_check = function(APIKey){
  if(is.null(APIKey)) stop("Key is missing.", call. = FALSE)
}
#' @export
lolstat_url_json <- function(urlResult) {
  jsonObj <- urlResult %>%
    xml2::read_html() %>%
    rvest::html_text() %>%
    jsonlite::parse_json()
  return(jsonObj)
}

#' @export
lolstat_get = function(x, waittime = 1.5){
  Sys.sleep(waittime)
  x = httr::GET(url = x)
  if(x$status_code == '400') stop("Bad request", call. = FALSE)
  if(x$status_code == '401') stop("Unauthorized", call. = FALSE)
  if(x$status_code == '403') stop("Forbidden", call. = FALSE)
  if(x$status_code == '404') stop("Data not found", call. = FALSE)
  if(x$status_code == '405') stop("Method not allowed", call. = FALSE)
  if(x$status_code == '415') stop("Unsupported media type", call. = FALSE)
  if(x$status_code == '429') stop("Rate limit exceeded", call. = FALSE)
  if(x$status_code == '500') stop("Internal server error", call. = FALSE)
  if(x$status_code == '502') stop("Bad gateway", call. = FALSE)
  if(x$status_code == '503') stop("Service unavailable", call. = FALSE)
  if(x$status_code == '504') stop("Gateway timeout", call. = FALSE)
  return(x)
}

