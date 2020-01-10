#' Set and get API key
#'
#' @details
#' Use setter and getter to store your API Key in one of three ways.
#' Their use is optional. You can always pass the API key string to the \code{key} argument of any package function that requires it,
#' but you do not have to.
#'
#' By default the \code{key} argument for these functions is \code{key = getAPIKey()}.
#' If your key has been stored in a manner that can be retrieved,
#' then you can call all the package API functions without having to provide the \code{key} argument repeatedly.
#'
#' You can get API key from https://developer.riotgames.com/
#'
#' @section Key storage methods:
#' If you have already set your key globally somewhere using \code{setAPIKey}, \code{getAPIKey} will retrieve it.
#' You can add the \code{LOLSTAT_KEY = "yourkey"} key-value pair to \code{options()} or as a system environment variable yourself
#' and \code{getAPIKey} will pick it up as long as you use the name \code{LOLSTAT_KEY}.
#' For convenience you can do this in your R session with \code{setAPIKey}.
#' It gives you three options for how to store the key.
#' The default is to use the \code{lolstat} package environment that is created when the package is loaded.
#'
#' @section Persistence:
#' Note that none of these three storage methods, including \code{"sysenv"} are persistent;
#' The stored key is lost when the R session is terminated.
#' A key that is stored outside of R as a system environment variable is retrievable with \code{getAPIkey},
#' just like those set in an R session with \code{setAPIKey} and \code{store = "sysenv"}.
#' However, if you truly want the key to persist as an environment variable when R terminates,
#' you must manually add it somewhere like \code{.Renviron};
#' \code{Sys.setenv} in R cannot achieve this.
#'
#' @param key character, API key. like RGAPI-****'
#' @param store character, method for storing API key. See details.
#'
#' @return \code{getAPIKey} returns the key string or \code{NULL} with a warning. \code{setAPIKey} returns a success message or an error.
#' @export
#' @name lolstat_key
#'
#' @examples
#' setAPIKey("fake key")
#' getAPIKey()
#' # getAPIKey("options") returns an error if set failured
#'
#' @export
setAPIKey = function(key, store = c('env','options', 'sysenv')){
  store = match.arg(store)
  err = 'Failed to set key.'
  if(store == 'env'){ # store == environment
    .session_lolstat_env$key = key
    if(.session_lolstat_env$key == key){
      message('Key stored successfully in package environment.')
    } else{
      stop(err, call. = FALSE)
    }
  }
  else if(store == 'options'){ # store == options
    options(LOLSTAT_KEY = key)
    if(options()$LOLSTAT_KEY == key){
      message('Key stored successfully in options().')
    } else{
      stop(err, call. = FALSE)
    }
  }
  else { # store == 'sysenv'
    Sys.setenv(LOLSTAT_KEY = key)
    if(Sys.getenv("LOLSTAT_KEY") == key){
      message('Key stored successfully in system environment.')
    } else {
      stop(err, call. = FALSE)
    }
  }
  invisible()
}

#' @export
#' @rdname lolstat_key
getAPIkey = function(store = c("env", 'options', 'sysenv')){
  store = if(missing(store)) c('env', 'options', 'sysenv') else
    match.arg(store)

  if("env" %in% store){
    key <- .session_lolstat_env$key
    if(!is.null(key)) return(key)
  }
  if("options" %in% store){
    key <- options()$LOLSTAT_KEY
    if(!is.null(key)) return(key)
  }
  if("sysenv" %in% store){
    key <- Sys.getenv("LOLSTAT_KEY")
    if(!is.null(key) && key != "") return(key)
  }
  wrn <- paste("LOLSTAT API key not found in package environment,",
               "global options, or system enivronment variables.")
  warning(wrn, call. = FALSE)
  NULL
}
