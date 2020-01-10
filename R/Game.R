#' @title lolstat Game
#'
#' @description this will return entire match object
#'
#' @param matchID identifier of match
#' @param region Character, league region
#' @param APIKey Character, if set explicitly; not needed if key is set globally. See \code{\link{setAPIKey}}.
#'
#'
#' @export
lolstat_game = function(matchID, region, APIKey){
  url <- paste0("https://", region, ".api.riotgames.com/lol/match/v4/matches/", matchID, "?api_key=", APIKey)
  matchObj = url %>% lolstat_get() %>% lolstat_url_json()
  return(matchObj)
}


