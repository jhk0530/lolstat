#' lolstat Matchlist
#'
#'
#' for more information, refer official API page, https://developer.riotgames.com/apis#match-v4
#'
#' @description get all matchlists
#'
#' @param accountId Character, See \code{\link{lolstat_league_summoners}}.
#' @param region Character, league region
#' @param APIKey Character, if set explicitly; not needed if key is set globally. See \code{\link{setAPIKey}}.
#' @param champion Numeric value : See \code{\link{lolstat_champions}}.
#' @param queue Numeric value 420 : Solo Rank, 430 : Normal, 440 : Team Rank, if NULL : all of these are applied.
#' @param beginTime Numeric value in epoch mileseconds. refer official API page
#' @param endTime Numeric value in epoch mileseconds. refer official API page
#' @param beginIndex Numeric value to howmuch of matchlist will be gathered, default is 0 and should be < 100.
#' @param endIndex Numeric value to howmuch of matchlist will be gathered, default is beginIndex + 100
#'
#' @examples
#' \dontrun{
#'  # use setAPIKey() to set APIKey
#'  setAPIKey('myAPIKey', 'sysenv')
#'
#'  # getAPIKey() to get APIKey
#'  APIKey = getAPIKey('sysenv')
#'
#'  # get recent matchlist (up to 100) of first Platinum I summoners matchlist in KR region.
#'  # P1 = lolstat_league_summoners('I','PLATINUM', 'RANKED_SOLO_5x5', 1, 'KR', APIKey)
#'  matches = lolstat_matchlists(P1$accountId, 'KR', APIKey )
#' }
#'


#' @export
lolstat_matchlists= function(accountIds, region, APIKey, champion = NULL, queue = NULL, endTime = NULL, beginTime = NULL, endIndex = NULL, beginIndex = NULL){
  matches = c()
  for(i in 1:length(accountIds)){
    matches = matches %>% rbind(lolstat_matchlist_accountId(accountIds[i], region, APIKey, champion, queue, endTime, beginTime, endIndex, beginIndex))
  }
  return(matches)
}

lolstat_matchlist_accountId = function(accountId, region, APIKey, champion, queue, endTime, beginTime, endIndex, beginIndex) {
  url <- paste0("https://",region,".api.riotgames.com/lol/match/v4/matchlists/by-account/", accountId, "?")

  if (!is.null(champion)) {
    url <- paste0(url, "champion=", champion, "&")
  }

  if (!is.null(queue)) {
    url <- paste0(url, "queue=", queue, "&")
  }

  if (!is.null(endTime)) {
    url <- paste0(url, "endTime=", endTime, "&")
  }

  if (!is.null(beginTime)) {
    url <- paste0(url, "beginTime=", beginTime, "&")
  }

  if (!is.null(endIndex)) {
    url <- paste0(url, "endIndex=", endIndex, "&")
  }

  if (!is.null(beginIndex)) {
    url <- paste0(url, "beginIndex=", beginIndex, "&")
  }

  jsonObj = paste0(url, "api_key=", APIKey) %>% lolstat_get %>% lolstat_url_json()

  # matches, startIndex, endIndex, totalGames

  matches <- jsonObj$matches
  if(is.null(queue)){queuelist = c(420, 430, 440)}
  matches <- data.frame(matrix(unlist(matches), nrow = length(matches), byrow = T), stringsAsFactors = FALSE)
  colnames(matches) <- c("platformId", "MatchID", "ChampID", "queue", "season", "timestamp", "role", "lane")
  matches = matches %>% select(MatchID, champion, queue) %>% filter(queue %in% queuelist) # remove redundant columns
  return(matches)
}


