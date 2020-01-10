#' lolstat League
#'
#' Get summoners information in league
#'
#' for more information, refer official API page, https://developer.riotgames.com/apis#league-v4
#'
#' @param division Character, I to IV
#' @param tier Character, IRON to DIAMOND (below MASTER)
#' @param queue Character, Game type
#' @param page Integer, start index, should be larger than 0
#' @param region Character, league region
#' @param APIKey Character, if set explicitly; not needed if key is set globally. See \code{\link{setAPIKey}}.
#'
#' @return a data frame consists of summoners name and accountID of Each
#'
#' @examples
#' \dontrun{
#'  # use setAPIKey() to set APIKey
#'  setAPIKey('myAPIKey', 'sysenv')
#'
#'  # getAPIKey() to get APIKey
#'  APIKey = getAPIKey('sysenv')
#'
#'  # get name of All Platinum I summoners in KR region.
#'  P1 = lolstat_league_summoners('I','PLATINUM', 'RANKED_SOLO_5x5', 1, 'KR', APIKey)
#'
#' }
#'
#' @export
lolstat_league_summoners = function(division , tier, queue, page = 1, region, APIKey){
  summonerName = lolstat_league_summonerName(division, tier, queue, page, region, APIKey)
  accountId = lolstat_league_summonerName_AccountID(summonerName, region, APIKey)
  return(data.frame(accountId = accountId, summonerName = summonerName))
}

lolstat_league_summonerName = function(division, tier, queue, page, region, APIKey){
  x = lolstat_league_url_league2name(division, tier, queue, page, region, APIKey) %>% lolstat_get()
  summoners = x %>% lolstat_url_json()

  if("error" %in% names(x$data))
    stop(paste("API error: ", x$data$error[1]), call. = FALSE)

  summonerName = sapply(1:length(summoners), function(i){
    summoners[[i]]$summonerName
  })
  return(summonerName)
}

lolstat_league_url_league2name = function(division, tier, queue, page, region, APIKey){
  key_check(APIKey)

  if(!tier %in% c("IRON", "BRONZE", "SILVER", "GOLD", "PLATINUM", "DIAMOND")){
    stop('tier : IRON, BRONZE, SILVER, GOLD, PLATINUM, DIAMOND', call. = FALSE)
  }
  if(!division %in% c("I",'II','III','IV')){
    stop('division : I, II, III, IV', call. = FALSE)
  }
  if(!queue %in% c("RANKED_SOLO_5x5", "RANKED_FLEX_SR", "RANKED_FLEX_TT")){
    stop('queue : RANKED_SOLO_5x5, RANKED_FLEX_SR, RANKED_FLEX_TT', call. = FALSE)
  }
  if(!is.numeric(page)){
    stop('page : numeric', call. = FALSE)
  }
  if(!page >= 1){
    stop('page : >= 1', call. = FALSE)
  }
  if(!region %in% c('RU', 'KR', 'BR1', 'OC1', 'JP1', 'NA1', 'EUN1', 'EUW1', 'TR1', 'LA1', 'LA2')){
    stop('region : BR1, EUN1, EUW1, JP1, KR, LA1, LA2, NA1, OC1, RU, TR1', call. = FALSE)
  }

  url <- paste0("https://",region,".api.riotgames.com/lol/league/v4/entries/",queue,"/",tier,"/", division, "?page=",page,"&api_key=", APIKey)
  return(url)
}

lolstat_league_summonerName_AccountID = function(summonerName, region, APIKey){
  message('Each request will take 1.5 seconds per summonerName')
  message(paste0('Total ', 1.5*length(summonerName), ' seconds need'))
  x = c()
  for(i in 1:length(summonerName)){
    jsonobj = lolstat_league_url_name2id(summonerName[i], region, APIKey) %>% lolstat_get() %>% lolstat_url_json()
    x[i] = jsonobj$accountId
  }
  return(x)
}

lolstat_league_url_name2id = function(name, region, APIKey){
  key_check(APIKey)
  url <- URLencode(paste0("https://",region,".api.riotgames.com/lol/summoner/v4/summoners/by-name/", name, "?api_key=", APIKey))
  return(url)
}
