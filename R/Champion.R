#' @title lolstat champions
#' get information about champion Id and name using DDragon API
#'
#' @examples
#' champInfo = lolstat_champions()
#'
#' @return data frame with consists of 2 column, champion ID & champion Name in English
#'
#' @export
lolstat_champions <- function() {
  url <- "https://ddragon.leagueoflegends.com/api/versions.json"
  versions = url %>% lolstat_get(waittime = 0) %>% lolstat_url_json() %>% unlist
  latestVersion = versions[1]
  message(paste0("using latest version : ", latestVersion))

  url <- paste0("http://ddragon.leagueoflegends.com/cdn/", latestVersion, "/data/en_US/champion.json")
  jsonObj = url %>% lolstat_get() %>% lolstat_url_json()
  champs <- jsonObj$data

  res <- c()
  for (i in 1:length(champs)) {
    res <- rbind(res, c(champs[[i]]$key, champs[[i]]$id))
  }
  res <- data.frame(res, stringsAsFactors = FALSE)
  colnames(res) <- c("ChampID", "ChampName")
  return(res)
}

#' @title lolstat champions id2name
#'
#' @description change champion from id to name
#'
#' @param matches result of lolstat_matchlists()
#' @param champInfo result of lolstat_champions()
#'
#' @examples matches = lolstat_champions_id2name(matches, champInfo)
#'
#' @export
lolstat_champions_id2name = function(matches, champInfo){
  return( matches %>% right_join(champInfo) %>% filter(!is.na(MatchID)) %>% select(-ChampID) )
}
