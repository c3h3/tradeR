
#' getCompanyInfo.cnyes
#'
#' function which can help you get the information (data.frame) of different companies from http://www.cnyes.com/usastock/hotprice.aspx?page=hot&kind=sp500
#' @param marketType must be in c("sp500")
#' @examples
#' getCompanyInfo.cnyes("sp500")
#' @export
getCompanyInfo.cnyes = function(marketType = "sp500"){
  queryData = list(page="hot",kind=marketType)
  url = "http://www.cnyes.com/usastock/hotprice.aspx"
  res = GET(url,query=queryData)
  dataTable = res %>%
    content(as = "text", encoding = "utf8") %>%
    htmlParse(encoding = "utf8") %>%
    readHTMLTable(stringsAsFactors=FALSE) %>%
    .$ctl05_TBstock
  names(dataTable)[2] <- "IDs"

  return(dataTable)
}
