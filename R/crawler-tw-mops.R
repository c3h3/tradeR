
#' getMartketTypeCodes.mops
#'
#' function which can help you get the codes of different market types in http://mops.twse.com.tw/mops/web/t51sb01
#' @examples
#' getMartketTypeCodes.mops()
#' @export
getMartketTypeCodes.mops = function(){
  list("TWSE"="sii",
       "TWOTC"="otc",
       "TWROTC"="rotc",
       "TWPUB"="pub")
}

#' getMarketInfo.mops
#'
#' function which can help you get the information (data.frame) of different market types from http://mops.twse.com.tw/mops/web/t51sb01
#' @param marketType must be in c("TWSE", "TWOTC", "TWROTC", "TWPUB")
#' @examples
#' getMarketInfo.mops("TWOTC")
#' @export
getMarketInfo.mops = function(marketType = "TWSE"){
  MOPS_MARKET_TYPES = getMartketTypeCodes.mops()
  assert_that(marketType %in% names(MOPS_MARKET_TYPES),
              msg = 'marketType must be in c("TWSE", "TWOTC", "TWROTC", "TWPUB")' )

  postData = list(
    encodeURIComponent="1",
    step="2",
    firstin="1",
    TYPEK=MOPS_MARKET_TYPES[[marketType]],
    code="01"
  )

  postUrl = "http://mops.twse.com.tw/mops/web/ajax_t51sb01"

  res = POST(postUrl,body = postData, encode = "form")
  df = res %>%
    content(as = "text", encoding = "utf8") %>%
    htmlParse(encoding = "utf8") %>%
    xpathApply("//select[@name='code']/option", function(node){
      list(category=xmlValue(node),code=xmlAttrs(node,"value")[1])
    }) %>%
    do.call(rbind,.) %>%
    as.data.frame()
  df[1,2] = ""
  df$marketType = marketType
  df$marketTypeCode = MOPS_MARKET_TYPES[[marketType]]

  return(df)
}


#' getCompanyInfo.mops
#'
#' function which can help you get the information (data.frame) of different companies from http://mops.twse.com.tw/mops/web/t51sb01
#' @param marketType must be in c("TWSE", "TWOTC", "TWROTC", "TWPUB")
#' @param code codes in http://mops.twse.com.tw/mops/web/t51sb01
#' @examples
#' getCompanyInfo.mops("TWOTC","02")
#' @export
getCompanyInfo.mops = function(marketType = "TWSE",code = "01"){
  MOPS_MARKET_TYPES = getMartketTypeCodes.mops()

  assert_that(marketType %in% names(MOPS_MARKET_TYPES),
              msg = 'marketType must be in c("TWSE", "TWOTC", "TWROTC", "TWPUB")' )

  postData = list(
    encodeURIComponent="1",
    step="1",
    firstin="1",
    TYPEK=MOPS_MARKET_TYPES[[marketType]],
    code=code
  )

  postData

  postUrl = "http://mops.twse.com.tw/mops/web/ajax_t51sb01"

  res = POST(postUrl,body = postData, encode = "form")

  tables = res %>%
    content(as = "text", encoding="utf8") %>%
    htmlParse(encoding = "utf8") %>%
    readHTMLTable(stringsAsFactors=FALSE)

  # print(length(tables))

  if (length(tables)>0){
    dataTable = tables[[2]]
    dataTable[,1] = gsub("[$,\xc2\xa0]", "", dataTable[,1])
    dataTable = dataTable[grep("[0-9]+", dataTable[,1]),]
    names(dataTable)[1]<-"IDs"
    # print(code)
    # print(dim(dataTable))
    return(dataTable)
  }else{
    return(NULL)
  }
}


#' getCategoryCompanyIDs.mops
#'
#' function which can help you get the company categories and related stock IDs from http://mops.twse.com.tw/mops/web/t51sb01
#' @param marketType must be in c("TWSE", "TWOTC", "TWROTC", "TWPUB")
#' @examples
#' getCategoryCompanyIDs.mops("TWOTC")
#' @export
getCategoryCompanyIDs.mops = function(marketType = "TWSE"){
  MOPS_MARKET_TYPES = getMartketTypeCodes.mops()

  assert_that(marketType %in% names(MOPS_MARKET_TYPES),
              msg = 'marketType must be in c("TWSE", "TWOTC", "TWROTC", "TWPUB")' )

  res = getMarketInfo.mops(marketType) %>%
    filter(code != "" ) %>%
    mutate(data=map2(marketType,code,getCompanyInfo.mops)) %>%
    mutate(category = unlist(category),IDs = map(data,~.$IDs), N_IDs = map(IDs,length)) %>%
    filter(N_IDs > 0) %>%
    select(category,IDs) %>%
    unnest(IDs)

  return(res)
}
