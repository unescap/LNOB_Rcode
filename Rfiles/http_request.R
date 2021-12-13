##### THIS WHOLE FILE IS TBD codes #####

if(! require("httr")) install.packages("httr")
require("httr")

if(! require("jsonlite")) install.packages("jsonlite")
require("jsonlite")



# http_post <- function (endpoint, request_body, api_base, key) {
#   #print(request_body)
#   request_body_json <- toJSON(request_body, auto_unbox = TRUE)
#   print(paste(api_base,endpoint, sep=""))
#   
#   result <- POST(paste(api_base,endpoint, sep=""),
#                body = request_body_json,
#                add_headers(`Content-Type`="application/json",`api-key`= key))
#   #print(result)
#   #return("ok")
#   return (content(result))
#   # return(result)
# }

http_post <- function (endpoint, request_body, api_base, key) {
  h2 <- handle('')
  #print(request_body)
  request_body_json <- toJSON(request_body, auto_unbox = TRUE)
  print(paste(api_base,endpoint, sep=""))
  
  result <- POST(paste(api_base,endpoint, sep=""),
                 body = request_body_json,
                 add_headers(`Content-Type`="application/json",`api-key`= key),
                 handle=h2)
  #print(result)
  #return("ok")
  return (content(result))
  # return(result)
}

http_get <- function (endpoint, api_base, key) {
  call <- paste(api_base,endpoint, sep="")
  h2 <- handle('')
  response <- GET(call, add_headers(`Content-Type`="application/json",`api-key`= key),
                  handle=h2)
  response_text <- content(response, "text")
  response_json <- fromJSON(response_text, flatten = TRUE)
  # response_df <- as.data.frame(response_json)
  # View(response_df)
  return (response_json)
}
# http_get("indicator_taxonomies")

http_publish <- function (country, api_base, key) {
  # print(request_body)
  # request_body_json <- toJSON(request_body, auto_unbox = TRUE)
  h2 <- handle('')
  callText<-paste(api_base, "publish-data?geo=", country, sep="")
  
  result <- GET(callText,
                 add_headers(`Content-Type`="application/json", `api-key`= key),
                handle=h2)
  

  print(callText)
  #return("ok")
  return (content(result))
  # return(result)
}


