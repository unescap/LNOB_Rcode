##### THIS WHOLE FILE IS TBD codes #####

if(! require("httr")) install.packages("httr")
require("httr")

if(! require("jsonlite")) install.packages("jsonlite")
require("jsonlite")



http_post <- function (endpoint, request_body, api_base, key) {
  request_body_json <- toJSON(request_body, auto_unbox = TRUE)
  print(paste(api_base,endpoint, sep=""))
  print(request_body_json)
  result <- POST(paste(api_base,endpoint, sep=""),
               body = request_body_json,
               add_headers(`Content-Type`="application/json",`api-key`= key))
  print(result)
  return (content(result))
}

http_get <- function (endpoint, api_base, key) {
  call <- paste(api_base,endpoint, sep="")
  response <- GET(call, add_headers(`Content-Type`="application/json",`api-key`= key))
  response_text <- content(response, "text")
  response_json <- fromJSON(response_text, flatten = TRUE)
  # response_df <- as.data.frame(response_json)
  # View(response_df)
  return (response_json)
}
# http_get("indicator_taxonomies")
