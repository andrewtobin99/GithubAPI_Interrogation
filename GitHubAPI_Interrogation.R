##install.packages("jsonlite")
library(jsonlite)
##install.packages("httpuv")
library(httpuv)
##install.packages("httr")
library(httr)

oauth_endpoints("github")

myapp <- oauth_app(appname = "Software_Eng_Andrew_Tobin",
                  key = "3c2f27cf86cf7c060134",
                  secret = "8550d4920a17f3f95b1ad3b18b4f9f404fba3f3e")

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 
