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


###########################################################
##Interrogating Myself

userData = fromJSON("https://api.github.com/users/andrewtobin99")

userData$followers ## I keep a low profile as you can see :'(

myFollowers = fromJSON("https://api.github.com/users/andrewtobin99/followers")

myFollowers$login

following = fromJSON("https://api.github.com/users/andrewtobin99/following")
following$login #People I'm following 

userData$public_repos #Shows count of my repositories that are public

repos = fromJSON("https://api.github.com/users/andrewtobin99/repos")
repos$name #Prints names of all my public repositories
repos$created_at #Gives the date my repositories were made 
repos$full_name #gives the full names of my repositories

#######################################################################
## Interrogating Lennart: trending user who makes Gatsby Themes

LennartData = GET("https://api.github.com/users/LekoArts/followers?per_page=100;", github_token)
stop_for_status(LennartData)
extract = content(LennartData)

githubDB = jsonlite::fromJSON(jsonlite::toJSON(extract))
githubDB$login


id = githubDB$login
user_ids = c(id)

## Generates Dataframe of users data
users = c()
usersDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer()
)

for(i in 1:length(user_ids))
{
  
  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingRequest = GET(followingURL, gtoken)
  followingContent = content(followingRequest)
  
  #Checks if the user following Lennart has followers
  #Skips over them if there is no follower as they may be bots
  if(length(followingContent) == 0)
  {
    next
  }
  
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login
  
  #Loops through github users who follow Lennarts
  for (j in 1:length(followingLogin))
  {
    # Ensures there's no duplicates
    if (is.element(followingLogin[j], users) == FALSE)
    {
      #Adds the user to the list
      users[length(users) + 1] = followingLogin[j]
      
      #Collects each users data
      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, gtoken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      #Stores number of other github accounts the user is following
      numberFollowing = followingDF2$following
      
      # Stores number of other github accounts following the user 
      numberOfFollowers = followingDF2$followers
      
      #Count of the number of repositories each user has
      numberOfRepos = followingDF2$public_repos
      
      #Stores the year each user joined github
      yearJoined = substr(followingDF2$created_at, start = 1, stop = 4)
      
      #Appends users data to a new row in the DF
      usersDB[nrow(usersDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)
      
    }
    next
  }
  #100 user cutoff
  if(length(users) > 100)
  {
    break
  }
  next
}
