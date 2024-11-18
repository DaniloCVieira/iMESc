check_version<-function(){
  repo_url <- "https://api.github.com/repos/DaniloCVieira/iMESc-Desktop/releases/latest"
  current_version =version
  # Send a GET request to the GitHub API
  response <- httr::GET(repo_url)

  # Check if the request was successful
  if (httr::status_code(response) == 200) {


    # Parse the response to a list
    release_info <- jsonlite::fromJSON(httr::content(response, as = "text"))

    # Extract the tag name (version)
    latest_version <- release_info$tag_name
    latest_version<-gsub("iMESc-Desktop-win-", "", latest_version)


    result<-current_version!=latest_version
    attr(result,"current")<-current_version
    attr(result,"latest")<-latest_version
    return(result)
  }




}


