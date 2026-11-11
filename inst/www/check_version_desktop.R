# TRUE (with the current and latest versions as attributes) when a newer iMESc-Desktop release
# exists; FALSE otherwise. Without internet, a slow connection or an unexpected answer from GitHub
# it returns FALSE: the check must never stop the app from opening.
check_version<-function(){
  repo_url <- "https://api.github.com/repos/DaniloCVieira/iMESc-Desktop/releases/latest"
  current_version =version
  tryCatch({
    # Send a GET request to the GitHub API
    response <- httr::GET(repo_url, httr::timeout(5))

    # Check if the request was successful
    if (httr::status_code(response) != 200) return(FALSE)

    # Parse the response to a list
    release_info <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))

    # Extract the tag name (version)
    latest_version <- release_info$tag_name
    if (!is.character(latest_version) || length(latest_version) != 1) return(FALSE)
    latest_version<-gsub("iMESc-Desktop-win-", "", latest_version)

    result<-isTRUE(current_version!=latest_version)
    attr(result,"current")<-current_version
    attr(result,"latest")<-latest_version
    result
  }, error = function(e) FALSE)
}


