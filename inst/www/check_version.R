check_version<-function(){
  repo_url <- "https://api.github.com/repos/DaniloCVieira/iMESc/releases/latest"
  current_version ="iMESc-1.0.3"
  # Send a GET request to the GitHub API
  response <- httr::GET(repo_url)
  version_to_numeric <- function(version) {
    as.numeric(unlist(strsplit(gsub("iMESc-", "", version), "\\.")))
  }
  # Check if the request was successful
  if (httr::status_code(response) == 200) {


    # Parse the response to a list
    release_info <- jsonlite::fromJSON(httr::content(response, as = "text"))

    # Extract the tag name (version)
    latest_version <- release_info$tag_name

    current_version_numeric <- version_to_numeric('iMESc-1.0.4')
    latest_version_numeric <- version_to_numeric(latest_version)

    return(all(current_version_numeric >= latest_version_numeric))
  }




}

