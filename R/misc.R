#' Gracefully say no
#' 
#' @description
#' Powered by hotheadhacker/no-as-a-service (https://github.com/hotheadhacker/no-as-a-service).
#' @param verbose A function to say no. Default to \code{cat}. Set to \code{NULL} to silently return no without saying it out loud. 
#' @return A variety of ways to say no. 
#' @seealso [.get_no()]
#' @export
.say_no <- function(verbose = cat) {
    no <- tryCatch({
        .get_no()
    }, error = function(msg){
        "No."
    })
    if (!is.null(verbose)) {
        do.call(verbose, list(no))
    }
    return(invisible(no))
}

#' Get a no
.get_no <- function() {
    httr2::request("https://naas.isalman.dev") %>% 
            httr2::req_url_path_append("no") %>% 
            httr2::req_timeout(0.5) %>%
            httr2::req_perform() %>% 
            httr2::resp_body_json() %>% 
            .$reason
}
