#' Depreacated. Use Config pkg
#' @export
read_config <- function(field, silent = F) {
    config <- yaml::read_yaml("_project.config.yaml")
    if (!field %in% names(config)) {
        if (silent) {
            return(NULL)
        } else {
            cli::cli_abort(paste0("Field not found in configuration file: ", field))
        }
    } else {
        return(config[[field]])
    }
}

#' Load cache files
#'
#' Load cache file according to file name under the cache folder specified by user configuration.
#' @export
load_cache <- function(name) {
    cache_folder <- read_config("cache_folder", T)
    if (is.null(cache_folder) | cache_folder == "") {
        cache_folder <- "cache"
    }
    target_file <- list.files(
        cache_folder,
        pattern = name,
        full.names = T,
        recursive = T
    )
    if (length(target_file) > 1) {
        cli::cli_alert_warning(paste0(
            "Found more than one cache file under pattern '",
            name,
            "', loading '",
            target_file[[1]],
            "'.\n\nOther potential files:\n",
            paste(target_file[-1], collapse = "\n")
        ))
    }
    if (length(target_file) == 0) {
        return(NULL)
    }
    cli::cli_alert_info("Loading cache: {target_file[[1]]}")
    return(readRDS(target_file[[1]]))
}

#' Write Config YAML
#' @export
write_params_yaml <- function(yaml_path, name, ...) {
    yaml_path <- ifelse(
        stringr::str_detect(yaml_path, "\\.yml$", T),
        paste0(yaml_path, ".yml"),
        yaml_path
    )
    params <- list(pars = list(name = name, ...))
    yaml::write_yaml(params, yaml_path)
    cli::cli_alert_success("YAML config file written at {yaml_path}")
    return(yaml_path)
}

#' Create Configuration File
#'
#' Several functions in this package requires user-specific configuration to work. Setting up a configuration file could reduce repetitive manual input in workflow.
#' @export
create_config <- function() {
    if (length(list.files(".", "^\\_project\\.config\\.yaml$")) > 0) {
        cli::cli_alert_info(paste0(
            "Configuration file already exists: ",
            list.files(".", "_project.config.yaml")[[1]],
            ", overwrite?"
        ))
        resp <- readline("Y/n >>> ")
        if (!resp %in% c("Y", "y")) {
            cli::cli_abort("Aborted. Please modify the existing configuration.")
        }
    }
    config <- list(
        "project" = list(
            "name" = ""
        ),
        "util" = list(
            "database" = "data/database",
            "ABGD_executable" = "",
            "cache_folder" = ""
        )
    )
    yaml::write_yaml(config, "_project.config.yaml")
}
