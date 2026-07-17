#' Connect to SQLite database
#' @param path Path to SQLite database. If not given, configuration file will be searched.
#' @export
.dbconn <- function(path = NULL) {
    if (!is.null(path)) {
        return(DBI::dbConnect(RSQLite::SQLite(), path))
    }
    path <- list.files(read_config("util")$database, read_config("project")$name, full.names = T)

    if (length(path) == 0) {
        db.path <- file.path(read_config("util")$database, paste0(read_config("project")$name, ".sqlite"))
        cli::cli_alert_info("No database file found. Creating new one: {db.path}")
        suppressMessages({
            dir.create(dirname(db.path))
            file.create(db.path)
        })
        path <- list.files(read_config("util")$database, read_config("project")$name, full.names = T)
    }
    if (length(path) > 1) {stop("There are more than one database file matching project name under configured database path.")}
    return(DBI::dbConnect(RSQLite::SQLite(), path[[1]]))
}

#' Generate Signature for Database Change
#'
#' @param table Table name subjected to change
#' @param type Change type
#' @param request Request ID
#' @param con Connection to database as produced by `DBI::dbConnect()`
#' @param time Time of change. Default to current time.
#' @param signature.table Table name to write signature.
#' @export
db_sign <- function(
    table,
    type,
    msg,
    request = request.id,
    con = .dbconn(),
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    signature.table = "signature"
) {
    if (!type %in% c("Initial", "Append", "Amend", "Del", "Hard", "Manual")) {
        stop(paste0("Unaccepted type: ", type))
    }

    signature <- data.frame(
        time = time,
        table = table,
        type = type,
        msg = msg,
        requestId = request
    )

    if (!signature.table %in% DBI::dbListTables(con)) {
        signature <- data.frame(
            time = time,
            table = table,
            type = type,
            msg = msg,
            requestId = request
        ) %>%
            dplyr::filter(!is.na(time))
        DBI::dbWriteTable(con, signature.table, signature, overwrite = T)
    } else {
        DBI::dbWriteTable(con, signature.table, signature, append = T)
    }
    DBI::dbDisconnect(con)
    return(invisible(NULL))
}

#' Execute YAML Change Request
#'
#' @param request.filename Path to the YAML request file
#' @export
db_exe_request <- function(request.filename) {
    request.file <- paste0("data/metadata/request/", request.filename, ".yaml")

    requests <- yaml::yaml.load_file(request.file) %>%
        purrr::flatten() %>%
        rlist::list.map(data.frame(.)) %>%
        dplyr::bind_rows() %>%
        tibble::tibble() %>%
        dplyr::mutate(
            requestId = paste0(
                request.filename,
                "_",
                stringr::str_pad(1:nrow(.), 2, pad = "0")
            ),
            status = ifelse(
                requestId %in%
                    (.dbconn() %>%
                        dplyr::tbl("signature") %>%
                        dplyr::collect() %>%
                        dplyr::pull(requestId)),
                "complete",
                "pending"
            )
        )

    purrr::walk(1:nrow(requests), \(x) {
        request <- requests[x, ]

        if (request$status == "complete") {
            return()
        }

        request.id <- paste0(request.filename, "_", stringr::str_pad(x, 2, pad = "0"))

        table <- request$table
        type <- request$type
        filter <- request$filter
        if (!is.null(filter)) {
            filter_expr <- rlang::parse_expr(filter)
        }
        msg <- request$msg
        amend_col <- request$amend_col
        amend_mutate <- request$amend_mutate
        hard_execute <- request$hard_execute

        if (!type %in% c("Del", "Amend", "Initial", "Append", "Hard")) {
            stop(paste0("Unknown type: ", type))
        }

        dbconn <- .dbconn()

        target <- dplyr::tbl(dbconn, table) %>% dplyr::collect()

        if (type == "Del") {
            target.updated <- target %>%
                dplyr::mutate(
                    del = ifelse(
                        rlang::eval_tidy(filter_expr, data = .),
                        paste(request.id),
                        del
                    )
                )
        }

        if (type == "Amend") {
            target.markedAsAmend <- target %>%
                dplyr::mutate(
                    amend = ifelse(
                        rlang::eval_tidy(filter_expr, data = .),
                        paste(request.id),
                        amend
                    )
                )

            target.updated <-
                paste0(
                    "target.markedAsAmend %>% mutate(",
                    amend_col,
                    " = case_when(", # mutate + case_when to manipulate
                    filter,
                    " ~ ", # filter expression to select target
                    amend_mutate, # mutate expression
                    ", .default = ",
                    amend_col,
                    "))", # set default for unchanged
                    collapse = ""
                ) %>%
                rlang::parse_expr() %>%
                rlang::eval_tidy()
        }

        if (type == "Hard") {
            target.updated <-
                hard_execute %>%
                rlang::parse_expr() %>%
                rlang::eval_tidy()
        }

        print(t(request))
        cat("\nParsed Expression:\n\n")

        if (!is.null(filter)) {
            cat("Filter\n")
            print(rlang::parse_expr(filter))
        }
        if (!is.null(amend_mutate)) {
            cat("\nMutate\n")
            print(rlang::parse_expr(amend_mutate))
        }
        if (!is.null(hard_execute)) {
            cat("\nHard\n")
            print(rlang::parse_expr(hard_execute))
            cat("\n")
        }

        exec <- readline(
            "Execute request? Type `y` to continue, type 'v' to preview. \n>>> "
        )

        if (exec != "y") {
            if (exec == "v") {
                View(target.updated)
                stop("Preview result")
            } else {
                stop("Unknown response, abort.")
            }
        }

        db_sign(table, type, msg, request.id)
        DBI::dbWriteTable(dbconn, table, target.updated, overwrite = T)
    })

    print("All request has been completed.")
    DBI::dbDisconnect(dbconn)
}

#' Write Table
#'
#' @param tbl Table object to write.
#' @param table Table name.
#' @param msg Change message.
#' @param request.id Request ID associated to this change.
#' @export
db_write <- function(tbl, table, msg, request.id) {
    dbconn <- .dbconn()

    db_sign(table, "Manual", msg, request.id)
    DBI::dbWriteTable(dbconn, table, tbl, overwrite = T)
    DBI::dbDisconnect(dbconn)
}

#' Generate Snapshot from All Tables
db_snapshot <- function() {
    library(tidyverse)
    library(DBI)

    dbconn <- .dbconn()

    dir <- paste0(
        "data/metadata/latest/",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        "/"
    )
    dir.create(dir)

    table <- DBI::dbListTables(dbconn)
    table <- table[table != "signature"]

    purrr::walk(table, \(tbl.name) {
        tbl <- dplyr::tbl(dbconn, tbl.name) %>% dplyr::collect()

        write.table(
            tbl,
            paste0(
                dir,
                tbl.name,
                ".csv"
            ),
            sep = ";",
            row.names = F,
            col.names = T
        )
    })

    signature <- dplyr::tbl(dbconn, "signature") %>% dplyr::collect()
    write.table(
        signature,
        paste0(dir, "signature.csv"),
        sep = ";",
        row.names = F,
        col.names = T
    )
    DBI::dbDisconnect(dbconn)
}

#' Return Signature of the Last Request
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
db_show_last_request <- function() {
    dbconn <- .dbconn()

    lastRequest <- dplyr::tbl(dbconn, "signature") %>%
        dplyr::collect() %>%
        dplyr::arrange(desc(requestId)) %>%
        dplyr::pull(requestId) %>%
        .[1]

    return(lastRequest)
    DBI::dbDisconnect(dbconn)
}

#' Pull Table
#' @param table Table name. Type "show_table" to display all available table names in database.
#' @param cleaned Clean table, rows marked in `del` column will be dropped.
#' @param formatting Format table. Utilize table-specific formatting function to format the table if available. Formatting functions are always named under the rule `format_` + table name.
#' @param format Argument to pass over to formatting functions. If available different formatting could be chosen.
#' @param silent Logical. Whether to silent messages from internal function call.
#'
#' @return A data.frame of requested table.
#'
#' @export
db_pull <- function(
    table,
    dbconn = .dbconn(),
    cleaned = T,
    formatting = T,
    format = NULL,
    silent = F
) {
    if (table == "show_table") {
        cli::cli_alert_info("The database has tables:\n\n")
        cat(DBI::dbListTables(dbconn), sep = "\n")
        return(invisible(NULL))
    }
    tbl <- dplyr::tbl(dbconn, table) %>% dplyr::collect()

    if (cleaned) {
        if ("del" %in% colnames(tbl)) {
            tbl <- tbl %>%
                dplyr::filter(is.na(del)) %>%
                dplyr::select(-c("del"))
        }
    }

    if (formatting) {
        tbl_formatter <- paste0("format_", table)
        # if (tbl_formatter %in% ls(paste0("package:", read_config("project")$name))) {
        #     # check availability of formatting function
        #     if (!silent) {
        #         cli::cli_alert_info("Found formatter: {tbl_formatter}")
        #     }
        #     if (is.null(format)) {
        #         format <- ""
        #     } else {
        #         format <- paste0(", format = ", format)
        #     }
        #     if (!silent) {
        #         cli::cli_alert_info(
        #             "Executing: {tbl_formatter}(tbl, formatting = T{format})"
        #         )
        #     }
        #     tbl <- paste0(
        #         tbl_formatter,
        #         "(tbl, formatting = T",
        #         format,
        #         ")"
        #     ) %>%
        #         rlang::parse_expr() %>%
        #         rlang::eval_tidy()
        # }
    }
    return(tbl)
}
