#' @importFrom magrittr %>%
tune_params <- function() {
  get_range_from_object <- function(x) {
    if (class(x)[1] == "quant_param") {
      lower <- x$range$lower
      upper <- x$range$upper

      if (lower == unknown() | upper == unknown()) {
        vals <- "unknown"
      } else {
        vals <- paste("c(", lower, ", ", upper, ")", sep = "")
      }
    } else {
      vals <- paste("c(",
                    paste(
                      paste('"', x$values, '"', sep = ""), collapse = ", "),
                    ")", sep = "")
    }

    return(vals)
  }

  create_param_code <- function(obj) {
    if (nchar(obj) == 0) stop("Please select object.\n")
    if (!exists(obj)) stop("Selected object not found.\n")

    object <- eval(parse(text = obj))

    class_validate <- any(class(object) %in% c("model_spec", "recipe", "workflow"))
    if (!class_validate) stop("Selected object must be of class 'model_spec', 'recipe' or 'workflow'.\n", call. = FALSE)

    obj_params <- object %>% dials::parameters()

    if (length(obj_params$name) == 0) stop("There are no tuning parameters.\n")

    obj_name <- rlang::sym(obj)

    param_body <-
      obj_params %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        values = purrr::map_chr(object, get_range_from_object),
        comma = c(rep(",", n()-1), "")
      ) %>%
      dplyr::mutate(values = dplyr::case_when(
        values == "unknown" ~ paste0("finalize(", name, "(), x = NA)"),
        TRUE ~ paste0(name, "(", values, ")")
      )) %>%
      stringr::str_glue_data(
        "    `{id}` = {values}{comma}"
      )

    stringr::str_glue(
      "\n\n",
      "# Update tuning parameters\n",
      "params <- \n",
      "  {obj_name} %>% \n",
      "  parameters() %>% \n",
      "  update( \n",
      paste(param_body, collapse = "\n"),
      "\n  )\n\n"
    ) %>%
      paste(collapse = "\n")
  }

  get_doc <- rstudioapi::getActiveDocumentContext()

  obj <- get_doc$selection[[1]]$text

  param_code <-
    tryCatch(
      create_param_code(obj),
      error = function(e) message(e)
    )

  start <- get_doc$selection[[1]]$range$start[1]

  cont <- get_doc$contents
  if (start == length(cont)) {
    cont_new <- c(cont[1:(start)], param_code)
    cont_txt <- paste(cont_new, collapse = "\n")
  } else {
    cont_new <- c(cont[1:(start)], param_code, cont[(start+1):(length(cont))])
    cont_txt <- paste(cont_new, collapse = "\n")
  }

  if(!is.null(param_code)) {
    rstudioapi::insertText(rstudioapi::document_range(c(1,1), end = c(length(cont)+1,1)), text = " ", id = get_doc$id)
    rstudioapi::insertText(rstudioapi::document_range(c(1,1), c(1,1)), text = cont_txt, id = get_doc$id)
    rstudioapi::setCursorPosition(rstudioapi::document_range(c(start+2,1), end = c(start+2,1)), id = get_doc$id)
  }
}
