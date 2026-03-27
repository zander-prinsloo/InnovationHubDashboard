#' Hugging Face model file proxy for @ai4data/search (Transformers.js)
#'
#' Shiny's `registerDataObj` only routes `dataobj/<name>` or `dataobj/<name>/<one-segment>`;
#' long HF paths make `downloads$get("")` error. This app serves the allowlisted files at
#' `{basePath}api/hf-proxy/<model>/resolve/<rev>/...` via a top-level HTTP handler instead.
#' On Posit Connect, `basePath` is the app URL prefix (e.g. `/content/abc123/`).
#'
#' @noRd
NULL

#' Browser base URL for Transformers.js (leading slash, trailing slash before `api/`).
#' @noRd
hf_proxy_client_url <- function(pathname) {
  if (
    is.null(pathname) || length(pathname) == 0 ||
      (length(pathname) == 1 && is.na(pathname)) ||
      !nzchar(pathname)
  ) {
    return("/api/hf-proxy/")
  }
  p <- sub("/+$", "", pathname[1])
  if (!nzchar(p) || p == "/") {
    return("/api/hf-proxy/")
  }
  paste0(p, "/api/hf-proxy/")
}

hf_proxy_config <- function() {
  hf_model_id <- "avsolatorio/GIST-small-Embedding-v0"
  hf_revision <- "main"
  list(
    path_prefix = paste0(hf_model_id, "/resolve/", hf_revision),
    allowed_suffix = c(
      "config.json",
      "tokenizer.json",
      "tokenizer_config.json",
      "special_tokens_map.json",
      "onnx/model_quantized.onnx"
    )
  )
}

#' @importFrom shiny httpResponse parseQueryString
#' @noRd
hf_proxy_response_list <- function(req, data) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  if (!identical(toupper(req$REQUEST_METHOD %||% "GET"), "GET")) {
    return(list(
      status = 405L,
      headers = list(
        "Content-Type" = "text/plain; charset=UTF-8",
        "Allow" = "GET"
      ),
      body = charToRaw("Method Not Allowed")
    ))
  }
  path <- req$PATH_INFO %||% ""
  path <- sub("^/+", "", path)
  path <- utils::URLdecode(path)
  # Strip app base prefix (e.g. Posit Connect: content/.../api/hf-proxy/...)
  path <- sub("^.*?api/hf-proxy/", "", path)
  path <- sub("^dataobj/hfproxy/?", "", path)
  if (!nzchar(path) || !startsWith(path, data$path_prefix)) {
    qs <- shiny::parseQueryString(req$QUERY_STRING %||% "")
    nonce <- qs$nonce %||% ""
    if (nzchar(nonce) && grepl("/", nonce, fixed = TRUE)) {
      recovered <- sub("^[0-9a-f]+/", "", nonce, ignore.case = TRUE)
      if (nzchar(recovered) && startsWith(recovered, data$path_prefix)) {
        path <- recovered
      }
    }
  }
  if (!nzchar(path) || !startsWith(path, data$path_prefix)) {
    return(list(
      status = 403L,
      headers = list("Content-Type" = "text/plain; charset=UTF-8"),
      body = charToRaw("Forbidden")
    ))
  }
  suffix <- sub(paste0("^", data$path_prefix, "/"), "", path, fixed = FALSE)
  if (!nzchar(suffix) || suffix == data$path_prefix) {
    return(list(
      status = 403L,
      headers = list("Content-Type" = "text/plain; charset=UTF-8"),
      body = charToRaw("Forbidden")
    ))
  }
  if (!suffix %in% data$allowed_suffix) {
    return(list(
      status = 403L,
      headers = list("Content-Type" = "text/plain; charset=UTF-8"),
      body = charToRaw("Forbidden")
    ))
  }
  upstream <- paste0("https://huggingface.co/", path)
  resp <- tryCatch(
    httr2::request(upstream)
    |> httr2::req_headers(
      "User-Agent" = "shiny-hf-proxy (ai4data-search; +https://huggingface.co)"
    )
    |> httr2::req_options(followlocation = TRUE)
    |> httr2::req_perform(),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    return(list(
      status = 502L,
      headers = list("Content-Type" = "text/plain; charset=UTF-8"),
      body = charToRaw(paste("Upstream fetch failed:", conditionMessage(resp)))
    ))
  }
  st <- httr2::resp_status(resp)
  if (st >= 400L) {
    return(list(
      status = st,
      headers = list("Content-Type" = "text/plain; charset=UTF-8"),
      body = charToRaw(paste("Upstream HTTP", st))
    ))
  }
  content_type <- httr2::resp_header(resp, "content-type")
  if (is.null(content_type) || !nzchar(content_type)) {
    content_type <- "application/octet-stream"
  }
  list(
    status = st,
    headers = list(
      "Content-Type" = content_type,
      "Cache-Control" = "public, max-age=3600",
      "Access-Control-Allow-Origin" = "*"
    ),
    body = httr2::resp_body_raw(resp)
  )
}

#' @noRd
hf_proxy_list_to_http_response <- function(x) {
  ct <- x$headers[["Content-Type"]]
  if (is.null(ct) || !nzchar(ct)) {
    ct <- "application/octet-stream"
  }
  extra <- x$headers
  extra[["Content-Type"]] <- NULL
  shiny::httpResponse(
    status = x$status,
    content_type = ct,
    content = x$body,
    headers = extra
  )
}

#' Top-level Shiny HTTP handler entry (see run_app).
#' @noRd
hf_proxy_http_handler <- function(req) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  pi <- req$PATH_INFO %||% ""
  if (!grepl("/api/hf-proxy(/|$)", pi)) {
    return(NULL)
  }
  data <- hf_proxy_config()
  x <- hf_proxy_response_list(req, data)
  hf_proxy_list_to_http_response(x)
}
