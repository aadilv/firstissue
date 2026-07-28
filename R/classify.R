classify_issue <- function(title, body, labels) {
  prompt <- paste0(
    "Rate this GitHub issue for beginner friendliness. Respond with JSON only.\n\n",
    "Title: ", title, "\n",
    "Labels: ", if (nchar(labels) > 0) labels else "none", "\n",
    "Body: ", substr(body, 1, 400), "\n\n",
    '{"score": <1-5>, "reason": "<one sentence max>"}'
  )

  resp <- httr2::request("https://api.groq.com/openai/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", Sys.getenv("GROQ_API_KEY")),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(
      model       = "llama-3.3-70b-versatile",
      messages    = list(list(role = "user", content = prompt)),
      max_tokens  = 120,
      temperature = 0
    )) |>
    httr2::req_retry(max_tries = 2) |>
    httr2::req_perform()

  raw  <- httr2::resp_body_json(resp)
  text <- raw$choices[[1]]$message$content

  # model sometimes wraps response in fences despite instructions
  text <- gsub("```json|```", "", text, perl = TRUE)

  jsonlite::fromJSON(trimws(text))
}