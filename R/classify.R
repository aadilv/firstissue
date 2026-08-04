classify_issue <- function(title, body, labels) {
  prompt <- paste0(
    "Evaluate this GitHub R package issue for a first-time open source contributor.\n\n",
    "Score 1-5 using this rubric:\n",
    "5 = Ideal: typo, simple doc fix, or clearly scoped with explicit maintainer guidance\n",
    "4 = Good: well-defined task, only basic R knowledge required\n",
    "3 = Moderate: needs some package familiarity but not internals\n",
    "2 = Hard: requires domain expertise or knowledge of package internals\n",
    "1 = Not suitable: complex bug, research-level, or deep expertise required\n\n",
    "Title: ", title, "\n",
    "Labels: ", if (nchar(labels) > 0) labels else "none", "\n",
    "Body: ", substr(body, 1, 400), "\n\n",
    "Respond with JSON only. Reason must cite something specific from the issue above.\n",
    '{"score": <1-5>, "reason": "<one sentence referencing the issue>"}'
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

# only classify issues that cleared heuristic bar (to save quota)
classify_issues <- function(df, max_classify = 10) {
  if (nrow(df) == 0) return(df)

  df$llm_score <- NA_integer_
  df$reason    <- ""

  # cap at top N by heuristic score
  by_score    <- order(df$score, decreasing = TRUE)
  candidates  <- by_score[df$score[by_score] >= 2]
  to_classify <- head(candidates, max_classify)

  for (i in to_classify) {
    row <- df[i, ]

    result <- tryCatch({
      classify_issue(row$title, row$body, row$labels)
    }, error = function(e) {
      message("classify failed: ", row$repo, " #", row$number, " — ", conditionMessage(e))
      NULL
    })

    if (!is.null(result)) {
      df$llm_score[i] <- result$score
      df$reason[i]    <- result$reason
    }

    Sys.sleep(0.5)
  }

  df$display_score <- ifelse(!is.na(df$llm_score), df$llm_score, df$score)
  df[order(df$display_score, decreasing = TRUE), ]
}