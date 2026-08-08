classify_issue <- function(title, body, labels, repo_description = "", repo_topics = "") {

  # compact repo context
  repo_context <- if (nchar(repo_description) > 0 || nchar(repo_topics) > 0) {
    paste0(
      "Package: ",
      if (nchar(repo_description) > 0) repo_description else "",
      if (nchar(repo_topics) > 0) paste0(" [", repo_topics, "]") else "",
      "\n"
    )
  } else {
    ""
  }

  prompt <- paste0(
    "Evaluate this GitHub R package issue for a first-time open source contributor.\n\n",
    repo_context,
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
  text   <- gsub("```json|```", "", text, perl = TRUE)
  result <- jsonlite::fromJSON(trimws(text))

  score <- suppressWarnings(as.integer(round(result$score)))
  if (is.na(score) || score < 1 || score > 5) score <- NA_integer_
  result$score <- score

  result
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

    if (nchar(row$body) == 0) next

    result <- tryCatch({
      classify_issue(
        row$title,
        row$body,
        row$labels,
        row$repo_description,
        row$repo_topics
      )
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