classify_issue <- function(title, body, labels,
                           repo_description = "", repo_topics = "",
                           user_skills = "") {

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

  # lets the model calibrate against what the user actually knows
  skill_context <- if (nchar(user_skills) > 0) {
    paste0("The contributor is comfortable with: ", user_skills, "\n")
  } else {
    ""
  }

  body_section <- if (nchar(body) > 0) {
    paste0("Body: ", substr(body, 1, 400), "\n")
  } else {
    ""
  }

  prompt <- paste0(
    "Evaluate this GitHub R package issue for a first-time open source contributor.\n\n",
    repo_context,
    skill_context,
    "Score 1-5 using this rubric:\n",
    "5 = Ideal: typo, simple doc fix, clearly scoped with explicit maintainer guidance, or marked good-first-issue\n",
    "4 = Good: well-defined task, requires only basic R knowledge, no package internals\n",
    "3 = Moderate: needs some familiarity with the package but not its internals\n",
    "2 = Hard: requires domain expertise, package internals, or significant context\n",
    "1 = Not suitable: complex bug, research-level, or requires deep expertise\n\n",
    "Title: ", title, "\n",
    "Labels: ", if (nchar(labels) > 0) labels else "none", "\n",
    body_section,
    "\nRespond with JSON only. ",
    "Reason must cite something specific from the title or body above. ",
    "Do not start the reason with 'The issue'.\n",
    '{"score": <1-5>, "reason": "<one sentence>"}'
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
    httr2::req_retry(
      max_tries    = 3,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503),
      backoff      = function(i) 30 * i
    ) |>
    httr2::req_perform()

  raw    <- httr2::resp_body_json(resp)
  text   <- raw$choices[[1]]$message$content
  text   <- gsub("```json|```", "", text, perl = TRUE)
  result <- jsonlite::fromJSON(trimws(text))

  score <- suppressWarnings(as.integer(round(result$score)))
  if (is.na(score) || score < 1 || score > 5) score <- NA_integer_
  result$score <- score

  result
}

classify_issues <- function(df, max_classify = 10, skills = NULL, progress_cb = NULL) {
  if (nrow(df) == 0) return(df)

  df$llm_score <- NA_integer_
  df$reason    <- ""

  user_skills <- if (!is.null(skills)) paste(skills, collapse = ", ") else ""

  by_score    <- order(df$score, decreasing = TRUE)
  candidates  <- by_score[df$score[by_score] >= 2]
  to_classify <- head(candidates, max_classify)

  for (idx in seq_along(to_classify)) {
    i   <- to_classify[idx]
    row <- df[i, ]



    result <- tryCatch({
      classify_issue(
        row$title,
        row$body,
        row$labels,
        row$repo_description,
        row$repo_topics,
        user_skills
      )
    }, error = function(e) {
      message("classify failed: ", row$repo, " #", row$number, " — ", conditionMessage(e))
      NULL
    })

    if (!is.null(result)) {
      df$llm_score[i] <- result$score
      df$reason[i]    <- result$reason
    }

    if (!is.null(progress_cb)) progress_cb(idx, length(to_classify))

    Sys.sleep(0.5)
  }

  df$display_score <- ifelse(!is.na(df$llm_score), df$llm_score, df$score)
  df[order(df$display_score, decreasing = TRUE), ]
}