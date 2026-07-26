# Heuristic scoring before any LLM calls

score_issue <- function(title, body_length, labels, comments, assigned) {
  score <- 0

  label_vec <- tolower(strsplit(labels, ", ")[[1]])

  # +ve signals
  if (any(grepl("good first issue|good-first-issue", label_vec))) score <- score + 3
  if (any(grepl("help wanted|documentation|docs|beginner", label_vec))) score <- score + 1
  if (body_length < 500)  score <- score + 1
  if (comments < 5)       score <- score + 1
  if (!assigned)          score <- score + 1

  # -ve signals
  if (any(grepl("performance|regression|security|crash|breaking", label_vec))) score <- score - 1

  min(max(score, 0), 5)
}

score_issues <- function(df) {
  if (nrow(df) == 0) return(df)

  df$score <- mapply(
    score_issue,
    body_length = df$body_length,
    labels      = df$labels,
    comments    = df$comments,
    assigned    = df$assigned,
    title       = df$title
  )

  df[order(df$score, decreasing = TRUE), ]
}