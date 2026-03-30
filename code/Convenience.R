# recoding for MODELING (factors)
recode_map <- list(
  # example (edit later as needed)
  intensity_band = c(
    "tier_1" = "low",
    "tier_2" = "moderate",
    "tier_3" = "high"
  ),
  contact_duration_band = c(
    "brief" = "short",
    "moderate" = "medium",
    "extended" = "long"
  )
)

# lookup for DISPLAY (labels)
lookup_df <- tibble(
  predictor = c(
    "intensity_band","intensity_band","intensity_band",
    "contact_duration_band","contact_duration_band","contact_duration_band"
  ),
  code = c(
    "low","moderate","high",
    "short","medium","long"
  ),
  label = c(
    "Low intensity","Moderate intensity","High intensity",
    "Short sessions","Medium sessions","Long sessions"
  )
)