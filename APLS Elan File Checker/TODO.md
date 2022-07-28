# Todo

## Basic functionality

- Include Redactions in overlap checker
- Order tiers by importance (main speaker(s) > interviewer > bystander(s)), and align overlaps from least important to most important
- Handle multiple main speakers in tier checker
- Redactions annotations formatted correctly (potentially as step 4): standalone annotation on speaker tier with just "REDACT", matching annotation on Redactions tier


## Testing

- Explicitly define what the outcome should be for each test file
  - Perhaps use `shinytest` ([article](https://shiny.rstudio.com/articles/testing-overview.html), [vignette](https://rstudio.github.io/shinytest/articles/shinytest.html))


## Extensions

- If transcript passes all checks, have it upload (automatically? optionally?) to server or email to Dan
- Option to fill in dictionary entries (if transcript fails step 2, download txt file with words and have transcribers delete lines or fill in DISC representations, which transcribers then re-upload)

