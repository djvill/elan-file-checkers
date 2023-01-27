# Todo

## Bugfixes

- Overlaps: If an overlapping annotation has a boundary that lines up exactly with another tier, `fixOverlapsTier()` throws an error with `overlapBoundsFixed %>% ... %>% filter(NewTS1==NewTS2)`
  - One of the NewTSs is missing because the long version of `overlapBoundsFixed` only has one boundary for that annotation. That comes from `findOverlapsTier()`'s `mutate(ANNOTATION_ID_overlapped = timesOtherTiers$ANNOTATION_ID %>% extract(which(Time > timesOtherTiers$Start & Time < timesOtherTiers$End)[1]))`
  - Patched by turning off zero-width checking by default---this will need a more durable solution down the line


## Basic functionality

- Step 1: Fail if there's a "recheck" or "text" tier
  - Add a parameter for prohibited tier names

- Convert "smart quotes" to straight quotes
  - Add a text preprocessing subtask, probably at the start of Step 2

- In `eaflist_to_df()`, `tierNames <- seq_len(nrow(df))` doesn't work as expected if `tierInfo()$TIER_ID` exists but has NAs (multiple files, or just some tier IDs missing)

- Add step 4 for Redaction checking
  - All speaker-tier annotations that are (str-trimmed) only "REDACT" must coincide with an annotation on the Redaction tier, and vice versa. Create test files for failure modes:
    - "REDACT" but no Redaction annotation
    - "REDACT" with other (non-whitespace) chars
    - Empty Redaction annotation
    - Redaction annotation with speaker annotation but the speaker annotation isn't "REDACT" (could be additional chars, or something completely different)

## Testing

- Step 3: If any overlaps are fixed, JSON file should include files w/ fixed overlaps (regardless of whether they're actually output by the app---that is, whether there are remaining overlaps to fix)


## Refactoring

- In shinytest utils: Create a separate function to set up savename, because there's duplicated code between snap() and snapDownload()
- In snapDownload(): Move success checking into the function itself (it's safer that way)


## Extensions

- Add number-of-fixed-overlaps counter (either just a total or by-file)
- ~If transcript passes all checks, have it upload (automatically? optionally?) to server or email to Dan~
  - Current system works fine
- ~Option to fill in dictionary entries (if transcript fails step 2, download txt file with words and have transcribers delete lines or fill in DISC representations, which transcribers then re-upload)~
  - Current system works fine

### Post-APLS Phase 1

- Handle multiple main speakers in tier checker
