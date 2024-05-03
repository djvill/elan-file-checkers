# Todo

## Format-independent

### Medium-range OOP

- Make trs_nesttiers an _optional_ output for as.trs_transcription()


## Basic functionality

- Convert "smart quotes" to straight quotes
  - Add a text preprocessing subtask, probably at the start of Step 2

- Add step 4 for Redaction checking
  - All speaker-tier annotations that are (str-trimmed) only "REDACT" must coincide with an annotation on the Redaction tier, and vice versa. Create test files for failure modes:
    - "REDACT" but no Redaction annotation
    - "REDACT" with other (non-whitespace) chars
    - Empty Redaction annotation
    - Redaction annotation with speaker annotation but the speaker annotation isn't "REDACT" (could be additional chars, or something completely different)


## Refactoring

- In shinytest utils: Create a separate function to set up savename, because there's duplicated code between snap() and snapDownload()
- In snapDownload(): Move success checking into the function itself (it's safer that way)
- Overlaps: If an overlapping annotation has a boundary that lines up exactly with another tier, `fixOverlapsTier()` throws an error with `overlapBoundsFixed %>% ... %>% filter(NewTS1==NewTS2)`
  - One of the NewTSs is missing because the long version of `overlapBoundsFixed` only has one boundary for that annotation. That comes from `findOverlapsTier()`'s `mutate(ANNOTATION_ID_overlapped = timesOtherTiers$ANNOTATION_ID %>% extract(which(Time > timesOtherTiers$Start & Time < timesOtherTiers$End)[1]))`
  - Patched by turning off zero-width checking by default---this will need a more durable solution down the line

## Extensions


### Post-APLS Phase 1

- Handle multiple main speakers in tier checker
