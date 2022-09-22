# Todo

## In logical order

- Order tiers by importance (main speaker(s) > interviewer > bystander(s)), and align overlaps from least important to most important
  - Add new test files: Ensure interviewer is snapped to main speaker(s)
  - So I can eventually add Redaction as the highest priority (since we need to delete exactly the right audio)
- Add Redaction to overlap checking/fixing
  - Add new test files: Ensure other speakers are snapped to Redaction; Ensure 2+ speakers overlapping with Redaction is handled correctly
  - Don't have a separate overlap handling process for Redaction---just have it be treated as a regular overlap
- Add step 4 for Redaction checking
  - All speaker-tier annotations that are (str-trimmed) only "REDACT" must coincide with an annotation on the Redaction tier, and vice versa. Create test files for failure modes:
    - "REDACT" but no Redaction annotation
    - "REDACT" with other (non-whitespace) chars
    - ~"REDACT" with Redaction annotation but the times don't align~ Handled in step 3
    - Empty Redaction annotation
    - Redaction annotation with speaker annotation but the speaker annotation isn't "REDACT" (could be additional chars, or something completely different)


## Basic functionality

- Include Redaction in overlap checker
- Order tiers by importance (main speaker(s) > interviewer > bystander(s)), and align overlaps from least important to most important
- Handle multiple main speakers in tier checker
- Redaction annotations formatted correctly (potentially as step 4): standalone annotation on speaker tier with just "REDACT", matching annotation on Redaction tier


## Testing

- Step 3: If any overlaps are fixed, JSON file should include files w/ fixed overlaps (regardless of whether they're actually output by the app---that is, whether there are remaining overlaps to fix)


## Refactoring

- Reduce the number of file structure objects created (some are unnecessary intermediate steps)
- In shinytest utils: Create a separate function to set up savename, because there's duplicated code between snap() and snapDownload()
- In snapDownload(): Move success checking into the function itself (it's safer that way)


## Extensions

- If transcript passes all checks, have it upload (automatically? optionally?) to server or email to Dan
- Option to fill in dictionary entries (if transcript fails step 2, download txt file with words and have transcribers delete lines or fill in DISC representations, which transcribers then re-upload)

