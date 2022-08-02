# Todo

## Basic functionality

- Include Redactions in overlap checker
- Order tiers by importance (main speaker(s) > interviewer > bystander(s)), and align overlaps from least important to most important
- Handle multiple main speakers in tier checker
- Redactions annotations formatted correctly (potentially as step 4): standalone annotation on speaker tier with just "REDACT", matching annotation on Redactions tier


## Testing

- Step 3: If any overlaps are fixed, JSON file should include files w/ fixed overlaps (regardless of whether they're actually output by the app---that is, whether there are remaining overlaps to fix)


## Extensions

- If transcript passes all checks, have it upload (automatically? optionally?) to server or email to Dan
- Option to fill in dictionary entries (if transcript fails step 2, download txt file with words and have transcribers delete lines or fill in DISC representations, which transcribers then re-upload)

