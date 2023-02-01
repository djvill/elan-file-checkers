# Elan File Checkers

[Dan Villarreal](https://www.linguistics.pitt.edu/people/dan-villarreal), University of Pittsburgh Linguistics

This is the source for the [APLS Elan file checker](https://djvill.shinyapps.io/apls_elan_file_checker/), a Shiny app that helps transcribers conform their Elan files to [LaBB-CAT](https://labbcat.canterbury.ac.nz/system/) specifications. It was developed by Dan Villarreal, originally for the [New Zealand Institute of Language, Brain, and Behaviour](https://www.canterbury.ac.nz/nzilbb/) and more recently for the [Archive of Pittsburgh Language and Speech (APLS)](https://labb-cat.linguistics.pitt.edu/labbcat/). The code for the original apps has been archived to the [`southland` branch](https://github.com/djvill/elan-file-checkers/tree/southland).


## Why an Elan file checker?

In short, this app is a **quality control tool that closes the gap between Elan and LaBB-CAT**, catching issues that originate in Elan before they cause downstream havoc in LaBB-CAT.

Elan is a free, flexible software tool for annotating linguistic data. LaBB-CAT is a tool for storing, searching, and manipulating linguistic corpora that accepts multiple file types, including Elan files. The flexibility of both Elan and LaBB-CAT can lead to a suboptimal situation for quality control and reproducibility. For example:

- Elan allows users to name tiers and speakers however they wish. In turn, LaBB-CAT allows the user to merge or rename speaker records, which may be necessary to ensure conformity to corpus conventions; the resulting disagreement between LaBB-CAT's version of a file and the original file can create major issues if the file is re-uploaded somewhere down the line (e.g., to add topic-tagged tiers).

- <a id="elan-spell-check"> Elan includes a spell-checker, but this doesn't include the myriad words that will inevitably need to be added to any corpus's dictionary (e.g., *Coraopolis*), and updating a custom dictionary across research assistants' computers is onerous. Without spell-checking capabilities, the responsibility for correcting bona fide misspellings (e.g., *Coropulis*) is downstreamed to the corpus manager. The presence of out-of-dictionary words in a transcript interferes with automatic phonetic alignment in LaBB-CAT, as any annotation that can't be mapped to a phonemic representation prevents the alignment of the turn in which this annotation is found.

- <a id="elan-hard-to-align"> Elan's user interface makes it rather difficult to align turn boundaries on multiple tiers (unlike Praat, which provides an easy way to duplicate alignments on multiple tiers; or Transcriber, in which time intervals are explicitly mapped to one or more tiers); this can result in short overlaps that are imperceptible to the Elan GUI user. These easily created overlaps make a mess in LaBB-CAT, where they become short orphan turns.

- <a id="elan-overlaps"> Elan also allows for turns to overlap in a way that messes with LaBB-CAT's automatic phonetic alignment performed via HTK or MFA. For example, if the main speaker's utterances from 0:40 to 0:55 are transcribed in a single turn and an interviewer's backchannel is transcribed from 0:46 to 0:47, LaBB-CAT refuses to align the entire main-speaker turn from 0:40 to 0:55.


## What do they do?

Once users drag-and-drop one or more files, the checker ensures that all files are Elan file format (`.eaf`). If so, the files go through up to three sequential steps; Step 2 only engages if all files pass Step 1, etc.

1. [Validating tier names and attributes...](#step-1-tier-check)
2. [Checking for out-of-dictionary words...](#step-2-dictionary-check)
3. [Checking for overlaps...](#step-3-overlap-check)


### Step 0: File name check

This step ensures that all files (a) begin with a valid APLS speaker code; and (b) end with `.eaf`.


### Step 1: Tier check

This step (a) ensures that all tiers have an Annotator attribute; and (b) checks that the names and attributes for speaker tiers (those not named "Noise", "Comment", or "Redaction") conform to naming conventions. Messages to the user are generated for any files that don't conform to these requirements.


### Step 2: Dictionary check

This step looks through all annotations on speaker tiers and generates messages to the user containing any annotations that aren't in a dictionary supplied by the app builder. This step thus performs both a spell-check and a convention-conformity-check in one fell swoop, with convention conformity narrowly tailored to how LaBB-CAT parses annotations. For example, even though the string `thr~[Tr]` isn't a valid entry in a standard or custom dictionary (or isn't likely to be), LaBB-CAT knows to interpret it as a hesitation with the phonetic material \[θɹ\] (rendered as `Tr` in the [DISC phonemic alphabet](https://djvill.github.io/APLS/doc/Phonemic-Transcription.html) used by LaBB-CAT), so this step interprets `thr~[Tr]` (and other entries ending in brackets) as valid.

The app uses two dictionaries, the [Unisyn](https://www.cstr.ed.ac.uk/projects/unisyn/) lexicon for American English and a [custom APLS dictionary](https://github.com/djvill/APLS/tree/main/files/custom-dictionary). The latter is continually updated and synced with the APLS LaBB-CAT's dictionary to avoid the [Elan spell-check issue](#elan-spell-check). For copyright reasons, the Unisyn dictionary is not available in this repo. 


### Step 3: Overlap check

This step tries to resolve overlaps that result from [shortfalls in Elan's UI](#elan-hard-to-align) in two ways: it 'snaps' near-enough boundaries to the same time-point and generates messages to the user about any overlaps that it couldn't automatically resolve. The threshold for 'near-enough' is configurable, defaulting to 500 milliseconds. This default threshold usually gobbles up the majority of overlaps, including virtually all of the unintended ones. The remaining overlaps that the user must resolve are usually of the type described [above](#elan-overlaps), which would otherwise prevent the automatic alignment (and thus the phonetic analysis) of larger chunks of speech.

This step includes all speaker tiers, plus the "Redaction" tier. It prioritizes tiers in this order: redaction > main speaker(s) > interviewer > bystander(s) (ties broken by alphabetical order). That is, if there are conflicting boundaries between a main speaker and an interviewer, it snaps the interviewer's boundary to the main speaker's.


### All files successful

If the checker completes Step 3, users are prompted to download the modified `.eaf` file(s) and upload them to the appropriate location. For simplicity's sake, the download button appears even if no files were actually modified. The download is a `.zip` file if multiple files were uploaded (regardless of how many were actually modified), and a single `.eaf` file otherwise. 


## How can I help?

Pop an issue on the [Issues page](https://github.com/djvill/elan-file-checkers/issues) or send me an [email](mailto:d.vill@pitt.edu?subject=GitHub%3A%20Elan%20File%20Checker).

