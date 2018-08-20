# Elan File Checkers

This is the source for two Shiny apps ([here](https://djvill.shinyapps.io/SouthlandElanFileChecker/ "Southland-CB Elan File Checker") and [here](https://djvill.shinyapps.io/SouthlandJHElanFileChecker/ "Southland-JH Elan File Checker")) that help transcribers conform their Elan files to [LaBB-CAT](https://labbcat.canterbury.ac.nz/system/) specifications. They were developed by [Dan Villarreal](https://canterbury-nz.academia.edu/DanVillarreal) for the [New Zealand Institute of Language, Brain, and Behaviour](https://www.canterbury.ac.nz/nzilbb/).

## Why Elan File Checkers?

In short, these apps are **quality control tools that close the gap between Elan and LaBB-CAT**, catching issues that originate in Elan before they cause downstream havoc in LaBB-CAT.

Elan is a free, flexible software tool for annotating linguistic data. LaBB-CAT is a tool for storing, searching, and manipulating linguistic corpora that accepts multiple file types, including Elan files. The flexibility of both Elan and LaBB-CAT can lead to suboptimal results, especially as concerns quality control and replicability. For example:

* Elan allows users to name tiers and speakers however they wish. In turn, LaBB-CAT allows the user to merge or rename speaker records, which may be necessary to ensure conformity to corpus conventions; the resulting disagreement between LaBB-CAT's version of a file and the original file can create major issues if the file is re-uploaded somewhere down the line (e.g., to add topic-tagged tiers).

* Elan includes a spell-checker, but this doesn't include the myriad words that will inevitably need to be added to any corpus's dictionary (e.g., *Otago*), and updating a custom dictionary across research assistants' computers is onerous. Without spell-checking capabilities, the responsibility for correcting bona fide misspellings (e.g., *Otaog*) is downstreamed to the corpus manager. The presence of out-of-dictionary words in a transcript interferes with automatic phonetic alignment in LaBB-CAT, as any annotation that can't be mapped to a phonemic representation prevents the alignment of the turn in which this annotation is found.

* Elan's user interface makes it rather difficult to align turn boundaries on multiple tiers (unlike Praat, which provides an easy way to duplicate alignments on multiple tiers; or Transcriber, in which time intervals are explicitly mapped to one or more tiers); this can result in short overlaps that are imperceptible to the Elan GUI user. These easily created overlaps make a mess in LaBB-CAT, where they become short orphan turns.

* Elan also allows for turns to overlap in a way that messes with LaBB-CAT's automatic phonetic alignment performed via htk. For example, if the main speaker's utterances from 0:40 to 0:55 are transcribed in a single turn and an interviewer's backchannel is transcribed from 0:46 to 0:47, htk refuses to align the entire main-speaker turn from 0:40 to 0:55.

## What do they do?

Once users drag-and-drop one or more files, the checker ensures that all files are Elan file format (`.eaf`). If so, the files go through up to three sequential steps; Step 2 only engages if all files pass Step 1, etc.

1. **Validating tier names and attributes...**: This step (a) ensures that all tiers have an Annotator attribute; and (b) checks that the names and Participant attributes for speaker tiers (those not named "Noise", "Comment", or "Reading Task") conform to the conventions for that corpus. For example, in the Southland-CB corpus, all audio and transcription files consist of the interviewer's initials, the speaker number, a hyphen, and the tape number (e.g., `KF3-04.eaf`); the transcription conventions dictate that there be a speaker tier called "KF3" and an interviewer tier called "Interviewer KF". Messages to the user are generated for any files that don't conform to these requirements (if any).

Side note: This first step is the most likely to differ between corpora; indeed, it's the only thing that's different for the Southland-CB and Southland-JH checkers in this repository. In the very near future, I'm hoping to split everything except Step 1 into a separate file so there's no need to make the same changes twice to other parts of the file checker.

2. **Checking for out-of-dictionary words...**: This step looks through all annotations on speaker tiers and generates messages to the user containing any annotations that aren't in a dictionary supplied by the app builder. (Note that these dictionaries are absent from this repository---see below.) This step thus performs both a spell-check and a convention-conformity-check in one fell swoop, with convention conformity narrowly tailored to how LaBB-CAT reads annotations. For example, even though the string `thr~[Tr]` isn't a valid entry in a standard or custom dictionary (or isn't likely to be), LaBB-CAT knows to interpret it as a hesitation with the phonetic material \[θɹ\] (rendered as `[Tr]` in the DISC phonetic alphabet used by LaBB-CAT), so this step interprets `thr~[Tr]` (and other entries ending in brackets) as valid.

3. **Checking for overlaps...**: This step tries to resolve overlaps that result from shortfalls in Elan's UI (as mentioned above) in two ways: it 'snaps' near-enough boundaries to the same time-point and generates messages to the user about any overlaps that it couldn't automatically resolve. By default, 'near-enough' means two boundaries are within 500 milliseconds of one another (though this can be easily changed), which usually gobbles up the majority of overlaps, including virtually all of the unintended ones. The remaining overlaps that the user must resolve are usually of the type described in the 4th bullet point above, which would otherwise prevent the automatic alignment (and thus the phonetic analysis) of larger chunks of speech.

If the checker reaches Step 3 and modifies any file(s) (i.e., if it resolves any overlaps), a download button appears so the user can update the file(s) on their local system or on the server; this returns a `zip` file if multiple files were uploaded (regardless of how many were actually modified) or an `eaf` file if just one file was uploaded. If all files pass through Step 3 with no modifications needed, a message informs the user that their file has passed muster and is ready to be uploaded to the server.

## What is (and isn't) in this repository?

Currently, this repository includes two highly similar Elan file checkers developed for different corpora within the Southland project; as mentioned above, the only real difference between these checkers is their requirements for tier names and attributes.

Something *not* included in this repository, but necessary for these checkers to run, is a dictionary file, which would normally be `CB Elan File Checker/dict/dict.txt` and `JH Elan File Checker/dict/dict.txt`. I've excluded these files for copyright reasons, as they're mostly populated by information from CELEX; NZILBB members can find the CELEX files on the NZILBB server. Users without CELEX access can use the custom dictionary of their choosing. In the near future, I'm hoping to add code that allows these checkers to bypass the dictionary-check step if no dictionary is found (with a warning about the missing functionality, of course!).

In addition to CELEX entries, the dictionary these apps use includes nearly 20 thousand additional entries that have been added to LaBB-CAT custom dictionaries over the years. One obstacle to the use of these apps is that this custom dictionary grows all the time---fortunately, it can be quickly and easily downloaded from LaBB-CAT by users with admin privileges as a `csv` file. As a result, this repository includes an R script ([UpdateElanCheckerDict.R]) that combines the baseline CELEX dictionary with this `csv` file, plus a set list of about 200 hesitation codes that are hard-coded into LaBB-CAT (e.g., `sh~`), and creates new versions of the `dict/dict.txt` files for you. If you're responsible for maintaining a LaBB-CAT instance that has a custom dictionary that grows, ideally you'll re-download this file and re-run the script every time you update the custom dictionary so the corresponding Elan File Checker app has the most up-to-date possible version of the dictionary.

## How can I help?

Pop an issue on the [Issues page](https://github.com/djvill/elan-file-checkers/issues) or send me an [email](mailto:djvill@ucdavis.edu?subject=GitHub%3A%20Elan%20File%20Checker).

