---
name: workflow-trace
description: Generate a Bidirectional Interaction Trace (BIT) of a workflow. Use when the user asks for a workflow trace, interaction trace, or compact input/output description of a process.
---

Present the workflow for $ARGUMENTS as a Bidirectional Interaction Trace (BIT) consisting of exactly two paragraphs separated by a blank line. The first paragraph must contain only user text input (commands, keystrokes, interactions) written as a continuous stream, optionally using shorthand notation such as arrows for sequence and grouping for related actions. The second paragraph must contain only program text output, written either as imperative sentences or as a dense state-transition shorthand, describing only visible state changes. Do not include explanations, headers, labels, commentary, metaphors, or teaching language. Remove articles where possible. Combine consecutive actions or outputs sharing the same verb or transition into a single expression, using temporal words like "then" where needed to preserve ordering. Treat the result as a lossless interaction trace governed by an implicit grammar in which inputs deterministically produce ordered state transitions, repetition is compressed structurally rather than verbally, and the artifact is readable as documentation, comparison surface, or executable mental model. Optimize strictly for brevity, signal density, consistency, and composability rather than pedagogy. Conform stylistically and structurally to the following example.

SPC w m → SPC s d → SPC b b | SPC s e → SPC s w → /JSON → visy → SPC b b p → SPC s p serialize → RET → SPC c t → ]e → SPC x → i…ESC → SPC h d m json → SPC b i

foo.py@serialize → *Dictionary → foo.py → *Etymology → *Wikipedia@JSON → foo.py+sentence → *rg → foo.py@match → *compilation@error → foo.py@error → *scratch+note → *Man → *BufferList
