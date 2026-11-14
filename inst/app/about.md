<!--
The about section in the sidebar, written in Markdown.

Text before the first "## " heading is the intro, always shown at the top.

Each "## " heading is a question and the text under it is its answer. A
question with no answer is left out, so write an answer to turn one on.

To show a question on some tabs only, end its heading with {tabs: house, senate}.
The tabs are house, senate and sponsorship. Without it, it shows on every tab.

Text inside these comment marks is never shown.
-->

South Dakota is one of many U.S. states where one political party has near-unanimous control of the state legislature. But what disagreements exist within the Republican party? This app uses votes in the SD House and Senate to identify sub-partisan cliques amongst voting members. 

Select a session and explore the graph network by using the "Find a legislator" or clicking on a member's photo to explore a specific voter's profile.

## What does a line mean? {tabs: house, senate}
A line between legislators means they vote the same way on a subset of controversial bills in that specific session, more often than random chance would suggest. 

Individual relationships do not mean members agree ideologically, since the data only show the issues raised during that sesesion. If two members only vote on bills related to subjects they agree on, they may appear to be more aligned than reality. 

## What does a line mean? {tabs: sponsorships}
A line between legislators means they sponsor bills together more often than random chance would suggest.

## How do I find which members are more extreme?
This app only describes how members vote relative to other members inside their chamber; it does not describe ideology or polarization in any colloquial sense. 

## Sources

Roll call data is from Plural Policy's [Open States](https://open.pluralpolicy.com) project. Supplementary information is from the [South Dakota Legislative Research Council](https://sdlegislature.gov). Graph backbone extraction is implemented using Zachary Neal's R package [backbones](https://doi.org/10.1371/journal.pone.0349258).

## Questions or corrections?
E-mail Slater Dixon at [forward@slaterjdixon.com](mailto:forward@slaterjdixon.com) if you see problems with the data or have a question about interpreting something you found. 

Created by [Slater Dixon](https://slaterjdixon.com)