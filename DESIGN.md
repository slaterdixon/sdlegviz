---
name: SD Legviz
description: Public ledger of who votes with whom in the South Dakota legislature.
colors:
  ink: "#1c1917"
  muted-stone: "#78716c"
  hairline: "#e7e5e4"
  paper: "#ffffff"
  warm-page: "#fafaf9"
  republican-red: "#cd2626"
  democratic-blue: "#00aef3"
  other-gray: "#808080"
typography:
  title:
    fontFamily: "system-ui, -apple-system, Segoe UI, Roboto, Helvetica Neue, sans-serif"
    fontSize: "1.15rem"
    fontWeight: 600
    lineHeight: 1.25
  brand:
    fontFamily: "system-ui, -apple-system, Segoe UI, Roboto, Helvetica Neue, sans-serif"
    fontWeight: 600
    letterSpacing: "-0.01em"
  body:
    fontFamily: "system-ui, -apple-system, Segoe UI, Roboto, Helvetica Neue, sans-serif"
    fontSize: "0.875rem"
    fontWeight: 400
  small:
    fontFamily: "system-ui, -apple-system, Segoe UI, Roboto, Helvetica Neue, sans-serif"
    fontSize: "0.8rem"
    fontWeight: 400
    lineHeight: 1.45
  label:
    fontFamily: "system-ui, -apple-system, Segoe UI, Roboto, Helvetica Neue, sans-serif"
    fontSize: "0.75rem"
    fontWeight: 400
    letterSpacing: "0.04em"
  section-label:
    fontFamily: "system-ui, -apple-system, Segoe UI, Roboto, Helvetica Neue, sans-serif"
    fontSize: "0.7rem"
    fontWeight: 600
    letterSpacing: "0.06em"
rounded:
  control: "6px"
  panel: "8px"
  pill: "99px"
spacing:
  xs: "0.25rem"
  sm: "0.5rem"
  md: "1rem"
  lg: "1.5rem"
components:
  graph-panel:
    backgroundColor: "{colors.warm-page}"
    rounded: "{rounded.panel}"
  nav-tab:
    textColor: "{colors.muted-stone}"
    typography: "{typography.body}"
  nav-tab-active:
    textColor: "{colors.ink}"
  share-track:
    backgroundColor: "{colors.hairline}"
    rounded: "{rounded.pill}"
    height: "0.5rem"
  share-fill:
    backgroundColor: "{colors.republican-red}"
    rounded: "{rounded.pill}"
  share-average-tick:
    backgroundColor: "{colors.ink}"
    width: "2px"
---

# Design System: SD Legviz

## Overview

**Creative North Star: "The Public Ledger"**

- A record to check, not a show to watch. Every mark traces to a definition.
- Neutral warm-gray chrome. Photos and party colors are the only strong color.
- Flat. Hairlines separate; nothing floats.
- No site "voice" or generated copy. Text on the page is the author's (`inst/app/about.md`).

**Key Characteristics:**
- Scanability and consistency over expression.
- Three columns on desktop: controls, graph, detail.
- Dense data stays readable by being quiet.

## Colors

Warm neutrals plus two data colors.

### Primary
- **Ledger Ink** (#1c1917): text, active tab, average tick, bootstrap primary.

### Neutral
- **Warm Page** (#fafaf9): page background and graph panel.
- **Paper** (#ffffff): navbar.
- **Hairline Stone** (#e7e5e4): borders, dividers, bar track.
- **Muted Stone** (#78716c): secondary text, labels, captions.

### Data colors (not chrome)
- **Republican Red** (#cd2626): Republican nodes, edges, bar fill, party dot.
- **Democratic Blue** (#00aef3): Democratic nodes, edges, bar fill, party dot.
- **Other Gray** (#808080): any other party.

### Named Rules
**The Data-Only Color Rule.** Red and blue appear only where they encode party: nodes, edges, bars, the party dot. Never in buttons, banners, headings, or backgrounds.

**The Quiet Chrome Rule.** The interface around the graph stays neutral so the graph is the loudest thing on screen.

## Typography

**Font:** system UI stack (system-ui, -apple-system, Segoe UI, Roboto, Helvetica Neue, sans-serif). No web font loaded.

### Hierarchy
- **Title** (600, 1.15rem, 1.25): legislator name.
- **Brand** (600, -0.01em): navbar title.
- **Body** (400, 0.875rem): card facts, nav.
- **Small** (400, 0.8rem, 1.45): About text, vote lists.
- **Label** (400, 0.75rem, uppercase, 0.04em): control labels.
- **Section label** (600, 0.7rem, uppercase, 0.06em): card and About headings.

### Named Rules
**The Label-Only Caps Rule.** Uppercase only for labels and section headings, never for sentences.

## Layout

- Desktop: grid of 16rem controls, flexible graph, 21rem detail; 1.5rem gap; height fills the viewport below the navbar.
- Controls and detail columns scroll on their own; the graph does not.
- Below 992px: single column, graph 70vh, navbar collapses.
- Spacing scale: 0.25 / 0.5 / 1 / 1.5rem.

## Elevation & Depth

Flat. No shadows. Depth is a 1px hairline (#e7e5e4) and the step from page to white navbar.

### Named Rules
**The Hairline Rule.** Separate with a 1px Hairline Stone border. No drop shadows, no glows.

## Shapes

- Controls 6px, panels 8px, bars fully pill (99px), legislator photos circular.
- Graph nodes are circular photos with a party-colored ring (4px, 6px selected). Leadership nodes are larger (40 vs 25).
- The logo is the South Dakota outline with a small network inside; heavy black outline, transparent background.

## Components

### Navigation
- Text tabs under the brand. Inactive Muted Stone; active Ledger Ink with a 2px underline.
- No pills, no filled tabs.

### Graph panel
- Warm Page background, 1px hairline, 8px corners, overflow hidden.

### Legislator card
- Round photo beside name; party shown as a colored dot plus party name in Muted Stone.
- Facts as a two-column list: label Muted Stone, value Ink.
- Section labels above each block, separated by a hairline.

### Share bar
- 0.5rem pill track in Hairline Stone, fill in the party color, 2px Ledger Ink tick at the party average.
- Label left, value right (value Muted Stone).

### About row
- Native expandable row: bold question, hairline between rows, answer in Muted Stone.
- Intro paragraph stays open above the rows in Ink.

## Do's and Don'ts

### Do:
- **Do** keep chrome neutral and let the graph carry the color.
- **Do** keep definitions one click away from every number.
- **Do** show members with no links as unlinked nodes.

### Don't:
- **Don't** give it "shiny dash" vibes: no gradient cards, big KPI tiles, glossy panels, or default Shiny/dashboard styling.
- **Don't** use anything polarizing: no loud partisan red/blue in the interface, no campaign or attack-ad styling, no rankings or labels that read as accusation.
- **Don't** add shadows, glows, or decorative motion.
- **Don't** write or generate site copy or a "voice"; copy is the author's.
