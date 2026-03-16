# Innovation Hub Dashboard – Context
## Project Overview

This project is a production-grade R Shiny dashboard built using the {golem} framework.
The application is part of the World Bank Poverty and Inequality Platform (PIP) – Innovation Hub, and is intended to showcase experimental poverty statistics and methodological comparisons in an interactive, visually polished way.

The app is structured as an R package, not a standalone Shiny script.

Primary goals:

- High-quality, publication-ready visualisations

- Modular, maintainable Shiny architecture

- Strong separation between UI, server logic, and plotting functions

- Efficient data handling for medium–large cross-country datasets

- Clean visual design aligned with World Bank / PIP branding

## Technology Stack & Conventions
### Framework & Architecture

{golem} for app scaffolding and package structure

App is launched with:

```
devtools::load_all()
golem::run_dev()
```

All UI/server logic lives in R/

Static assets live in inst/ (when applicable)

### Shiny Structure

The dashboard is implemented as a Shiny module:

mod_interactive_dashboard_ui()

mod_interactive_dashboard_server()

The module is instantiated from app_ui.R and app_server.R

No use of global variables inside modules

### Data Handling

Core datasets:

d_dm – welfare conversion methodology data

d_stb – standard / baseline methodology data

Data is passed into the module from app_server() (never loaded inside the module)

Data manipulation style:

collapse / fastverse preferred (fsubset, fmutate, fselect, rowbind)

data.table semantics where helpful

Tidyverse used only when necessary (e.g. pivot_longer, pivot_wider)

Avoid mixing paradigms unnecessarily

## Metadata & Documentation

### Metadata files:

dm_metadata.xlsx

stb_metadata.xlsx

### Long-form methodological descriptions are stored as Markdown files:

dm_full_description.md

stb_full_description.md

sn_full_description.md

Markdown is rendered using:

shiny::includeMarkdown()

## Dashboard Design & UX
Layout Structure

Upper section (light background):

Method selector (selectInput)

Economy selector (selectInput)

Dynamic text panel describing the method and country

“Learn more” button

Top chart: single-country poverty headcount comparison

Lower section (dark blue background):

Toggle buttons:

Rankings

Changes

Scatterplot

### Bottom chart:

Multi-country comparisons

Side annotation:

“Click on any economy to deep dive above”

Visual Principles

Clear visual hierarchy

Minimal clutter

Dark blue background used to distinguish “global comparison” section

Buttons adapt styling depending on background (btn-outline-light on dark blue)

Charts are ggplot-based and publication-quality

## Core Plotting Logic

### Top Chart

Function:

plot_single_country(data, select_country, select_method)


Compares Default vs New methodology

Shows arrows indicating direction/magnitude of change

Uses fixed poverty lines ($2.15, $3.65, $6.85)

Fully deterministic given inputs

### Bottom Charts

Placeholder functions (to be implemented / extended):

plot_rankings()
plot_changes()
plot_scatter()


These operate on the currently selected dataset

Controlled by reactive current_tab

## Reactive Logic Rules

Method selection determines dataset

"Welfare conversion" → d_dm

Anything else → d_stb

Economy dropdown updates whenever the dataset changes

Inputs are always wrapped in reactives:

selected_method()
selected_economy()


Never call input$...() as a function

All modal logic is handled via observeEvent()

## Coding Standards & Expectations for AI Assistance

When helping with this project, the assistant should:

- Respect the golem structure

- Do not suggest shiny::runApp()

- Do not introduce global variables

- Prefer modular solutions

- New features → new functions or modules

- Use collapse / data.table idioms

- Avoid unnecessary dplyr pipelines

- Be explicit

- Namespace Shiny calls when in doubt (shiny::showModal)

- Assume production intent

- Avoid hacks

- Prefer readable, robust code that is very well documented using roxygen2 and clear, succinct, comments

- Align with World Bank / PIP visual identity

- Think incrementally

- Layout first

- Then reactivity

- Then polish

## Current Status

- App runs cleanly via golem::run_dev()

- Top chart works and updates correctly

- Method-specific Markdown renders in modal

- Lower section layout matches design mockups

- Bottom chart functions still to be implemented / wired fully

## How the AI Should Help Going Forward

Useful tasks include:

- Refactoring module code for clarity

- Adding new charts or interactions

- Improving reactivity or performance

- Enhancing visual design (CSS, layout tweaks)

- Debugging Shiny/reactive issues

- Preparing the app for deployment (Posit Connect)

- Embedding Flourish charts, whether reactive, static, or scrollytelling

The assistant should assume the user is highly technical, comfortable with:

- Shiny internals

- R package development

- Data.table / collapse

- ggplot2

- Econometric/statistical context
