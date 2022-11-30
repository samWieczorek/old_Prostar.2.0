DAPAR.loc <- DAPARdata.loc <- Prostar.loc <- NULL


library(fresh)
library(DaparToolshed)
library(QFeatures)

source(file.path(".", "mod_loading_page.R"), local = TRUE)$value
source(file.path(".", "mod_homepage.R"), local = TRUE)$value
source(file.path(".", "mod_insert_md.R"), local = TRUE)$value
source(file.path(".", "mod_choose_pipeline.R"), local = TRUE)$value
source(file.path(".", "mod_open_demoDataset.R"), local = TRUE)$value
source(file.path(".", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path(".", "mod_release_notes.R"), local = TRUE)$value
source(file.path(".", "mod_check_updates.R"), local = TRUE)$value
source(file.path(".", "mod_bug_report.R"), local = TRUE)$value
source(file.path(".", "mod_test.R"), local = TRUE)$value
source(file.path(".", "mod_format_DT.R"), local = TRUE)$value


## URLs for the .md files stored in the website github directory
base_URL <- "http://www.prostar-proteomics.org/md/"
URL_FAQ <- paste0(base_URL, "FAQ.md")
URL_links <- paste0(base_URL, "links.md")
URL_ProstarPresentation <- paste0(base_URL, "presentation.md")
URL_formerReleases <-paste0(base_URL, "formerReleases.md")
URL_versionNotes <- paste0(base_URL, "versionNotes.md")


actionBtnClass <- "btn-primary"

# Bootstrap colors for buttons

# ="btn btn-primary">Primary</button>
#   <button type="button" class="btn btn-secondary">Secondary</button>
#   <button type="button" class="btn btn-success">Success</button>
#   <button type="button" class="btn btn-danger">Danger</button>
#   <button type="button" class="btn btn-warning">Warning</button>
#   <button type="button" class="btn btn-info">Info</button>
#   <button type="button" class="btn btn-light">Light</button>
#   <button type="button" class="btn btn-dark">Dark</button>
#   
#   <button type="button" class="btn btn-link">Link</button>

redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px;"

listBrewerPalettes <- c("Dark2 (qualit.)" = "Dark2",
  "Accent (qualit.)"="Accent",
  "Paired (qualit.)" = "Paired",
  "Pastel1 (qualit.)" = "Pastel1",
  "Pastel2 (qualit.)" = "Pastel2",
  "Set1 (qualit.)" = "Set1",
  "Set2 (qualit.)" = "Set2", 
  "Set3 (qualit.)" = "Set3",
  "BrBG (diverging)"="BrBG",
  "PiYG (diverging)"=  "PiYG",
  "PRGn (diverging)" ="PRGn",
  "PuOr (diverging)" ="PuOr",
  "RdBu (diverging)"="RdBu",
  "RdGy (diverging)" ="RdGy",
  "RdYlBu (diverging)" ="RdYlBu",
  "RdYlGn (diverging)" ="RdYlGn",
  "Spectral (diverging)"="Spectral")