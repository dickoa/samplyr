## Repair the site's LLM copies after pkgdown::build_site(), in CI
##
## pkgdown writes llms.txt and one .md per page. pandoc's HTML reader takes
## the formulas of a page as text and its Markdown writer escapes them, so
## they reach the copies mangled whatever the pandoc version, and pandoc
## 3.11 also flattens code blocks. llm-docs.R is a copy of the shared
## llm-docs.R of the package gates (~/Devel/R/gates). It picks a pandoc that
## keeps code fenced, and this script rebuilds the copies with it, restores
## the formulas as TeX, and fails if a defect remains.
##
## Usage, from the package root after pkgdown::build_site():
##   Rscript .gitlab/build-llm-docs.R

source(".gitlab/llm-docs.R")
llm_docs_pandoc()
pkg <- pkgdown::as_pkgdown(".")
pkgdown::build_llm_docs(pkg)
repair_llm_math(pkg)
check_llm_docs(pkg)
