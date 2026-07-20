## R CMD check results

0 errors | 1 warning | 0 notes

All package code, examples, tests, and vignettes pass. The warning is from
CRAN incoming feasibility and currently reports:

* This is a new submission with development version `0.8.9999`.
* `Remotes` is not a standard CRAN `DESCRIPTION` field.
* Strong dependencies `sondage` and `svyplan` are not yet available from
  CRAN or Bioconductor.
* The configured GitLab issue tracker returns HTTP 404 because project issues
  are currently disabled. GitLab remains the primary development forge.

## Test environments

* Local: Arch Linux, R 4.6.1 Patched
* GitLab CI: Linux (rocker/r-ver:4.5.0, R-release)
* GitLab CI: Linux (rocker/r-devel, R-devel)
* GitHub Actions: macOS-latest (R-release)
* GitHub Actions: windows-latest (R-release)
* GitHub Actions: ubuntu-latest (R-release, R-devel, R-oldrel-1)
