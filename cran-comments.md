## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission

  The same NOTE reports the `BugReports` URL as possibly invalid and
  suggests appending `/issues`. The package is hosted on GitLab, whose
  issue tracker is served at `/-/work_items`; `/-/issues` is the GitHub
  convention and is not the path GitLab uses. The URL is correct as given.

## Test environments

* Local: Arch Linux, R 4.6.1
* GitHub Actions: macOS-latest (R-release)
* GitHub Actions: windows-latest (R-release)
* GitHub Actions: ubuntu-latest (R-release, R-devel, R-oldrel-1)

## Dependencies

`samplyr` imports `sondage` (selection algorithms) and `svyplan` (sample
size and precision planning), both submitted to CRAN by the same
maintainer. This package requires `sondage (>= 0.9.0)` and
`svyplan (>= 0.9.0)`.

## Downstream dependencies

None (new package).
