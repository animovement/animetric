# animetric

*An R package for calculating movement-based metrics*

The primary aim of the *animetric* package is to provide methods for
calculating movement-based metrics, including kinematics, navigational
statistics, and social interaction measures.

## Installation

You can install the development version of *animetric* with:

``` r

install.packages('animetric', repos = c('https://animovement.r-universe.dev', 'https://cloud.r-project.org'))
```

Once you have installed the package, you can load it with:

``` r

library("animetric")
```

## Lifecycle

*animetric* is under active development.
[`add_orientation()`](https://animovement.dev/animetric/reference/add_orientation.md),
[`compute_min_step()`](https://animovement.dev/animetric/reference/compute_min_step.md)
and
[`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md)
are experimental, as are the `vertical` and `min_step` arguments of
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md),
`step_length` of
[`add_tortuosity()`](https://animovement.dev/animetric/reference/add_tortuosity.md),
and `min_step` and `step_length` of
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md):
they may still change without a deprecation cycle. Everything else is
stable and changes only through one.

## Citation

If you enjoy the package, please make sure to cite it. If you find a
bug, feel free to open an issue.

To cite *animetric* in publications use:

``` r

citation("animetric")
#> To cite animetric in publications, please cite the animovement toolbox
#> as a whole (the first entry below). If your work used only animetric,
#> you may cite the package directly instead (the second entry).
#> 
#>   Roald-Arbøl M (2026). "animovement: An R toolbox for analysing
#>   movement across space and time." doi:10.5281/zenodo.13235277
#>   <https://doi.org/10.5281/zenodo.13235277>.
#>   <https://animovement.dev/animovement/>.
#> 
#>   Roald-Arbøl M (2026). "animetric: An R package for calculating
#>   movement-based metrics." doi:10.5281/zenodo.17818613
#>   <https://doi.org/10.5281/zenodo.17818613>.
#>   <https://animovement.dev/animetric/>.
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
```
