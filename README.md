# orderly.helper

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/orderly.helper/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/orderly.helper/actions)
[![codecov.io](https://codecov.io/github/mrc-ide/orderly.helper/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/orderly.helper?branch=main)
<!-- badges: end -->

This package exists to smooth over the difference between [`orderly1`](https://vaccineimpact.org/orderly) and [`orderly2`](https://mrc-ide.github.io/orderly2) while we manage the migration between the two packages. It will allow you to refer to either (but at the same time only *one*) of the packages by its namespace, so that

```
orderly::orderly_run(...)
```

will run `orderly_run` in one of the packages.

Within an orderly repo, you can then run:

```
orderly.helper::activate()
```

which will configure everything for you.

Alternatively, run

```r
orderly.helper::use()
```

which will set up orderly based on your personal preferences.

If you don't want to think about any of this, you can call

```r
orderly.helper::auto()
```

We recommend adding to your `.Rprofile`:

```{r}
options(orderly.version = 2, orderly.helper.verbose = TRUE)
if (!require("orderly.helper", quietly = TRUE)) {
  orderly.helper::auto()
}
```

With the two options configured as you prefer.

## Installation

To install `orderly.helper`:

```r
remotes::install_github("mrc-ide/orderly.helper", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
