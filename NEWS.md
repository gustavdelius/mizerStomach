# mizerStomach 0.1.0

* Initial release.
* Added tools to validate stomach-content data and fit normal, smoothly
  truncated-exponential, and Gaussian-mixture distributions to log predator-prey
  mass ratios.
* Added diagnostics and plots for fitted distributions, including histogram and
  violin-plot views.
* Added transformations between prey-number and prey-biomass weightings.
* Added support for transferring fitted feeding kernels to and from
  `MizerParams` objects, including Gaussian-mixture kernels.
* Updated the mizer integration for mizer 3.4's S3 `MizerParams` objects by
  using the public parameter accessors, and removed the unused
  `mizerExperimental` dependency.
* Added an interactive Shiny gadget for fitting and adjusting distributions.
* Added the `barnes_data` example dataset, package articles, and an expanded test
  suite.
