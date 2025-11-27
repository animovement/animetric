#' @keywords internal
circ_check <- function() {
  # Check that circular is installed
  rlang::check_installed(
    "circular",
    reason = "for calculating circular descriptive statistics,",
    action = function(...) {
      utils::install.packages(
        'circular',
        repos = c(
          'https://animovement.r-universe.dev',
          'https://cloud.r-project.org'
        )
      )
    }
  )
}

#' @keywords internal
dt_check <- function() {
  # Check that data.table is installed
  rlang::check_installed(
    "data.table",
    reason = "for computing rolling tortuosity measures",
    action = function(...) {
      utils::install.packages(
        'data.table',
        repos = c(
          'https://animovement.r-universe.dev',
          'https://cloud.r-project.org'
        )
      )
    }
  )
}
