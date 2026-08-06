# Sourced automatically by testthat::test_dir()/test_file() before any
# test-*.R file runs. This app is a plain Shiny app (not an R package), so
# there's no load_all()/devtools equivalent - the .lfs_* helper functions
# under test have to be sourced explicitly here instead.
#
# NOTE: testthat changes the working directory to tests/testthat itself for
# the duration of the run (confirmed empirically - getwd() inside a test is
# ".../lipidflowshiny/tests/testthat", NOT the project root), even though
# the project is launched via `Rscript -e "testthat::test_dir('tests/testthat')"`
# from the project root. This helper itself runs with that same working
# directory (also confirmed empirically), so the project root is simply two
# levels up from getwd() at the time this file is sourced - no need for
# fragile call-stack introspection to find "this file's own path".
project_root <- normalizePath(file.path(getwd(), "..", ".."))
r_files <- list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)
if (length(r_files) == 0) {
  stop("helper-source-app.R: no R/ source files found under '", project_root, "' - ",
       "check that the project layout hasn't changed.")
}
for (f in r_files) source(f, chdir = FALSE)

# A handful of .lfs_* functions accept a db_dir/data-dir argument that
# defaults to LFS_DEFAULT_DB_DIR (normally set in global.R, which tests
# deliberately don't source). Define it here too so functions that fall
# back to it still find the real bundled data/*.rda files during tests.
LFS_DEFAULT_DB_DIR <- file.path(project_root, "data")
