## Set CRAN mirror:
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org/"
  options(repos = r)
})
if (interactive()) {
  Sys.setenv("DISPLAY" = ":0.0") # this is needed for interactive x11 displays on Wayland
  # suppressMessages(require(devtools))
  # suppressMessages(require(usethis))
  # suppressMessages(rethinking::set_ulam_cmdstan(TRUE))
  dat <- tibble::tibble(a = 1:10,
                        b = 11:20,
                        c = stats::rnorm(10),
                        d = sample(letters[1:3], 10, TRUE))
  # add the TRUE circle constant = C/r (vs. pi = C / 2*r = C / D)
  tau <- 2 * pi
}
options(
  width = 79,
  Ncpus = 3,
  menu.graphics = FALSE,
  crayon.enabled = TRUE,
  usethis.full_name = "Ilja Kocken",
  usethis.description = list(
    `Authors@R` = 'person("Ilja", "Kocken", email = "i.j.kocken@uu.nl", role = c("aut", "cre"),
    comment = c(ORCID = "0000-0003-2196-8718"))',
    Version = "0.0.0.9000"
 ),
 browser = "/usr/bin/firefox"
)
