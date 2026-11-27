# CSI Pacific ACL Return to Performance Protocol <img src="images/csi-logo-medal-only.png" align="right" height="40"/>

**Description:**

Quarto website prototype for the CSI Pacific ACL reconstruction Return to Performance (RTP) protocol.
Includes the clinical rehabilitation guide (pre-op to Phase 5), phase-by-phase progress reports, athlete wellness and force plate dashboards, and a Shiny app for entering test results.
The long-term goal is a reproducible template that generates a custom protocol for each athlete enrolled in the program.

> All athlete data in this repository is fictional sample data for a placeholder athlete ("Frank Reynolds").

**Status:**

`Prototype 🧪`: in active development, not production ready

**Directory Structure**:

```
├── _quarto.yml             // website configuration (navbar, sidebar, theme)
├── index.qmd               // home page / athlete overview dashboard
├── about.qmd               // about page
├── docs/guide/             // the ACL rehabilitation guide
├── checklists/             // phase progress reports
├── dashboards/             // wellness and force plate dashboards
├── data-collection-app/    // Shiny app for entering test results
├── R/                      // shared R code (Global.R, plot_functions.R, Themes.R)
├── athlete/                // per-athlete inputs: intake form, data, photo (fictional sample athlete)
├── protocol/               // protocol criteria, the same for every athlete
├── images/                 // logos and site images
└── _site/                  // rendered website (committed, see Workflows)
```

**Building the Site**:

1. Open `CSI-Pacific-RTP.Rproj` in RStudio. `renv` activates automatically.
2. First time on a new computer: run `renv::restore()` in the R console to install the exact package versions recorded in `renv.lock`.
   (Because the project lives in OneDrive, set `RENV_PATHS_LIBRARY_ROOT` in your user-level `~/.Renviron` so the package library is stored outside OneDrive.)
   
**Setting up a New Athlete**:

Everything specific to one athlete lives in `athlete/`:

-   `ACL-Intake-Form.xlsx`: name, sport, injury and surgery details (drives the athlete name and rehab timeline across the site)
-   `athlete.png`: photo shown on the home page
-   `FP-Database.xlsx`, `outcome_data.xlsx`, `tindeq_results.csv`: test and monitoring data (the data collection app writes to the last two)

Replace these files with the new athlete's, then render the site.

**Workflows**:

Published on Posit Connect Cloud, which redeploys when a push is synced to the `main` branch.
Connect Cloud builds `index.qmd` itself; all other pages are served from the committed `_site/` folder, so render locally and commit `_site/` along with source changes.
Connect Cloud installs R packages from `renv.lock`. After installing or updating packages, run `renv::snapshot()` and commit the updated `renv.lock`.

**Environment Variables**:

None at present. All data is local sample data; a connection to the organizational database is planned.

**Additional Documents**:

None

**Additional Notes**:

Refer to [GitHub Issues](https://github.com/AKates717/CSI-Pacific-RTP/issues) for outstanding requests.

**Maintainers & Contacts**:

-   CSIP Practitioner Contact: [Andrew Kates](mailto:akates@csipacific.ca)
-   Repository Maintainer: [Andrew Kates](mailto:akates@csipacific.ca)