# CSI Pacific ACL Return to Performance Protocol <img src="images/csi-logo-medal-only.png" align="right" width="120"/>

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
├── sample_data/            // fictional sample data and protocol criteria
├── images/                 // logos and site images
└── _site/                  // rendered website (committed, see Workflows)
```

**Building the Site**:

1. Open `CSI-Pacific-RTP.Rproj` in RStudio.
2. Render the full site from the Terminal: `quarto render`
3. All R code runs from the project root (`execute-dir: project` in `_quarto.yml`), so paths are written relative to the root, e.g. `source("R/Global.R")`.

**Workflows**:

Published on Posit Connect Cloud, which redeploys when a push is synced to the `main` branch.
Connect Cloud builds `index.qmd` itself; all other pages are served from the committed `_site/` folder, so render locally and commit `_site/` along with source changes.

**Environment Variables**:

None at present. All data is local sample data; a connection to the organizational database is planned.

**Additional Documents**:

None

**Additional Notes**:

Refer to [GitHub Issues](https://github.com/AKates717/CSI-Pacific-RTP/issues) for outstanding requests.

**Maintainers & Contacts**:

-   CSIP Practitioner Contact: [Andrew Kates](mailto:akates@csipacific.ca)
-   Repository Maintainer: [Andrew Kates](mailto:akates@csipacific.ca)