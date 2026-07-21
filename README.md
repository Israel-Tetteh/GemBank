# GemBank: A Modern Seed Bank Management System

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Code of Conduct](https://img.shields.io/badge/Contributor%20Covenant-v2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)
<!-- badges: end -->

GemBank is a comprehensive and modern Shiny application designed to manage the entire lifecycle of a breeding program's genetic resources. It provides an intuitive interface for data entry, inventory management, trial creation, and powerful querying, all backed by a robust SQLite database with a full audit trail.

Built with the `{golem}` framework, GemBank is a robust, production-ready application that can be easily deployed and maintained.

## Features

-   **Database Setup**: Easily initialize a new SQLite database or connect to an existing one with a user-friendly file browser.
-   **Master Registry**: A sequential, accordion-style data entry workflow for:
    -   **Germplasm**: Register new accessions with detailed passport information.
    -   **Traits**: Define a controlled vocabulary for phenotypic measurements.
    -   **Trials**: Create and describe experimental trials with full metadata.
    -   **Plots**: Map accessions to specific plots within a trial.
    -   **Observations**: Record phenotypic data from the field.
-   **Inventory Management**:
    -   Register new physical seed lots with detailed provenance and quality metrics.
    -   Withdraw seeds for various purposes with a confirmation step to prevent errors.
    -   View live vault balances and a complete history of all lot movements.
-   **Passport Explorer**: A 360-degree view of any accession, including its passport data, current inventory status, historical trial performance, and a complete audit trail.
-   **Species Explorer**: Get a high-level summary of your germplasm collection, aggregated by species, and click any accession to jump directly to its passport.
-   **Powerful Query Hub**: A dedicated section with tools to search, filter, and export data from all major tables:
    -   Germplasm Query
    -   Inventory Query
    -   Trial Query
    -   Plot Query
-   **Reactive State Management**: A global refresh trigger ensures that all modules and data views stay perfectly in sync with database changes in real-time, without requiring manual page reloads.

## Installation

You can install the development version of GemBank from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("Israel-Tetteh/GemBank")
```

## Usage

After installation, you can launch the GemBank application by running the following commands in your R console:

```r
library(GemBank)
run_app()
```

The application will launch in your default web browser, starting with the database connection screen.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Code of Conduct

Please note that the GemBank project is released with a Contributor Code of Conduct. By contributing to this project, you agree to abide by its terms.