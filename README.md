# kelp

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/kelp/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/kelp/actions)
[![codecov.io](https://codecov.io/github/mrc-ide/kelp/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/kelp?branch=main)
<!-- badges: end -->

## Background

This is an R wrapper around [SeaweedFS](https://github.com/chrislusf/seaweedfs). It currently makes functions available to:
 * Upload a file
 * Read a file
 * Delete a file
 
## Usage

This package provides 3 class types to interact with SeaweedFS
* seaweed_master - low level client exposes functions from master server API, designed to match as close to [master api spec](https://github.com/chrislusf/seaweedfs/wiki/Master-Server-API) as possible
* seaweed_volume - low level client exposes functions from volume server API, designed to match as close to [volume api spec](https://github.com/chrislusf/seaweedfs/wiki/Volume-Server-API) as possible
* kelp - wrapper for master and volume APIs which provides some higher level functions for interacting with SeaweedFS. Planning to support e.g. uploading and downloading raw R objects into the store in future.

## Installation

To install `kelp`:

```r
remotes::install_github("mrc-ide/kelp", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
