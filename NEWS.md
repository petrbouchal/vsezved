# vsezved (development version)

## Bug fixes

* `vz_*_register()` works again; it no longer relies on MŠMT's now defunct CKAN catalogue API. This is a quick fix - the proper way would be to work through the DCAT SPARQL endpoint for data.gov.cz.
* new functions `*_stistko_*()` for getting codelists ("číselník") from the stistko server
* fixed `vz_get_directory()` to accommodate changes on the server

## Improvements

* functional high-level and more granular workflows for both directories and registers
* customisable file storage locations
* `vz_*_register()` functions return tibbles with names in snake_case
* `vz_*_register()` functions allow selecting tables to extract from XML dump

# vsezved 0.1.0

* first barely working version. See [blog post](https://petrbouchal.xyz/cz/post/vsezved) announcement (in Czech only)

# vsezved 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
