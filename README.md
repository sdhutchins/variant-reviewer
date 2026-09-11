# Variant Reviewer

[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.21934011-1682D4)](https://doi.org/10.5281/zenodo.21934011)

By [Samuel Bharti](https://www.samuelbharti.com)

This repository is a fork of
[Samuel Bharti's Variant Reviewer](https://github.com/samuelbharti/variant-reviewer).
Key additions in this fork include:

- Gene and variant search modes with ProtVar-based normalization.
- A dedicated ProtVar predictions card for AlphaMissense, CADD, ESM-1b, FoldX,
  and Missense3D.
- Downloadable CSV tables and copy controls for text cards.
- A floating bring-your-own-key assistant that stays outside the dashboard
  layout.

Variant Reviewer is an R Shiny app for reviewing one human gene and, optionally,
one variant. It brings public gene, clinical, population, protein, structural,
expression, interaction, disease, drug, and literature annotations into one
dashboard. It is not intended for clinical diagnosis or treatment decisions.

## Install and run

Variant Reviewer requires R 4.3 or newer. Its packages are managed with `renv`.

```r
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
renv::restore()
shiny::runApp()
```

Input validation uses
[Samuel Bharti's biobouncer](https://github.com/samuelbharti/biobouncer), which
is restored from the project lockfile.

## Usage

Choose a gene symbol or enter a ProtVar-supported variant, including HGVS, VCF
fields, genomic coordinates, UniProt protein notation, or a gnomAD, dbSNP,
ClinVar, or COSMIC identifier. When an input has multiple canonical mappings,
the app asks you to select the intended variant or residue.

Tables can be downloaded as CSV files, and text cards include copy controls.
The optional floating assistant supports bring-your-own-key access to Gemini,
OpenAI, or Anthropic models. The app's About page records its data sources,
versions, and provenance links.

## Testing

```r
shiny::runTests(".")
```

The suite includes offline parser fixtures, Shiny module tests, and optional
live-browser tests.

## Contributing

Issues and pull requests are welcome. Read [CONTRIBUTING.md](CONTRIBUTING.md)
and the [Code of Conduct](CODE_OF_CONDUCT.md) first. Report security problems
using the process in [SECURITY.md](SECURITY.md), not a public issue.

## Citation

Zenodo archives each release. Use the DOI for the version you consulted from
the [Zenodo record](https://doi.org/10.5281/zenodo.21934011).
[CITATION.cff](CITATION.cff) contains the complete metadata, and
[CITATION.md](CITATION.md) provides text and BibTeX formats.

## License

[MIT](LICENSE). Copyright (c) 2026 Samuel Bharti.
