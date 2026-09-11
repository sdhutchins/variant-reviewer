# About page: what the app is, the annotations it pulls together, and where the
# data comes from. Static content built from bslib cards so it matches the
# dashboard's look.

# One row per annotation card on the dashboard: the label, its source, and a
# one-line description of what it contributes. Kept as data so the table below
# stays in step with the dashboard grid.
.about_annotations <- list(
  list(
    "Gene summary",
    "MyGene.info",
    "Gene name, function summary, and cross-referenced identifiers (Entrez, Ensembl, UniProt)."
  ),
  list(
    "Variant annotation",
    "ProtVar and MyVariant.info",
    paste(
      "Normalized genomic and protein change, gene, consequence, rsID,",
      "and ClinVar significance."
    )
  ),
  list(
    "ProtVar predictions",
    "ProtVar (EMBL-EBI)",
    "AlphaMissense, CADD, ESM-1b, FoldX, and Missense3D for the selected substitution."
  ),
  list(
    "Additional in-silico predictions",
    "dbNSFP (MyVariant.info)",
    "Additional REVEL, PolyPhen-2, SIFT, and MetaLR/SVM prediction scores."
  ),
  list(
    "Protein context",
    "ProtVar (EMBL-EBI)",
    "Protein function and catalogued substitutions at the selected residue."
  ),
  list(
    "Protein domains & features",
    "UniProt (EBI Proteins)",
    "Domains, regions, and sites, flagging which one the variant residue falls in."
  ),
  list(
    "3D structure",
    "AlphaFold DB",
    "Predicted 3D structure with the variant residue highlighted (r3dmol viewer)."
  ),
  list(
    "Clinical significance",
    "ClinVar (NCBI)",
    "Curated clinical interpretations for the variant."
  ),
  list(
    "Population frequency",
    "gnomAD",
    "Allele frequencies across reference populations."
  ),
  list(
    "Gene constraint",
    "gnomAD",
    "pLI, LOEUF, and Z-scores for tolerance to loss-of-function and missense variation."
  ),
  list(
    "Variant consequences",
    "Ensembl VEP",
    "Predicted molecular consequences of the variant."
  ),
  list("Tissue expression", "GTEx", "Gene expression across human tissues."),
  list(
    "Protein interactions",
    "STRING",
    "Predicted and known interaction partners."
  ),
  list(
    "Disease associations",
    "Open Targets",
    "Gene-disease association evidence."
  ),
  list(
    "Phenotypes (HPO)",
    "Monarch Initiative",
    "Human Phenotype Ontology terms associated with the gene."
  ),
  list(
    "Known drugs",
    "Open Targets",
    "Drugs and clinical candidates targeting the gene, with clinical stage."
  ),
  list(
    "Pharmacogenomics",
    "Open Targets",
    "Variant and genotype effects on drug response, with evidence levels."
  ),
  list(
    "Literature",
    "Europe PMC",
    "Recent publications for the gene, refined by the variant when loaded."
  ),
  list(
    "External links",
    "derived",
    "Deep links to external resources for the gene and variant."
  ),
  list(
    "AI assistant",
    "BYOK (Gemini / OpenAI / Anthropic)",
    "Optional chat grounded in the gene and variant currently on screen."
  )
)

.about_annotation_row <- function(entry) {
  tags$tr(
    tags$td(tags$strong(entry[[1]])),
    tags$td(entry[[2]]),
    tags$td(class = "text-muted", entry[[3]])
  )
}

about_annotations_table_data <- function() {
  data.frame(
    Annotation = vapply(.about_annotations, `[[`, character(1), 1),
    Source = vapply(.about_annotations, `[[`, character(1), 2),
    `What it shows` = vapply(.about_annotations, `[[`, character(1), 3),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Versions here describe what the code explicitly selects. "Provider current"
# is intentional when an upstream service updates data behind an unversioned
# endpoint, because claiming a frozen release would be misleading.
.about_provenance <- list(
  list(
    "ProtVar",
    "API 2.0",
    paste(
      "Data 2.1; UniProt 2025_01; Ensembl 113; CADD v1.7;",
      "popEVE 2025.03; Missense3D 2026.02; dbSNP b156; COSMIC v103;",
      "ClinVar 2025-02; gnomAD v4.1.0"
    ),
    "https://www.ebi.ac.uk/ProtVar/api/swagger-ui/index.html",
    "https://www.ebi.ac.uk/ProtVar/about"
  ),
  list(
    "MyGene.info",
    "REST API v3",
    "Provider current; not pinned by the API route",
    "https://docs.mygene.info/en/latest/"
  ),
  list(
    "MyVariant.info",
    "REST API v1",
    "Provider current; not pinned by the API route",
    "https://docs.myvariant.info/en/latest/"
  ),
  list(
    "ClinVar",
    "NCBI E-utilities",
    "Live ClinVar records; not pinned",
    "https://www.ncbi.nlm.nih.gov/books/NBK25500/"
  ),
  list(
    "UniProt",
    "EBI Proteins API",
    "Provider current; not pinned by the API route",
    "https://www.ebi.ac.uk/proteins/api/doc/"
  ),
  list(
    "AlphaFold DB",
    "AlphaFold DB API",
    "Latest model version returned for each accession",
    "https://alphafold.ebi.ac.uk/api-docs"
  ),
  list(
    "gnomAD",
    "GraphQL API",
    "gnomAD v4 dataset (gnomad_r4); GRCh38 constraint",
    "https://gnomad.broadinstitute.org/help/whats-new"
  ),
  list(
    "GTEx",
    "REST API v2",
    "GTEx v8 (gtex_v8)",
    "https://gtexportal.org/api/v2/redoc"
  ),
  list(
    "Ensembl",
    "Ensembl REST API",
    "Provider current; GRCh38; release not pinned",
    "https://rest.ensembl.org/documentation"
  ),
  list(
    "STRING",
    "STRING API",
    "Provider current; human taxonomy 9606",
    "https://string-db.org/help/api/"
  ),
  list(
    "Open Targets",
    "GraphQL API v4",
    "Provider current Platform release; not pinned",
    "https://platform-docs.opentargets.org/data-access/graphql-api"
  ),
  list(
    "Monarch Initiative",
    "REST API v3",
    "Provider current knowledge graph; not pinned",
    "https://api.monarchinitiative.org/v3/docs"
  ),
  list(
    "Europe PMC",
    "REST web service",
    "Provider current; response version supplied by Europe PMC",
    "https://europepmc.org/RestfulWebService"
  )
)

.about_provenance_row <- function(entry) {
  tags$tr(
    tags$td(tags$strong(entry[[1]])),
    tags$td(entry[[2]]),
    tags$td(class = "text-muted", entry[[3]]),
    tags$td(
      tags$a(
        href = entry[[4]],
        target = "_blank",
        rel = "noopener",
        "Documentation"
      ),
      if (length(entry) >= 5) {
        tagList(
          " · ",
          tags$a(
            href = entry[[5]],
            target = "_blank",
            rel = "noopener",
            "Release notes"
          )
        )
      }
    )
  )
}

about_provenance_table_data <- function() {
  data.frame(
    Source = vapply(.about_provenance, `[[`, character(1), 1),
    API = vapply(.about_provenance, `[[`, character(1), 2),
    `Data release used` = vapply(.about_provenance, `[[`, character(1), 3),
    Documentation = vapply(.about_provenance, `[[`, character(1), 4),
    `Release notes` = vapply(
      .about_provenance,
      function(entry) if (length(entry) >= 5) entry[[5]] else "",
      character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

.about_card_header <- function(title, copy = FALSE, download_id = NULL) {
  card_header(
    class = "d-flex justify-content-between align-items-center gap-2",
    tags$h2(title, class = "h6 mb-0"),
    if (isTRUE(copy) || !is.null(download_id)) {
      tags$div(
        class = "d-flex align-items-center gap-2",
        if (isTRUE(copy)) vr_copy_button(paste("Copy", title, "text")),
        if (!is.null(download_id)) {
          vr_csv_button(download_id, paste("Download", title, "as CSV"))
        }
      )
    }
  )
}

about_page <- tagList(
  layout_columns(
    col_widths = 12,
    card(
      .about_card_header("Overview", copy = TRUE),
      card_body(
        tags$p(
          "Variant Reviewer is a lightweight gene and variant interpretation ",
          "companion. Start with a gene symbol and optionally select one of ",
          "its notable variants, or enter a variant directly. A variant ",
          "resolves its gene, so the dashboard pulls ",
          "together, in one place, what the gene does, where it is expressed, ",
          "what it interacts with, and protein- and clinical-level context for ",
          "the variant."
        ),
        tags$p(
          class = "mb-0",
          "It is a thin, reactive front end over several public bioinformatics ",
          "APIs. The gene is resolved once and shared across every card, so ",
          "each source is queried only when needed, and successful responses ",
          "are cached in-process so repeated searches are instant."
        )
      )
    )
  ),
  layout_columns(
    col_widths = 12,
    card(
      .about_card_header("Scope", copy = TRUE),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          div(
            tags$h3("What it does", class = "h6"),
            tags$ul(
              class = "mb-0",
              tags$li(
                "Reviews one human gene and optionally one variant at a time."
              ),
              tags$li(
                "Aggregates public annotations, read-only, onto one dashboard."
              ),
              tags$li(
                "Links out to the primary sources for deeper investigation."
              )
            )
          ),
          div(
            tags$h3("What it doesn't", class = "h6"),
            tags$ul(
              class = "mb-0",
              tags$li("Batch or VCF-scale analysis, or variant calling."),
              tags$li("Non-human species."),
              tags$li(
                "Clinical diagnosis, treatment, or genetic-counseling advice."
              )
            )
          )
        ),
        tags$p(
          class = "text-muted small mb-0 mt-3",
          tags$strong("Assistant: "),
          "the chat helps interpret the gene or variant on screen and answers ",
          "general genomics questions within that scope. It is not a source of ",
          "medical, diagnostic, or treatment advice."
        )
      )
    )
  ),
  layout_columns(
    col_widths = 12,
    card(
      .about_card_header(
        "Annotations available",
        download_id = "about_annotations_download"
      ),
      card_body(
        tags$table(
          class = "table table-sm align-middle mb-0",
          tags$thead(
            tags$tr(
              tags$th("Annotation"),
              tags$th("Source"),
              tags$th("What it shows")
            )
          ),
          tags$tbody(
            lapply(.about_annotations, .about_annotation_row)
          )
        )
      )
    )
  ),
  layout_columns(
    col_widths = 12,
    card(
      .about_card_header("Input validation", copy = TRUE),
      card_body(
        tags$p(
          "Before any external API is queried, the gene symbol and variant you ",
          "enter are validated with the ",
          tags$a(
            href = "https://github.com/samuelbharti/biobouncer",
            target = "_blank",
            rel = "noopener",
            tags$strong("biobouncer")
          ),
          " package, using its offline ",
          tags$em("pattern"),
          " mode: a fast, reproducible, network-free grammar check. Gene ",
          "symbols are checked against the HGNC symbol grammar and rsIDs against ",
          "the dbSNP grammar. ProtVar then resolves HGVS, VCF fields, genomic ",
          "coordinates, UniProt protein notation, and gnomAD, dbSNP, ClinVar, ",
          "or COSMIC identifiers to one normalized variant record. Genomic ",
          "alleles are reported as GRCh38 RefSeq HGVS, such as ",
          "NC_000006.12:g.13305184G>A."
        ),
        tags$p(
          class = "mb-0",
          "Malformed input is rejected up front with an inline message, so no ",
          "lookup is ever fired on an obviously bad identifier. This applies ",
          "both to what you type and to what the assistant loads. It is a ",
          "format check, not an existence check: whether the identifier actually ",
          "exists is confirmed by the resolving services (MyGene and ProtVar)."
        )
      )
    )
  ),
  layout_columns(
    col_widths = 12,
    card(
      .about_card_header(
        "Data provenance and versions",
        download_id = "about_provenance_download"
      ),
      card_body(
        tags$p(
          "This table records the API contract and data release selected by ",
          "the app. Provider-current services can change upstream without a ",
          "code change, so they are identified as unpinned rather than given ",
          "a potentially stale release number."
        ),
        tags$div(
          class = "table-responsive",
          tags$table(
            class = paste(
              "table table-sm align-middle mb-0",
              "vr-provenance-table"
            ),
            tags$thead(tags$tr(
              tags$th("Source"),
              tags$th("API"),
              tags$th("Data release used"),
              tags$th("Reference")
            )),
            tags$tbody(lapply(.about_provenance, .about_provenance_row))
          )
        )
      )
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      .about_card_header("Data sources & privacy", copy = TRUE),
      card_body(
        tags$p(
          "All data sources are public and require no API key. Queries are ",
          "sent directly to the upstream providers listed above."
        ),
        tags$p(
          class = "mb-0",
          "The optional AI assistant is ",
          tags$strong("bring your own key (BYOK)"),
          ": your API key is held only in this session's server memory, is ",
          "never written to disk, and is redacted from any surfaced error. You ",
          "can also supply a key via an environment variable ",
          "(",
          tags$code("GEMINI_API_KEY"),
          ", ",
          tags$code("OPENAI_API_KEY"),
          ", or ",
          tags$code("ANTHROPIC_API_KEY"),
          ")."
        )
      )
    ),
    card(
      .about_card_header("About this build", copy = TRUE),
      card_body(
        vr_field("Version", app_version()),
        tags$p(
          class = "mb-0",
          "Built with Shiny and bslib. See the ",
          tags$a(
            href = "https://github.com/samuelbharti/variant-reviewer",
            target = "_blank",
            rel = "noopener",
            "project repository"
          ),
          " for source, documentation, and contribution guidelines."
        )
      )
    )
  )
)
