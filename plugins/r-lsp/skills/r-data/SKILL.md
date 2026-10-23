---
name: r-data
description: Write or change R code against a dataset whose columns live in the data, not the code - a CSV, parquet, an S3 object, a database table, a team loader. Use before writing tidyverse pipelines, when a column name is uncertain, when data is on S3 or behind a loader, or when r-colcheck says a source cannot be opened. Gets the real column names cheaply (r-colmap, the schema cache, discover and learn --all) so the code is checked rather than guessed.
---

# Code against data you cannot see

Column names inside `mutate()`, `filter()`, `summarise()`, `select()` and `aes()` are only checked when the checker knows the data. Get that knowledge first, then write; the post-edit note then reports a wrong name at the line that uses it.

1. **What the project already knows.** `r-colmap` prints every data source the code reads with its columns and where each column is used, dropped or created; `r-colmap COLUMN` finds every site that touches one. Read it before adding a step to an existing pipeline: the names, and the conventions, are there.
2. **Data the checker cannot open.** `r-colcheck discover` lists every source whose frame the code pipes into verbs and that has no local file and no cached schema (S3 keys, URIs, team loaders), where it is used, and the `learn` command for each; a frame that is loaded and never used in a pipeline is not listed, since nothing downstream of it needs checking. `r-colcheck learn --all` runs them (`--dry-run` to see them first); it evaluates each expression once with the shell's credentials and renv active, and records the column names and types in `.colcheck/schemas.json`. Never run `learn` from a hook or a loop: it reaches the network.
3. **Parquet or arrow on a bucket.** For `read_parquet("s3://...")`, `open_dataset("s3://...")` or a loader that reads one, learn from `arrow::open_dataset("s3://bucket/prefix/")`: it reads the file footer, not the rows. The note already suggests that expression; for a team loader, a `loaders` entry in `.colcheck.json` tells `discover` which argument is the key and what cheap expression returns the schema for it:
   ```json
   {"loaders": [{"fn": "teamdata::fetch_table", "key": "name",
                 "learn": "arrow::open_dataset(sprintf('s3://bucket/%s/', KEY))", "load_all": true}]}
   ```
   `KEY` stands for the quoted key; `source` names files to source first, `load_all` loads the package. Commit that file: the cache stays local (`.colcheck/.gitignore`), the registry is shared.
4. **A column the data gained since.** A finding you are sure is wrong, or a note saying the entry is stale (older than `schema_ttl_days`, 90 by default): `r-colcheck refresh KEY`, or `refresh --stale`.
5. **Write, then read the note.** After each Edit/Write (or Bash change) of the file, the note lists column names that cannot exist at that point and the step where a frame became unknown. `r-colcheck --steps FILE` prints the column set after every step when a finding is surprising.
