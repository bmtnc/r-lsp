---
name: r-data
description: Write or change R code against a dataset whose columns live in the data, not the code - a CSV, parquet, an S3 object, a database table, a team loader. Use before writing tidyverse pipelines, when a column name is uncertain, when data is on S3 or behind a loader, or when r-colcheck says a source cannot be opened. Gets the real column names cheaply (r-colmap, the schema cache, discover and learn --all) so the code is checked rather than guessed.
---

# Code against data you cannot see

Column names inside `mutate()`, `filter()`, `summarise()`, `select()` and `aes()` are only checked when the checker knows the data. Get that knowledge first, then write; the post-edit note then reports a wrong name at the line that uses it.

1. **What the project already knows.** `r-colmap` prints every data source the code reads with its columns and where each column is used, dropped, created or joined in, then every function parameter a body uses as a frame with the columns it reads (what the function expects of the frame it is given); `r-colmap COLUMN` finds every site that touches one. Read it before adding a step to an existing pipeline: the names, and the conventions, are there. Read the header first: `N of M pipeline steps followed` is how much of the code the map saw, and a low ratio means step 2 below before anything else.
2. **Data the checker cannot open.** `r-colcheck discover` lists every source whose frame the code pipes into verbs and that has no local file and no cached schema (S3 keys, URIs, team loaders), where it is used, and the `learn` command for each; a frame that is loaded and never used in a pipeline is not listed, since nothing downstream of it needs checking. `r-colcheck learn --all` runs them (`--dry-run` to see them first); it evaluates each expression once with the shell's credentials and renv active, and records the column names and types in `.colcheck/schemas.json`. Never run `learn` from a hook or a loop: it reaches the network.
3. **Parquet or arrow on a bucket.** For `read_parquet("s3://...")`, `open_dataset("s3://...")` or a loader that reads one, learn from `arrow::open_dataset("s3://bucket/prefix/")`: it reads the file footer, not the rows. The note already suggests that expression; for a team loader, a `loaders` entry in `.colcheck.json` tells `discover` which argument is the key and what cheap expression returns the schema for it:
   ```json
   {"loaders": [{"fn": "teamdata::fetch_table", "key": "name",
                 "learn": "arrow::open_dataset(sprintf('s3://bucket/%s/', KEY))", "load_all": true}]}
   ```
   `KEY` stands for the quoted key; `source` names files to source first, `load_all` loads the package. Commit that file: the cache stays local (`.colcheck/.gitignore`), the registry is shared.
4. **A column the data gained since.** A finding you are sure is wrong, or a note saying the entry is stale (older than `schema_ttl_days`, 90 by default): `r-colcheck refresh KEY`, or `refresh --stale`.
5. **Write, then read the note.** After each Edit/Write (or Bash change) of the file, the note lists column names that cannot exist at that point and the step where a frame became unknown. `r-colcheck --steps FILE` prints the column set after every step when a finding is surprising.
6. **Which columns does nothing read?** Run `r-colmap`, check the header ratio, and read the rest of each `no use seen in the steps followed` line before calling anything dead: `returned by f()` means the callers have it (`r-callmap f` lists them), `written by saveRDS()` means it reached a file or a bucket, `tracking ended at ... (verb)` means the checker lost the frame there (`r-colcheck --steps FILE` shows the step; a `# cols: KEY` on that statement covers what follows). Only `no later step carries it` is a candidate, and `grep` for the name as a string (`df[["x"]]`, `all_of()`) settles it.
