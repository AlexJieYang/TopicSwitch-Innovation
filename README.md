# TopicSwitch-Innovation

Author-level topic-switch scores, descriptive figures, and fixed-effects regressions of scientific outcomes.

Official faculty profile: [Alex J. Yang — Nanjing University](https://im.nju.edu.cn/info/1103/53391.htm).

## Environment

Use Python 3.11 or 3.12. Install the declared Python dependencies from the repository root:

```bash
python -m venv .venv
source .venv/bin/activate
python -m pip install -r requirements.txt
```

On Windows, activate with `.venv\Scripts\Activate.ps1`. Select the same environment in JupyterLab. 

R scripts require R 4.3 or later and packages `fixest`, `data.table`, `dplyr`, and `jsonlite`:

```r
install.packages(c("fixest", "data.table", "dplyr", "jsonlite"))
```

## Inputs and outputs

Set `data_dir` and `output_dir` in `config.json`, or use the `PROJECT_DATA_DIR` and `PROJECT_OUTPUT_DIR` environment variables. Relative directories resolve from the repository root. `PROJECT_CONFIG` selects another JSON configuration file. `DATA.md` describes the data fields and units used by the analyses.

Generated figures are stored under `outputs/<notebook>/`; R tables use `outputs/tables/`. Set `PROJECT_OUTPUT_DIR` to choose another output directory. Start notebooks and R scripts with the repository root as the working directory. Output directories are created when needed.

Bootstrap figures default to 10,000 resamples and seed 1. `BOOTSTRAP_RESAMPLES` (minimum 1,000) and `RANDOM_SEED` configure these values. Bootstrap calculations use bounded batches where supported.

## Workflow

1. Set the data directory for author-paper relations, reference edges, and paper metadata.
2. Open `Switch score.ipynb` and run its cells in order to write `outputs/switch_scores.csv`. This table has `AuthorId`, `PublicationId`, and `switchScore`.
3. Use the prepared study covariates for the fixed-effects regressions.
4. Run `Rscript "reg .R"` for tables, then open `Plot Reg.ipynb`. Tables are read from the configured data directory or `outputs/tables/`.
5. Open `Distribution and trend.ipynb` for descriptive figures.

## Source validation

```bash
python scripts/check_sources.py
```

This command parses Python sources, notebook cell sources, and JSON without executing research code or loading research data. The GitHub workflow performs the same static checks. Notebooks are stored with empty outputs and no execution counts.

## Citation and reuse

Use `CITATION.cff` for the software repository and cite the associated research publication separately. 

The code and repository documentation are licensed under MIT; see `LICENSE`.
Third-party datasets, external map data, and research publications retain their original rights and terms; the software license does not grant rights to those materials.
