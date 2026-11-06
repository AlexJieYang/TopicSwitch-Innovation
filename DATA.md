# Data fields

`switch4ref2.csv` contains `AuthorID`, `PaperID`, `Year`, `Age`, `FieldID`, `FieldID_AU`, `JournalID`, `switchScore`, `P(gf)`, `H-index`, `Past_Pub`, `Funding`, `Reference_Count`, `Team_Size`, `International`, `InterDisTeam`, `FocalField`, `Disruption`, `Disruption5`, `Disruption10`, `NDC`, `NCC`, `Atyp_Median_Z`, `RaoStirling_Ref`, `Citation_Count`, `Hits5`, `EC`, `Patent_Count`, and `NCT_Count`. R's default CSV name conversion reads `H-index` as `H.index`. Prepared variables and units follow the study definitions.

The switch score is one minus the cosine similarity of cited-journal frequency vectors. Each author's papers are ordered by year and then paper ID. Only earlier papers in that order can enter history; same-year ordering is by ID. The default inclusive history bound is `current_year - 3`. `previous_k=None` uses all earlier papers in that window. Empty history or a zero/undefined vector yields a missing score. Duplicate author-paper pairs are removed before the score calculation.

The first two reference columns retain the original citing/cited order contract. Do not reverse them. Join score outputs to study covariates by author and paper identifiers using the study selection rules.
