"""Cosine distance between a paper's cited journals and its author's prior work."""
import numpy as np
import pandas as pd


def groupby_count(df, colgroupby, colcountby, count_unique=True):
    groups = df.groupby(colgroupby, sort=False, as_index=False)[colcountby]
    counts = groups.nunique() if count_unique else groups.count()
    return counts.rename(columns={colcountby: str(colcountby) + 'Count'})


def pandas_cosine_similarity(df1, df2, col_keys, col_values):
    """Return 1 - cosine similarity; undefined/empty vectors yield NaN."""
    left = df1.dropna(subset=[col_keys, col_values]).groupby(col_keys)[col_values].sum()
    right = df2.dropna(subset=[col_keys, col_values]).groupby(col_keys)[col_values].sum()
    denom = np.linalg.norm(left.to_numpy()) * np.linalg.norm(right.to_numpy())
    if not np.isfinite(denom) or denom == 0:
        return np.nan
    common = left.index.intersection(right.index)
    similarity = np.dot(left.loc[common].to_numpy(), right.loc[common].to_numpy()) / denom
    return float(1.0 - np.clip(similarity, -1.0, 1.0))


def author_switch(authordf, previous_k=None, year_window=3):
    """Compare each paper only with earlier papers in this author's ordered rows.

    Papers are ordered by Year then PublicationId, as in the analysis notebooks.
    Within-year ties therefore use the publication ID order, not an inferred date.
    The inclusive lower year bound is current_year - year_window.
    """
    required = {'PublicationId', 'Year', 'CitedJournalId', 'CitedJournalCount'}
    missing = required - set(authordf.columns)
    if missing:
        raise ValueError('Missing columns: ' + ', '.join(sorted(missing)))
    if previous_k is not None and (not isinstance(previous_k, int) or previous_k < 1):
        raise ValueError('previous_k must be None or a positive integer')
    if year_window is not None and year_window < 0:
        raise ValueError('year_window must be None or nonnegative')
    frame = authordf.sort_values(['Year', 'PublicationId', 'CitedJournalId']).reset_index(drop=True)
    groups = list(frame.groupby('PublicationId', sort=False))
    results = []
    for index, (paper_id, current) in enumerate(groups):
        start = max(0, index - previous_k) if previous_k is not None else 0
        preceding = groups[start:index]
        score = np.nan
        if preceding:
            history = pd.concat([group for _, group in preceding], ignore_index=True)
            if year_window is not None:
                history = history[history['Year'] >= current['Year'].iloc[0] - year_window]
            score = pandas_cosine_similarity(history, current, 'CitedJournalId', 'CitedJournalCount')
        results.append((paper_id, score))
    return pd.DataFrame(results, columns=['PublicationId', 'switchScore'])
