"""Plot coefficient estimates and standard-error intervals from fixest tables."""
import re
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

NUMBER = r'[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?'
COEFFICIENT = re.compile(rf'(?P<estimate>{NUMBER})\s*[* .]*\s*\((?P<se>{NUMBER})\)')


def read_etable(path):
    table = pd.read_csv(path, dtype=str, keep_default_na=False)
    if 'Unnamed: 1' in table:
        label_column = 'Unnamed: 1'
    else:
        candidates = [c for c in table if not re.fullmatch(r'reg\d+(?:\..*)?', c)]
        if not candidates:
            raise ValueError('The exported table must include its row labels.')
        label_column = candidates[-1]
    return table.set_index(label_column).drop(columns=['Unnamed: 0'], errors='ignore')


def coefficient_rows(table, column):
    if column not in table:
        raise ValueError(f'Model {column} is absent; available models: {list(table.columns)}')
    rows = []
    for label, value in table[column].items():
        match = COEFFICIENT.search(str(value).replace(',', ''))
        if match:
            rows.append((str(label), float(match['estimate']), float(match['se'])))
    if not rows:
        raise ValueError(f'No coefficient and standard-error pairs found in {column}.')
    return pd.DataFrame(rows, columns=['term', 'estimate', 'se'])


def plot_models(table, columns, title, destination):
    fig, axes = plt.subplots(1, len(columns), figsize=(3.6 * len(columns), 6), squeeze=False)
    for axis, column in zip(axes[0], columns):
        rows = coefficient_rows(table, column).iloc[::-1]
        y = np.arange(len(rows))
        axis.errorbar(rows['estimate'], y, xerr=1.96 * rows['se'], fmt='o', capsize=3)
        axis.set_yticks(y, rows['term'])
        axis.axvline(0, color='grey', linestyle='--')
        axis.set_title(column)
        axis.set_xlabel('Coefficient (95% normal interval)')
        axis.spines[['top', 'right']].set_visible(False)
    fig.suptitle(title)
    fig.tight_layout()
    fig.savefig(destination, bbox_inches='tight')


def plot_split(table, split_label, destination):
    if split_label not in table.index:
        raise ValueError(f'Missing split row: {split_label}')
    rows = []
    for column in table:
        if column.startswith('Unnamed:'):
            continue
        coefficients = coefficient_rows(table, column)
        row = coefficients[coefficients['term'] == 'switchScore']
        if not row.empty:
            rows.append((table.loc[split_label, column], row['estimate'].iloc[0], row['se'].iloc[0]))
    if not rows:
        raise ValueError('No switchScore estimates were exported for these subsamples.')
    values = pd.DataFrame(rows, columns=['sample', 'estimate', 'se'])
    values['sample'] = pd.to_numeric(values['sample'], errors='raise')
    values = values.sort_values('sample')
    fig, ax = plt.subplots(figsize=(12, 4))
    ax.errorbar(values['sample'].astype(str), values['estimate'], yerr=1.96 * values['se'], fmt='o', capsize=3)
    ax.axhline(0, color='grey', linestyle='--')
    ax.tick_params(axis='x', rotation=90)
    ax.set(xlabel=split_label, ylabel='Switch-score coefficient (95% normal interval)')
    fig.tight_layout()
    fig.savefig(destination, bbox_inches='tight')
