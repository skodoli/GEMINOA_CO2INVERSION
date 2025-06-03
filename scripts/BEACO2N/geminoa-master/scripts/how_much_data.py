#!/usr/bin/env python3
from pathlib import Path

import pandas as pd
import dateutil.parser as dp

files = list(Path('data/beacon').glob('*.csv'))

failed = []
out = pd.DataFrame(columns=['rows', 'filename', 'start', 'end', 'days'])
for f in files:
    if 'summary.csv' in str(f):
        continue
    try:
        df = pd.read_csv(f)
        start = df['local_timestamp'].iloc[0]
        end = df['local_timestamp'].iloc[-1]
        days = (dp.parse(end) - dp.parse(start)).days
        newdf = pd.DataFrame({'rows': [len(df)], 'filename': [f], 'start': [start], 'end': [end], 'days': [days]})
        out = pd.concat([out, newdf])
    except Exception as E:
        print(f, E)
        failed.append(f)

out.to_csv('beacon-data/summary.csv', index=False)
for f in failed:
    print(f'Failed to read {f}')
