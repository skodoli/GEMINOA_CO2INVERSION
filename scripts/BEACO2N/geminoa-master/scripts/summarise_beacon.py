#!/usr/bin/env python3
import pandas as pd
from pathlib import Path
import multiprocessing as mp
import datetime

ONE_DAY = datetime.timedelta(days=1)

def get_timeperiod(filename):
    name = filename.name.replace('%20', ' ')
    try:
        days = set()
        print(filename)
        for i, line in enumerate(open(filename, 'r')):
            if i == 0:
                continue # skip header
            yyyy, mm, dd = line.split(',')[0].split(' ')[0].split('-')
            days.add(datetime.date(int(yyyy), int(mm), int(dd)))
        days = sorted(days)
        n_days = len(days)
        sequences = []
        seq = []
        for d1, d2 in zip(days, days[1:]):
            delta = d2 - d1
            # print(delta)
            if delta > ONE_DAY:
                seq.append(d1)
                sequences.append((seq[0], seq[-1] - seq[0]))
                seq = [d2]

        return ([name, days[0], days[-1], n_days], sequences)
    except:
        return ([name, None, None, None], None)

if __name__ == "__main__":
    with mp.Pool(mp.cpu_count()) as pool:
        outs = pool.map(get_timeperiod, Path('./data/beacon').glob('*.csv'))
        print(f"{outs = :}")
        # out = [x for x in out if x[1] is not None]
        # seqs = [x[0] for x in out if x[1] is not None]
        # out = pd.DataFrame(out, columns=['file', 'start', 'end', 'days'])
        # sorted_values = out.sort_values(by='days', ascending=False)
        # sorted_values.to_csv('data_timeperiod.csv', index=False)
        # print(sorted_values)
