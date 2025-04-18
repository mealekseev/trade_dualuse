""" Assembles csvs into one file """

import sys
import pandas as pd
from pathlib import Path

from settings import OUTPUT_PATH, EXTERNAL_PATH


def import_csvs():
    for yr in range(1966, 2004):
        path = EXTERNAL_PATH / 'output' / 'motivation' / 'history' / f'{yr}'
        files = sorted(path.glob('*.csv'))
        df_list = []
        for i, f in enumerate(files):
            print(f)
            df = pd.read_csv(f)
            df['field_title'] = df['field_title'].map(
                lambda x: x.lower().replace(' ', '_').replace('-', '').replace('.', ''))
            df['field_title'] = df['field_title'].map(
                lambda x: 'dollar_value' if x.startswith('dollar_value') else
                'fsc_desc' if x.startswith('fsc_') else
                'sys_equ_desc' if x.startswith('sys_[system]') else
                'est_completion_date' if x.startswith('est[imated]') else
                x
            )
            df.loc[df['field_title'] == 'dollar_value', 'value'] = df.loc[
                df['field_title'] == 'dollar_value', 'meaning']
            df.set_index('field_title', inplace=True)
            df = df[['value']]
            df = df.T
            df['id'] = int(str(f).split('/')[-1].split('_')[1].split('.')[0])
            df_list.append(df)
        df = pd.concat(df_list)
        df.sort_values('id', inplace=True)
        print(f"Year {yr}: {df.shape} shape")
        df.to_csv(OUTPUT_PATH / 'measurement' / 'history' / 'years' / f'{yr}.csv', index=False)


if __name__ == '__main__':
    import_csvs()

