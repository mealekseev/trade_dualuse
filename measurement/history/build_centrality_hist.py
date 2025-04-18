""" Assembles csvs into one file """

import numpy as np
import pandas as pd
from pathlib import Path

from settings import DATA_PATH, OUTPUT_PATH, EXTERNAL_PATH
from utilities import merge_df


def build_naics_historic_centrality(w=10):
    cw = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'psc_naics' / 'DLA NAICS to FSC or PSC Cross Reference.xlsx', dtype=str
    )
    cw = cw[~cw['FSC'].isna() & ~cw['NAICS Code'].isna()]
    cw = cw[['NAICS Code', 'NAICS Description', 'FSC']]
    cw = cw[cw['NAICS Code'] != 'NONE'].copy()
    cw.columns = ['naics12', 'naics_desc', 'fsc']
    cw['naics12'] = cw['naics12'].map(lambda x: '314120' if x == '314129' else x)
    cw['wgt'] = 1 / cw.groupby('fsc')['naics12'].transform('count')

    df_list = []
    yr_list = list(range(1966, 2004))
    for yr in yr_list:
        print(yr)
        df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'history' / 'years' / f'{yr}.csv')
        if yr < 1984:
            df['dollar_value'] = df['dollar_value'].map(lambda x: int(x) if x != 'Invalid Number' else np.nan)
            df = df.groupby('federal_supply_class')['dollar_value'].sum().reset_index()
            df['year'] = yr
            df_list.append(df)
        else:
            fsc_col = 'federal_supply_classification_or_service_code'
            dollar_col = 'total_dollars_(000)'
            if yr == 1991:
                dollar_col = 'total_dollars'
            if yr >= 1992 and yr < 2001:
                dollar_col = 'total_dollars_(whole_dollars)'
                fsc_col = 'fsc_desc'
            if yr >= 2001:
                dollar_col = 'total_dollars_(whole_dollars)'
                fsc_col = 'federal_supply_class_(fsc)_or_service_code'
            if yr < 2001:
                df[dollar_col] = df[dollar_col].map(lambda x:
                    int(x[:-1] + '0') if x[-1] == '{'
                    else int(x[:-1] + '1') if x[-1] == 'A'
                    else int(x[:-1] + '2') if x[-1] == 'B'
                    else int(x[:-1] + '3') if x[-1] == 'C'
                    else int(x[:-1] + '4') if x[-1] == 'D'
                    else int(x[:-1] + '5') if x[-1] == 'E'
                    else int(x[:-1] + '6') if x[-1] == 'F'
                    else int(x[:-1] + '7') if x[-1] == 'G'
                    else int(x[:-1] + '8') if x[-1] == 'H'
                    else int(x[:-1] + '9') if x[-1] == 'I'
                    else -int(x[:-1] + '0') if x[-1] == '}'
                    else -int(x[:-1] + '1') if x[-1] == 'J'
                    else -int(x[:-1] + '2') if x[-1] == 'K'
                    else -int(x[:-1] + '3') if x[-1] == 'L'
                    else -int(x[:-1] + '4') if x[-1] == 'M'
                    else -int(x[:-1] + '5') if x[-1] == 'N'
                    else -int(x[:-1] + '6') if x[-1] == 'O'
                    else -int(x[:-1] + '7') if x[-1] == 'P'
                    else -int(x[:-1] + '8') if x[-1] == 'Q'
                    else -int(x[:-1] + '9') if x[-1] == 'R'
                    else int(x)
                )
            else:
                df[dollar_col] = df[dollar_col].map(lambda x:
                    -int(x[:-1] + '0') if x[-1] == 'p'
                    else -int(x[:-1] + '1') if x[-1] == 'q'
                    else -int(x[:-1] + '2') if x[-1] == 'r'
                    else -int(x[:-1] + '3') if x[-1] == 's'
                    else -int(x[:-1] + '4') if x[-1] == 't'
                    else -int(x[:-1] + '5') if x[-1] == 'u'
                    else -int(x[:-1] + '6') if x[-1] == 'v'
                    else -int(x[:-1] + '7') if x[-1] == 'w'
                    else -int(x[:-1] + '8') if x[-1] == 'x'
                    else -int(x[:-1] + '9') if x[-1] == 'y'
                    else int(x)
                )
            df = df.groupby(fsc_col)[dollar_col].sum().reset_index()
            df.columns = ['federal_supply_class', 'dollar_value']
            df['year'] = yr
            df_list.append(df)

    df = pd.concat(df_list)
    df = df.groupby(['year', 'federal_supply_class'])['dollar_value'].sum().reset_index()
    df = df[df['federal_supply_class'].map(lambda x: not x[0].isalpha())].copy()
    df['dollar_value_sum'] = df.groupby('year')['dollar_value'].transform('sum')
    df['share_value'] = df['dollar_value'] / df['dollar_value_sum']
    df = df[['year', 'federal_supply_class', 'share_value']]

    mil = merge_df(df, cw, left_on='federal_supply_class', right_on='fsc', how='inner')
    mil['share_value'] = mil['share_value'] * mil['wgt']
    mil = mil.groupby(['year', 'naics12'])['share_value'].sum().reset_index()
    mil.columns = ['year', 'naics12', 's_M']

    nber = pd.read_csv(DATA_PATH / 'measurement' / 'history' / 'nberces5818v1_n2012.csv', dtype={'naics': str})
    nber['vadd'] = nber['vadd'].fillna(0.0)
    nber['vadd_sum'] = nber.groupby('year')['vadd'].transform('sum')
    nber['vadd_share'] = nber['vadd'] / nber['vadd_sum']
    nber = nber[['year', 'naics', 'vadd_share', 'vadd_sum']]
    nber.columns = ['year', 'naics12', 's_C', 'C']
    nber = nber[(nber['year'] >= 1966) & (nber['year'] < 2004)].copy()

    naics_names = pd.read_excel(DATA_PATH / 'crosswalks' / 'census' / '6-digit_2012_Codes.xls', skiprows=1, dtype=str)
    naics_names.columns = ['naics12', 'naics_desc']
    panel = pd.DataFrame(
        [(x, y) for x in naics_names['naics12'].tolist() for y in yr_list], columns=['naics12', 'year'])
    panel = merge_df(panel, naics_names, on='naics12', how='left', keep_merge=False)

    cent = merge_df(mil, nber, on=['year', 'naics12'], how='outer', keep_merge=False)
    cent = merge_df(panel, cent, on=['year', 'naics12'], how='left', keep_merge=False)
    cent['s_M'] = cent['s_M'].fillna(0.0)
    cent['year'] = cent['year'].map(int)
    cent.sort_values(['naics12', 'year'], inplace=True)
    cent['s_M_norm'] = cent['s_M'] / cent.groupby('year')['s_M'].transform('sum')
    cent['s_M_trim'] = cent['s_M'] * (~cent['s_C'].isna())
    cent['s_M_trim'] = cent['s_M_trim'] / cent.groupby('year')['s_M_trim'].transform('sum')
    cent.loc[cent['s_C'].isna(), 's_M_trim'] = np.nan
    cent['S_M_trim'] = cent['s_M_trim'] / (cent['s_M_trim'] + cent['s_C'])
    cent['S_M_norm'] = cent['s_M_norm'] / (cent['s_M_norm'] + cent['s_C'])
    cent['S_M'] = cent['s_M'] / (cent['s_M'] + cent['s_C'])

    cols = ['s_M', 's_C', 's_M_norm', 's_M_trim', 'S_M_trim', 'S_M_norm', 'S_M']
    for col in cols:
        cent[f'{col}_ma'] = cent.groupby('naics12')[col].transform(lambda x: x.rolling(window=w, min_periods=1).mean())
    cent.to_csv(OUTPUT_PATH / 'measurement' / 'history' / 'centrality_naics12_hist.csv', index=False)


def build_sitc_historic_centrality():
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'sitc75_naics12.csv', dtype={'sitc75': 'str', 'naics12': 'str'})
    cw = cw.groupby(['sitc75', 'naics12']).first().reset_index()

    attrib = pd.read_csv(OUTPUT_PATH / 'measurement' / 'history' / 'centrality_naics12_hist.csv',
                         dtype={'naics12': 'str', 'year': 'int'})
    df = merge_df(cw, attrib, how='inner', left_on='naics12', right_on='naics12', keep_merge=False)

    cols = [
        's_M', 's_C', 's_M_norm', 's_M_trim', 'S_M', 'S_M_norm', 'S_M_trim',
        's_M_ma', 's_C_ma', 's_M_norm_ma', 's_M_trim_ma', 'S_M_ma', 'S_M_norm_ma', 'S_M_trim_ma'
    ]
    for col in cols:
        df[col] = df[col] * df['wgt'] / df.groupby(['sitc75', 'year'])['wgt'].transform('sum')
        df[col] = df.groupby(['sitc75', 'year'])[col].transform('sum')
        if col not in ['s_M', 's_M_ma', 's_M_norm_ma', 's_M_trim_ma']:
            df.loc[df[col] == 0, col] = np.nan
    df = df.groupby(['sitc75', 'year']).first().reset_index()[['sitc75', 'year', 'naics12', 'naics_desc'] + cols]

    s2 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'S2_codes.csv', encoding='latin2', dtype={'s2_code': str})
    s2 = s2[s2['sector'].map(lambda x: len(x) == 4)].copy()
    s2 = s2[['sector', 'sitc_name']]

    df = merge_df(s2, df, left_on='sector', right_on='sitc75', how='left', keep_merge=False)
    df.drop(columns=['sitc75'], inplace=True)
    df.rename({'sector': 'sitc75'}, axis=1, inplace=True)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'history' / 'centrality_sitc75_hist.csv', index=False)


def build_trade_share(w=10):
    path = EXTERNAL_PATH / 'output' / 'motivation' / 'gravity' / 'sitc2'
    path = Path('/Volumes/Seagate/trade_defense/gravity/sitc')
    files = sorted(path.glob('*.csv'))

    f = files[0]
    for f in files:
        print(f)
        df = pd.read_csv(f, usecols=['sitc', 'exporter_code', 'importer_code', 'year', 'value'], dtype={'sitc': str})
        df_list = []
        for j in range(1, w + 1):
            if j < w:
                df_yr = df[df['year'] <= (1962 + j)].copy()
            else:
                df_yr = df.copy()
            df_yr['value'] = df_yr.groupby(['exporter_code', 'importer_code'])['value'].transform(
                lambda x: x.rolling(window=j, min_periods=j).sum())
            df_yr = df_yr.groupby(['sitc', 'year'])[['value']].sum().reset_index()
            df_yr['value'] = df_yr['value'].fillna(0.0) / j
            if j < w:
                df_yr = df_yr[df_yr['year'] == (1962 + j)].copy()
            else:
                df_yr = df_yr[df_yr['year'] >= (1962 + j)].copy()
            df_list.append(df_yr)
        df_res = pd.concat(df_list)
        df_res.to_csv(
            OUTPUT_PATH / 'measurement' / 'history' / 'trade' /
                f"{f.stem.replace('gravity', 'sum').replace('_sitc_', '_sitc2_')}.csv",
            index=False
        )


def build_masterfile():
    path = OUTPUT_PATH / 'measurement' / 'history' / 'trade'
    files = sorted(path.glob('*.csv'))

    f = files[0]
    df_list = []
    for f in files:
        print(f)
        df = pd.read_csv(f, dtype={'sitc': str})
        df_list.append(df)
    df = pd.concat(df_list)
    df.columns = ['sitc75', 'year', 'value']
    df['share_value'] = df['value'] / df.groupby('year')['value'].transform('sum')

    cent = pd.read_csv(
        OUTPUT_PATH / 'measurement' / 'history' / 'centrality_sitc75_hist.csv', dtype={'sitc75': str, 'naics12': str}
    )
    df = merge_df(df, cent, on=['sitc75', 'year'], how='inner', keep_merge=False)
    df['share_value_norm'] = df['value'] / df.groupby('year')['value'].transform('sum')
    df.to_csv(OUTPUT_PATH / 'measurement' / 'history' / 'history_masterfile.csv', index=False)


if __name__ == '__main__':
    build_naics_historic_centrality()
    build_sitc_historic_centrality()
    build_trade_share()
    build_masterfile()

