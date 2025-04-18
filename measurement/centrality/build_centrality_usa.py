# Build USA centrality measures

import numpy as np
import pandas as pd

from settings import DATA_PATH, OUTPUT_PATH, output
from utilities import merge_df, divide_robust


def filter_io_tables(subcontracts=True):
    mil = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'mil.csv')
    M = mil['USA'].values[0]

    gdp = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'gdp.csv')
    C = gdp['USA'].values[0]

    fd_consumer = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'fd_consumer_usa.csv')
    industry_list = fd_consumer['industry'].to_list()
    s_C = fd_consumer['share_consumer_data'].values

    fd_military = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'fd_military_usa.csv')
    assert fd_military['industry'].to_list() == industry_list
    if subcontracts:
        s_M = fd_military['share_military_combined_subcontracts'].values
    else:
        s_M = fd_military['share_military_combined'].values

    io_matrix = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'io_usa.csv')
    io_matrix['industry_to'] = io_matrix['industry_to'].map(lambda x: x.replace('USA_DOM', 'USA_AGG'))
    io_matrix['share_value_data'] = io_matrix['share_value_data']
    io_matrix = io_matrix[['industry_to', 'industry_from', 'share_value_data']]
    io_matrix = io_matrix.pivot(index='industry_to', columns='industry_from', values='share_value_data')
    assert io_matrix.index.to_list() == industry_list
    assert io_matrix.columns.to_list() == industry_list
    Omega = io_matrix.values
    return s_C, s_M, C, M, Omega


def build_centrality(subcontracts=True):
    s_C, s_M, C, M, Omega = filter_io_tables(subcontracts=subcontracts)

    # expenditures and sales shares
    Psi = np.linalg.inv(np.eye(s_C.shape[0]) - Omega)
    cent_M = np.einsum('kl,k->l', Psi, s_M)
    cent_C = np.einsum('kl,k->l', Psi, s_C)
    cent_M_norm = cent_M / sum(cent_M)
    cent_C_norm = cent_C / sum(cent_C)
    E = s_M * M + s_C * C
    X = Psi.T @ E

    # sales shares
    S_M = s_M * M / (s_M * M + s_C * C)
    S_C = s_C * C / (s_M * M + s_C * C)
    S_M_norm = (s_M / sum(cent_M)) / divide_robust(s_M / sum(cent_M) + s_C / sum(cent_C))
    S_C_norm = (s_C / sum(cent_C)) / divide_robust(s_M / sum(cent_M) + s_C / sum(cent_C))

    # adjusted sales
    C_M = cent_M * M / (cent_M * M + cent_C * C)
    C_C = cent_C * C / (cent_M * M + cent_C * C)
    C_M_norm = cent_M_norm / (cent_M_norm + cent_C_norm)
    C_C_norm = cent_C_norm / (cent_M_norm + cent_C_norm)

    # add attributes
    attrib = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'attrib.csv')
    attrib['M'] = M
    attrib['C'] = C
    attrib['s_M'] = s_M
    attrib['s_C'] = s_C
    attrib['cent_M'] = cent_M
    attrib['cent_C'] = cent_C
    attrib['cent_M_norm'] = cent_M_norm
    attrib['cent_C_norm'] = cent_C_norm
    attrib['X'] = X
    attrib['E'] = s_M * M + s_C * C

    attrib['S_M'] = S_M
    attrib['S_C'] = S_C
    attrib['C_M'] = C_M
    attrib['C_C'] = C_C
    attrib['S_M_norm'] = S_M_norm
    attrib['S_C_norm'] = S_C_norm
    attrib['C_M_norm'] = C_M_norm
    attrib['C_C_norm'] = C_C_norm
    return attrib


def build_centrality_variants():
    attrib1 = build_centrality(subcontracts=False)
    attrib2 = build_centrality(subcontracts=True)
    for col in attrib2.columns[6:]:
        attrib2.rename({col: col + '_subctr'}, axis=1, inplace=True)
    attrib2.drop(attrib1.columns[1:6], axis=1, inplace=True)
    attrib = merge_df(attrib1, attrib2, how='inner', on='industry', keep_merge=False)
    attrib.to_csv(OUTPUT_PATH / 'measurement' / 'centrality' / 'centrality_naics12.csv', index=False)


def add_hs_centrality():
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs4_naics12.csv', dtype={'hscode': 'str', 'naics12': 'str'})
    cw = cw.groupby(['hscode', 'naics12']).first().reset_index()
    cw.loc[cw['hscode'].map(lambda x: x.startswith('2710')), 'hscode'] = '271000'
    elast = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'hs12_elast.csv', dtype={'hs12': 'str'})
    elast['sigma'] = elast['sigma_sdb']
    cw = merge_df(cw, elast, how='right', left_on='hscode', right_on='hs12', keep_merge=False)
    cw['hscode'] = cw['hscode'].fillna('710820')
    cw['naics12'] = cw['naics12'].fillna('999990')

    attrib = pd.read_csv(OUTPUT_PATH / 'measurement' / 'centrality' / 'centrality_naics12.csv', dtype={'sector': 'str'})
    df = merge_df(cw, attrib, how='left', left_on='naics12', right_on='sector', keep_merge=False)
    df = df[df['sector'] != '999990'].copy()
    cols = [
        'S_M', 'S_C', 'S_M_norm', 'S_C_norm', 'C_M', 'C_C', 'C_M_norm', 'C_C_norm',
        's_M', 's_C', 'cent_M', 'cent_C', 'cent_M_norm', 'cent_C_norm', 'X', 'E', 'M', 'C'
    ]
    cols = cols + [col + '_subctr' for col in cols] + ['sigma', 'sigma_sdb', 'sigma_sbw', 'sigma_bw', 'sigma_cepii']
    for col in cols:
        df[col] = df[col] * df['wt_mappings'] / df.groupby('hs12')['wt_mappings'].transform('sum')
        df[col] = df.groupby('hs12')[col].transform('sum')
    df = df.groupby('hs12').first().reset_index()[['hs12', 'naics12', 'sector_name'] + cols]
    df['hs4'] = df['hs12'].map(lambda x: x[:4])
    df['hs2'] = df['hs12'].map(lambda x: x[:2])

    # sales shares
    for suffix in ['', '_subctr']:
        for sigma in ['sigma', 'sigma_sdb', 'sigma_sbw', 'sigma_bw', 'sigma_cepii']:
            df[f'C_M_{sigma}{suffix}'] = df[f'C_M{suffix}'] / df[sigma]

    # add dualuse outcomes
    hscode = pd.read_csv(
        OUTPUT_PATH / 'motivation' / 'dualuse_masterfile_hs4.csv',
        dtype={'sector': 'str', 'dualuse': 'str', 'dualuse_categ': 'str', 'dualuse_subct': 'str'}
    )
    df = merge_df(df, hscode, how='inner', left_on='hs12', right_on='sector', keep_merge=True)

    # rank
    cols = [
        col for col in df.columns if col.startswith('S_M') or col.startswith('C_M')
                                     or col.startswith('cent_') or col.startswith('s_')
    ]
    for col in cols:
        df[f'rank_{col}'] = (df[col].rank() - 1) / df.shape[0]
        for b in [5, 50]:
            df[f'pbin{b}_{col}'] = ((df[col].rank() - 1) / df.shape[0] * b).map(lambda x: int(x) + 1)
            df[f'rbin{b}_{col}'] = pd.cut(df[col], bins=np.linspace(0, 1.0, b + 1), labels=False)
            df = df.copy()
    df.sort_values('C_M_sigma', inplace=True, ascending=False)
    df = df[['hs4', 'hscode_name'] + [col for col in df.columns if col not in ['hs4', 'hscode_name']]]
    
    cols = [col for col in df.columns if col.startswith('dualuse_2018') or col == 'military']
    df['dualuse_outcome'] = df[cols + ['military']].apply(lambda row: max(row), axis=1)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'centrality' / 'centrality_hs12.csv', index=False)


def add_hs_centrality_4digit():
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs4_naics12_4digit.csv', dtype={'hs4': 'str', 'naics12': 'str'})
    elast = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'hs12_elast_4digit.csv', dtype={'hs4': 'str'})
    elast['sigma'] = elast['sigma_sdb']
    cw = merge_df(cw, elast, how='inner', left_on='hs4', right_on='hs4', keep_merge=False)

    attrib = pd.read_csv(OUTPUT_PATH / 'measurement' / 'centrality' / 'centrality_naics12.csv', dtype={'sector': 'str'})
    df = merge_df(cw, attrib, how='left', left_on='naics12', right_on='sector', keep_merge=False)
    df = df[df['sector'] != '999990'].copy()
    cols = [
        'S_M', 'S_C', 'S_M_norm', 'S_C_norm', 'C_M', 'C_C', 'C_M_norm', 'C_C_norm',
        's_M', 's_C', 'cent_M', 'cent_C', 'cent_M_norm', 'cent_C_norm', 'X', 'E', 'M', 'C'
    ]
    cols = cols + [col + '_subctr' for col in cols] + ['sigma', 'sigma_sdb', 'sigma_sbw', 'sigma_bw', 'sigma_cepii']
    for col in cols:
        df[col] = df[col] * df['wt_mappings'] / df.groupby('hs4')['wt_mappings'].transform('sum')
        df[col] = df.groupby('hs4')[col].transform('sum')
    df = df.groupby('hs4').first().reset_index()[['hs4', 'naics12', 'sector_name'] + cols]
    df['hs2'] = df['hs4'].map(lambda x: x[:2])

    # sales shares
    for suffix in ['', '_subctr']:
        for sigma in ['sigma', 'sigma_sdb', 'sigma_sbw', 'sigma_bw', 'sigma_cepii']:
            df[f'C_M_{sigma}{suffix}'] = df[f'C_M{suffix}'] / df[sigma]

    # add dualuse outcomes
    hscode = pd.read_csv(
        OUTPUT_PATH / 'motivation' / 'dualuse_masterfile_hs4_4digit.csv',
        dtype={'sector': 'str', 'dualuse': 'str', 'dualuse_categ': 'str', 'dualuse_subct': 'str'}
    )
    df = merge_df(df, hscode, how='inner', left_on='hs4', right_on='sector', keep_merge=True)

    # rank
    cols = [col for col in df.columns if col.startswith('S_M') or col.startswith('C_M')]
    for col in cols:
        df[f'rank_{col}'] = (df[col].rank() - 1) / df.shape[0]
        for b in [5, 50]:
            df[f'pbin{b}_{col}'] = ((df[col].rank() - 1) / df.shape[0] * b).map(lambda x: int(x) + 1)
            df[f'rbin{b}_{col}'] = pd.cut(df[col], bins=np.linspace(0, 1.0, b + 1), labels=False)
            df = df.copy()
    df.sort_values('C_M_sigma', inplace=True, ascending=False)
    df = df[['hs4', 'hscode_name'] + [col for col in df.columns if col not in ['hs4', 'hscode_name']]]
    cols = [col for col in df.columns if col.startswith('dualuse_2018') or col == 'military']
    df['dualuse_outcome'] = df[cols + ['military']].apply(lambda row: max(row), axis=1)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'centrality' / 'centrality_hs12_4digit.csv', index=False)


def main():
    build_centrality_variants()
    add_hs_centrality()
    add_hs_centrality_4digit()


if __name__ == '__main__':
    main()

