""" Build USA-CHN input-output network with alliances """

import numpy as np
import pandas as pd

from settings import DATA_PATH, OUTPUT_PATH
from utilities import merge_df
from calibration.iotables.build_iotables_naics4 import cw_chn_sender


def amend_trade(four_digit=True):
    cpi_u = pd.read_csv(DATA_PATH / 'measurement' / 'accounting' / 'CPIAUCSL.csv')
    cpi_u.columns = ['fiscal_year', 'index']
    cpi_u['fiscal_year'] = cpi_u['fiscal_year'].map(lambda x: int(x[:4]))
    cpi_u['index'] = cpi_u['index'].map(lambda x: float(x) if x != '.' else np.nan)
    cpi_u['index'] = cpi_u.loc[cpi_u['fiscal_year'] == 2018, 'index'].tolist()[0] / cpi_u['index']

    df_list = []
    for year in range(2012, 2020):
        print(f"Processing year {year}...")
        df = pd.read_csv(DATA_PATH / 'motivation' / 'trade' / 'BACI_HS12_V202401b' / f'BACI_HS12_Y{year}_V202401b.csv')
        df = df[df['i'] != df['j']].copy()
        df.columns = ['year', 'exporter', 'importer', 'hscode', 'value', 'quantity']
        df['hscode'] = df['hscode'].map(lambda x: str(x).zfill(6))
        if four_digit:
            df['hscode'] = df['hscode'].map(lambda x: x[:4])
        df['value'] = df['value'] * 1000 * cpi_u.loc[cpi_u['fiscal_year'] == year, 'index'].tolist()[0]
        df_list.append(df)
    df = pd.concat(df_list, axis=0, ignore_index=True)
    df = df.groupby(['exporter', 'importer', 'hscode'])['value'].sum().reset_index()

    # add country codes
    country = pd.read_csv(DATA_PATH / 'motivation' / 'trade' / 'BACI_HS12_V202401b' / 'country_codes_V202401b.csv')
    country.loc[country['country_iso3'].isna(), 'country_iso3'] = (
        country.loc[country['country_iso3'].isna(), 'country_code'].map(str))
    country = country[['country_code', 'country_iso3']]
    country.columns = ['exporter', 'exporter_code']
    df = merge_df(df, country, on='exporter', how='left', keep_merge=False)
    country.columns = ['importer', 'importer_code']
    df = merge_df(df, country, on='importer', how='left', keep_merge=False)
    df = df[['exporter_code', 'importer_code', 'hscode', 'value']]

    # add rest of the world
    df_cty = pd.read_csv(DATA_PATH / 'motivation' / 'trade' / 'BACI_HS12_V202401b' / 'Countries_V202211_manual.csv')
    df_cty = df_cty.groupby('iso3')[['modern_block', 'country']].last().reset_index()
    df = merge_df(df, df_cty, left_on='exporter_code', right_on='iso3', how='left', keep_merge=True)
    df.loc[
        (df['modern_block'] == 'Western') & (df['exporter_code'] != 'USA') & (df['importer_code'] == 'CHN'),
        'exporter_code'
    ] = 'A_W'
    df.loc[
        (df['modern_block'] == 'Eastern') & (df['exporter_code'] != 'CHN') & (df['importer_code'] == 'USA'),
        'exporter_code'
    ] = 'A_E'
    df['exporter_code'] = df['exporter_code'].map(lambda x: 'ROW' if x not in ['CHN', 'USA', 'A_W', 'A_E'] else x)
    df['importer_code'] = df['importer_code'].map(lambda x: 'ROW' if x not in ['CHN', 'USA'] else x)
    df = df[df['exporter_code'] != df['importer_code']].copy()
    df = df[~((df['exporter_code'] == 'A_E') & (df['importer_code'] != 'USA'))].copy()
    df = df[~((df['exporter_code'] == 'A_W') & (df['importer_code'] != 'CHN'))].copy()
    df = df.groupby(['exporter_code', 'importer_code', 'hscode'])['value'].sum().reset_index()

    # take naics6 level
    if four_digit:
        hs4_naics = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs4_naics12_4digit.csv',
                                dtype={'hs4': 'str', 'naics12': 'str'})
        hs4_naics['hscode'] = hs4_naics['hs4']
        hs4_naics['naics12'] = hs4_naics['naics12'].map(lambda x: x[:4])
        hs4_naics = hs4_naics.groupby(['hscode', 'naics12'])['wt_mappings'].sum().reset_index()
    else:
        hs4_naics = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs4_naics12.csv',
                                dtype={'hscode': 'str', 'naics12': 'str'})
        hs4_naics.loc[hs4_naics['hscode'].map(lambda x: x.startswith('2710')), 'hscode'] = '271000'
    hs4_naics = hs4_naics.groupby(['hscode', 'naics12'])['wt_mappings'].first().reset_index()
    df = merge_df(df, hs4_naics, on='hscode', how='left', keep_merge=False)
    df['value'] = df['value'] * df['wt_mappings']
    df = df.groupby(['exporter_code', 'importer_code', 'hscode', 'naics12'])['value'].sum().reset_index()
    df.loc[df['exporter_code'].map(lambda x: x in ['A_W', 'A_E']), 'naics12'] = df.loc[
        df['exporter_code'].map(lambda x: x in ['A_W', 'A_E']), 'hscode']
    df = df.groupby(['exporter_code', 'importer_code', 'hscode', 'naics12'])['value'].sum().reset_index()

    # generate export file
    df_xp = df.copy()
    df_xp['industry_from'] = df_xp['exporter_code'] + '_DOM' + df_xp['naics12']
    df_xp['industry_to'] = df_xp['importer_code'] + '_IMP' + df_xp['hscode']
    if four_digit:
        df_xp.loc[df_xp['industry_from'].map(lambda x: x.startswith('ROW')), 'industry_from'] = 'ROW_LAB0000'
    else:
        df_xp.loc[df_xp['industry_from'].map(lambda x: x.startswith('ROW')), 'industry_from'] = 'ROW_LAB000000'
    df_xp = df_xp.groupby(['industry_to', 'industry_from'])['value'].sum().reset_index()
    df_xp['value_sum'] = df_xp.groupby('industry_to')['value'].transform(sum)
    df_xp['share_value'] = df_xp['value'] / df_xp['value_sum']
    if four_digit:
        df_xp.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_xp_4digit_alliance.csv', index=False)
    else:
        df_xp.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_xp_alliance.csv', index=False)


def amend_io_matrix(baseline=False):
    io_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4.csv')
    io_matrix = io_matrix[io_matrix['industry_to'].map(lambda x: x[4:7] != 'IMP')].copy()
    io_matrix = io_matrix[['industry_to', 'industry_from', 'share_value']]

    trade = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_xp_4digit_alliance.csv')
    trade['industry_from'] = trade['industry_from'].map(
        lambda x: 'USA_ALL' + x[7:] if x[:7] == 'A_W_DOM' else 'CHN_ALL' + x[7:] if x[:7] == 'A_E_DOM' else x)
    trade = trade[['industry_to', 'industry_from', 'share_value']]

    trade_all = trade[trade['industry_from'].map(lambda x: x[4:7] == 'ALL')].groupby(
        ['industry_from']).first().reset_index()[['industry_from']]
    trade_all.columns = ['industry_to']
    trade_all['industry_from'] = 'ROW_LAB0000'
    trade_all['share_value'] = 1.0

    io_matrix = pd.concat([io_matrix, trade, trade_all], ignore_index=True)
    if not baseline:
        io_matrix.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_alliances.csv', index=False)
    if baseline:
        io_matrix['industry_from'] = io_matrix['industry_from'].map(lambda x: x.replace('USA_ALL', 'CH1_ALL'))
        io_matrix['industry_from'] = io_matrix['industry_from'].map(lambda x: x.replace('CHN_ALL', 'USA_ALL'))
        io_matrix['industry_from'] = io_matrix['industry_from'].map(lambda x: x.replace('CH1_ALL', 'CHN_ALL'))
        io_matrix['industry_to'] = io_matrix['industry_to'].map(lambda x: x.replace('USA_ALL', 'CH1_ALL'))
        io_matrix['industry_to'] = io_matrix['industry_to'].map(lambda x: x.replace('CHN_ALL', 'USA_ALL'))
        io_matrix['industry_to'] = io_matrix['industry_to'].map(lambda x: x.replace('CH1_ALL', 'CHN_ALL'))
        io_matrix.to_csv(
            OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_alliances_baseline.csv', index=False)


def amend_final_demand(baseline=False):
    if not baseline:
        fd_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_alliances.csv')
    else:
        fd_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_alliances_baseline.csv')
    industry_list = list(fd_matrix['industry_to'].unique())
    fd_matrix = pd.DataFrame(industry_list, columns=['industry'])

    df = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4.csv')

    fd_matrix = merge_df(fd_matrix, df, how='left', on='industry', keep_merge=False)
    cols = [col for col in fd_matrix.columns if col.startswith('share_')]
    for col in cols:
        fd_matrix[col] = fd_matrix[col].fillna(0.0)
    if not baseline:
        fd_matrix.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_alliances.csv', index=False)
    if baseline:
        fd_matrix.to_csv(
            OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_alliances_baseline.csv', index=False)


def amend_attrib(baseline=False):
    if not baseline:
        io_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_alliances.csv')
    else:
        io_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_alliances_baseline.csv')
    industry_list = list(io_matrix['industry_to'].unique())
    attrib = pd.DataFrame(industry_list, columns=['industry'])
    attrib['country'] = attrib['industry'].map(lambda x: x[:3])
    attrib['type'] = attrib['industry'].map(lambda x: x[4:7])
    attrib['sector'] = attrib['industry'].map(lambda x: x[7:])

    attrib_old = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'attrib_naics4.csv')
    attrib_copy1 = attrib_old[attrib_old['type'] == 'IMP'].copy()
    attrib_copy1['industry'] = attrib_copy1['industry'].map(lambda x: 'CHN_ALL' + x[7:])
    attrib_copy1['country'] = 'CHN'
    attrib_copy1['type'] = 'ALL'
    attrib_copy2 = attrib_old[attrib_old['type'] == 'IMP'].copy()
    attrib_copy2['industry'] = attrib_copy1['industry'].map(lambda x: 'USA_ALL' + x[7:])
    attrib_copy2['country'] = 'USA'
    attrib_copy2['type'] = 'ALL'
    attrib_old = pd.concat([attrib_old, attrib_copy1, attrib_copy2], ignore_index=True)
    attrib_old = attrib_old.groupby('industry').first().reset_index()

    attrib = merge_df(attrib, attrib_old, how='left', on=['industry', 'country', 'type', 'sector'], keep_merge=False)
    if not baseline:
        attrib.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'attrib_naics4_alliances.csv', index=False)
    else:
        attrib.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'attrib_naics4_alliances_baseline.csv', index=False)


def amend_elast(baseline=False):
    if not baseline:
        elast = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_alliances.csv')
    else:
        elast = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_alliances_baseline.csv')
    elast = elast[['industry']]
    elast['country'] = elast['industry'].map(lambda x: x[:3])
    elast['type'] = elast['industry'].map(lambda x: x[4:7])
    elast['sector'] = elast['industry'].map(lambda x: x[7:])
    elast['sigma'] = 1.0
    elast.loc[elast['type'] == 'IMP', 'sigma'] = 2.53
    elast.loc[elast['type'] == 'MPT', 'sigma'] = 1.53
    elast.loc[elast['type'] == 'AGG', 'sigma'] = 1.19
    if not baseline:
        elast.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'elast_naics4_alliances.csv', index=False)
    else:
        elast.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'elast_naics4_alliances_baseline.csv', index=False)


def amend_het_elast(baseline=False):
    if not baseline:
        elast = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_alliances.csv')
    else:
        elast = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_alliances_baseline.csv')
    # elast = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_alliances.csv')
    elast = elast[['industry']]
    elast['industry'] = elast['industry'].map(lambda x: x.replace('USA_ALL', 'ROW_ALL'))
    elast['industry'] = elast['industry'].map(lambda x: x.replace('CHN_ALL', 'ROW_ALL'))
    elast['country'] = elast['industry'].map(lambda x: x[:3])
    elast['type'] = elast['industry'].map(lambda x: x[4:7])
    elast['sector'] = elast['industry'].map(lambda x: x[7:])
    elast['sigma'] = 1.0
    elast.loc[elast['type'] == 'IMP', 'sigma'] = 2.53
    elast.loc[elast['type'] == 'MPT', 'sigma'] = 1.53
    elast.loc[elast['type'] == 'AGG', 'sigma'] = 1.19

    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'hs12_elast_4digit.csv', dtype={'hs4': 'str'})
    df['sigma'] = df['sigma_sdb']
    df = df[['hs4', 'sigma']]
    df.columns = ['sector', 'sigma_wgt']

    elast = merge_df(elast, df, left_on='sector', right_on='sector', how='left', keep_merge=False)
    elast.loc[~elast['sigma_wgt'].isna() & (elast['type'] == 'IMP'), 'sigma'] = \
        elast.loc[~elast['sigma_wgt'].isna() & (elast['type'] == 'IMP'), 'sigma_wgt']
    elast = elast[['industry', 'country', 'type', 'sector', 'sigma']]
    if not baseline:
        elast.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'elast_hetero_naics4_alliances.csv', index=False)
    else:
        elast.to_csv(
            OUTPUT_PATH / 'calibration' / 'iotables' / 'elast_hetero_naics4_alliances_baseline.csv', index=False)


def main():
    # amend_trade(four_digit=True)
    amend_io_matrix()
    amend_io_matrix(baseline=True)
    amend_final_demand()
    amend_final_demand(baseline=True)
    amend_attrib()
    amend_attrib(baseline=True)
    amend_elast()
    amend_elast(baseline=True)
    amend_het_elast()
    amend_het_elast(baseline=True)


if __name__ == '__main__':
    main()

