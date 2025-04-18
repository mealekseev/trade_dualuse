""" Build trade data for the input-output tables """

import numpy as np
import pandas as pd

from settings import DATA_PATH, OUTPUT_PATH
from utilities import merge_df


def construct_trade(four_digit=True):
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
    df['exporter_code'] = df['exporter_code'].map(lambda x: 'ROW' if x not in ['CHN', 'USA'] else x)
    df['importer_code'] = df['importer_code'].map(lambda x: 'ROW' if x not in ['CHN', 'USA'] else x)
    df = df[df['exporter_code'] != df['importer_code']].copy()
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
        df_xp.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_xp_4digit.csv', index=False)
    else:
        df_xp.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_xp.csv', index=False)

    # generate import file
    df_mp = df.copy()
    df_mp['industry_from'] = df_mp['importer_code'] + '_IMP' + df_mp['hscode']
    df_mp['industry_to'] = df_mp['importer_code'] + '_MPT' + df_mp['naics12']
    df_mp = df_mp.groupby(['industry_to', 'industry_from'])['value'].sum().reset_index()
    df_mp['value_sum'] = df_mp.groupby('industry_to')['value'].transform(sum)
    df_mp['share_value'] = df_mp['value'] / df_mp['value_sum']
    if four_digit:
        df_mp.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_mp_4digit.csv', index=False)
    else:
        df_mp.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_mp.csv', index=False)


def main():
    construct_trade()
    construct_trade(four_digit=False)


if __name__ == '__main__':
    main()

