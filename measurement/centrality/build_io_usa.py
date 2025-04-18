""" Build USA closed-economy centrality """

import numpy as np
import pandas as pd
import itertools

from settings import DATA_PATH, STATS_PATH, OUTPUT_PATH, output
from utilities import merge_df


def cw_usa_naics(four_digit=True, include_trade=True):
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    if four_digit:
        naics12_list = naics12_list['naics12'].map(lambda x: x[:4]).unique().tolist()
    else:
        naics12_list = naics12_list['naics12'].unique().tolist()
    if include_trade:
        io_list = (
            [['USA_AGG' + i, 'USA_DOM' + i] for i in naics12_list] +
            [['USA_AGG' + i, 'USA_MPT' + i] for i in naics12_list]
        )
    else:
        io_list = [['USA_AGG' + i, 'USA_DOM' + i] for i in naics12_list]
    io_matrix = pd.DataFrame(io_list, columns=['industry_to', 'industry_from'])

    # build naics12
    naics_usa = pd.read_csv(DATA_PATH / 'measurement' / 'accounting' / 'susb' / 'us_state_6digitnaics_2017.txt', encoding='latin1')
    naics_usa.columns = [col.lower() for col in naics_usa.columns]
    naics_usa = naics_usa[naics_usa['state'] == 0]
    naics_usa = naics_usa[naics_usa['entrsize'] == 1]
    naics_usa = naics_usa[naics_usa['naics'].map(lambda x: len(x) == 6)]
    naics_usa['rcpt'] = naics_usa['rcpt'] * 1000
    naics_usa = naics_usa[['naics', 'rcpt']]
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics17_naics12.csv', dtype=str)
    naics_usa = merge_df(
        naics_usa, cw, left_on='naics', right_on='naics17', how='left', validate='1:m', keep_merge=False)
    naics_usa['wt_mappings'] = naics_usa['wt_mappings'].map(float)
    naics_usa['rcpt'] = naics_usa['rcpt'] * naics_usa['wt_mappings']
    naics_usa = naics_usa.groupby('naics12')['rcpt'].sum().reset_index()

    if four_digit:
        naics_usa['naics12'] = naics_usa['naics12'].map(lambda x: x[:4])
        naics_usa = naics_usa.groupby('naics12')['rcpt'].sum().reset_index()

    naics_usa['industry_to'] = 'USA_AGG' + naics_usa['naics12']
    naics_usa['industry_from'] = 'USA_DOM' + naics_usa['naics12']
    naics_usa.rename({'rcpt': 'value'}, axis=1, inplace=True)
    naics_usa = naics_usa[['industry_to', 'industry_from', 'value']]

    if not include_trade:
        df = naics_usa.copy()
    else:
        if four_digit:
            trade = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_mp_4digit.csv')
        else:
            trade = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_mp.csv')
        trade = trade[trade['industry_to'].map(lambda x: x.startswith('USA'))].copy()
        trade = trade.groupby('industry_to').first().reset_index()
        trade = trade[['industry_to', 'value_sum']]
        trade.columns = ['industry_from', 'value']
        if four_digit:
            trade['industry_to'] = 'USA_AGG' + trade['industry_from'].map(lambda x: x[7:])
        else:
            trade['industry_to'] = 'USA_AGG' + trade['industry_from'].map(lambda x: x[7:])
        trade = trade[['industry_to', 'industry_from', 'value']]
        df = pd.concat([naics_usa, trade], axis=0, ignore_index=True)

    io_matrix = merge_df(io_matrix, df, on=['industry_to', 'industry_from'], how='left', keep_merge=False)
    io_matrix['value'] = io_matrix['value'].fillna(0.0)
    io_matrix['value_sum'] = io_matrix.groupby('industry_to')['value'].transform('sum')
    io_matrix['value_sum'] = io_matrix['value_sum'].map(lambda x: 1 if x == 0 else x)
    io_matrix['share_value'] = io_matrix['value'] / io_matrix['value_sum']
    io_matrix['share_value_sum'] = io_matrix.groupby('industry_to')['share_value'].transform('sum')
    io_matrix.loc[
        (io_matrix['share_value_sum'] == 0.0) & (io_matrix['industry_from'].map(lambda x: x[4:7]) == 'DOM'),
        'share_value'
    ] = 1.0

    assert max(abs(io_matrix.groupby('industry_to')['share_value'].sum() - 1.0)) < 1e-14
    return io_matrix
    

def construct_expenditures():
    # gdp figures
    gdp = pd.read_csv(
        DATA_PATH / 'measurement' / 'accounting' / 'API_NY.GDP.MKTP.CD_DS2_en_csv_v2_5871885' /
            'API_NY.GDP.MKTP.CD_DS2_en_csv_v2_5871885.csv',
        skiprows=4
    )
    gdp = gdp[['Country Name', 'Country Code', '2018']]
    gdp = gdp[gdp['Country Code'].isin(['CHN', 'USA', 'WLD'])].copy()
    gdp = gdp[['Country Code', '2018']]
    gdp.columns = ['country', 'gdp']
    gdp.loc[gdp['country'] == 'WLD', 'gdp'] = (
            gdp.loc[gdp['country'] == 'WLD', 'gdp'].values[0] - gdp.loc[gdp['country'] != 'WLD', 'gdp'].sum())
    gdp.set_index('country', inplace=True)
    gdp['gdp'] = gdp['gdp'] / 1e9
    gdp = gdp.T
    gdp.columns = ['CHN', 'USA', 'ROW']
    gdp.to_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'gdp.csv', index=False)

    # military expenditures
    mil = pd.read_csv(
        DATA_PATH / 'measurement' / 'accounting' / 'API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2_5871672' /
            'API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2_5871672.csv',
        skiprows=4
    )
    mil = mil[['Country Name', 'Country Code', '2018']]
    mil = mil[mil['Country Code'].isin(['CHN', 'USA', 'WLD'])].copy()
    mil = mil[['Country Code', '2018']]
    mil.columns = ['country', 'gdp']
    mil['gdp'] = mil['gdp'] / 100
    mil.set_index('country', inplace=True)
    mil = mil.T
    gdp_array = np.copy(gdp.loc['gdp'].values)
    gdp_array[2] = gdp_array[0] + gdp_array[1] + gdp_array[2]
    mil_array = mil.loc['gdp'].values * gdp_array
    mil_array[2] = mil_array[2] - mil_array[1] - mil_array[0]
    mil.loc['gdp'] = mil_array
    mil.columns = ['CHN', 'USA', 'ROW']
    mil.to_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'mil.csv', index=False)


def get_military_fd(naics_combined=True, subcontracts=True):
    # select names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].unique().tolist()
    fd_matrix = pd.DataFrame(['USA_AGG' + i for i in naics12_list], columns=['industry'])

    # build shares
    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'usaspending_awards_naics_naics_combined.csv',
                     dtype={'naics': 'str', 'naics_combined': 'str'})
    if not naics_combined:
        df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'usaspending_awards_naics_naics.csv',
                         dtype={'naics': 'str', 'naics_combined': 'str'})
        df = df.rename({'naics': 'naics_combined', 'naics_wgt': 'naics_combined_wgt'}, axis=1)
        df['naics_combined'] = df['naics_combined'].fillna('------')
    df = df[(df['fiscal_year'] > 2010) & (df['fiscal_year'] < 2020)]
    if subcontracts:
        df['total_subaward_amount'] = df['total_subaward_amount'].fillna(0.0)
        df.loc[df['total_subaward_amount'] > df['award_amount'], 'total_subaward_amount'] = (
            df.loc[df['total_subaward_amount'] > df['award_amount'], 'award_amount'])
        df['award_amount'] = df['award_amount'] - df['total_subaward_amount']
    df['amount'] = df['award_amount'] * df['naics_combined_wgt']
    df = df.groupby(['fiscal_year', 'naics_combined'])['amount'].sum().reset_index()

    cpi_u = pd.read_csv(DATA_PATH / 'measurement' / 'accounting' / 'CPIAUCSL.csv')
    cpi_u.columns = ['fiscal_year', 'index']
    cpi_u['fiscal_year'] = cpi_u['fiscal_year'].map(lambda x: int(x[:4]))
    cpi_u['index'] = cpi_u['index'].map(lambda x: float(x) if x != '.' else np.nan)
    cpi_u['index'] = cpi_u.loc[cpi_u['fiscal_year'] == 2018, 'index'].tolist()[0] / cpi_u['index']

    df = merge_df(df, cpi_u, how='left', on='fiscal_year')
    df['amount'] = df['amount'] * df['index']
    df = df.groupby('naics_combined')['amount'].sum().reset_index()
    df_mil = df.copy()
    df_mil['share_military_usa'] = df_mil['amount'] / df_mil['amount'].sum()

    if subcontracts:
        df_sub = pd.read_csv(
            OUTPUT_PATH / 'measurement' / 'contracts' / 'usaspending_subawards_naics.csv', dtype={'duns_naics': 'str'}
        )
        df_sub = df_sub[(df_sub['fy'] > 2010) & (df_sub['fy'] < 2020)].copy()

        df_sub_present = df_sub[~df_sub['duns_naics'].isna()].copy()
        df_sub_present['subaward_amount'] = df_sub_present['subaward_amount'] * df_sub_present['duns_naics_wgt']
        df_sub_present = df_sub_present.groupby(['fy', 'duns_naics'])['subaward_amount'].sum().reset_index()
        df_sub_present = merge_df(df_sub_present, cpi_u, how='left', left_on='fy', right_on='fiscal_year')
        df_sub_present['subaward_amount'] = df_sub_present['subaward_amount'] * df_sub_present['index']
        df_sub_present = df_sub_present[['fiscal_year', 'duns_naics', 'subaward_amount']]
        df_sub_present.columns = ['fiscal_year', 'naics_combined', 'amount']
        df_sub_present = df_sub_present.groupby(['naics_combined'])['amount'].sum().reset_index()
        df_sub_present.columns = ['naics_combined', 'amount']

        df_sub_absent = df_sub[df_sub['duns_naics'].isna()][['fy', 'subaward_amount', 'award_id']].copy()
        df_awards = pd.read_csv(
            OUTPUT_PATH / 'measurement' / 'contracts' / f'usaspending_awards_naics_naics_combined.csv',
            dtype={'naics_combined': 'str'}, usecols=['award_id', 'naics_combined']
        )
        df_sub_absent = merge_df(df_sub_absent, df_awards, on='award_id', how='inner', keep_merge=False)
        df_sub_absent = df_sub_absent.groupby(['fy', 'naics_combined'])['subaward_amount'].sum().reset_index()
        df_sub_absent = merge_df(df_sub_absent, cpi_u, how='left', left_on='fy', right_on='fiscal_year')
        df_sub_absent['subaward_amount'] = df_sub_absent['subaward_amount'] * df_sub_absent['index']
        df_sub_absent = df_sub_absent[['fiscal_year', 'naics_combined', 'subaward_amount']]
        df_sub_absent = df_sub_absent.groupby(['naics_combined'])['subaward_amount'].sum().reset_index()
        df_sub_absent.columns = ['naics_combined', 'amount']

        df_sub = pd.concat([df_sub_present, df_sub_absent], axis=0, ignore_index=True)
        df_sub = df_sub.groupby(['naics_combined'])['amount'].sum().reset_index()
        df_sub.columns = ['naics_combined', 'amount']

        df = pd.concat([df, df_sub], axis=0, ignore_index=True)
        df = df.groupby('naics_combined')['amount'].sum().reset_index()
        df['share_military_usa'] = df['amount'] / df['amount'].sum()

        # correlation check
        df_joint = merge_df(df_mil, df, on='naics_combined', how='inner', keep_merge=False)
        corr_num = df_joint['share_military_usa_x'].corr(df_joint['share_military_usa_y'])
        print(f"Correlation: {corr_num}")
    else:
        df['share_military_usa'] = df['amount'] / df['amount'].sum()

    df['share_military_usa'] = df['amount'] / df['amount'].sum()
    df = df[['naics_combined', 'share_military_usa']]
    df['industry'] = 'USA_AGG' + df['naics_combined']
    df = df[['industry', 'share_military_usa']]

    fd_matrix = merge_df(fd_matrix, df, on='industry', how='left', keep_merge=False)
    fd_matrix['share_military_usa'] = fd_matrix['share_military_usa'].fillna(0.0)
    fd_matrix['share_military_usa'] = fd_matrix['share_military_usa'] / fd_matrix['share_military_usa'].sum()
    fd_matrix = fd_matrix[['industry', 'share_military_usa']]
    return fd_matrix


def get_fd_usa_military_variants():
    df1 = get_military_fd(naics_combined=True, subcontracts=True)
    df1.columns = ['industry', 'share_military_combined_subcontracts']

    df2 = get_military_fd(naics_combined=False, subcontracts=True)
    df2.columns = ['industry', 'share_military_subcontracts']

    df3 = get_military_fd(naics_combined=True, subcontracts=False)
    df3.columns = ['industry', 'share_military_combined']

    df4 = get_military_fd(naics_combined=False, subcontracts=False)
    df4.columns = ['industry', 'share_military']

    df = merge_df(df1, df2, on='industry', how='outer', keep_merge=False)
    df = merge_df(df, df3, on='industry', how='outer', keep_merge=False)
    df = merge_df(df, df4, on='industry', how='outer', keep_merge=False)

    df.sort_values('industry', inplace=True)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'fd_military_usa.csv', index=False)


def cw_bea_receiver(weight=0.5):
    # does not make a difference in six-digit setting
    bea_cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype={'naics12': 'str'})
    cw = cw_usa_naics(four_digit=False, include_trade=False)

    cw['naics12'] = cw['industry_to'].map(lambda x: x[7:])
    cw = cw.groupby('naics12')['value'].sum().reset_index()
    bea_cw = merge_df(bea_cw, cw, how='outer', on='naics12', keep_merge=False)
    bea_cw['value_wgt'] = bea_cw['value'] * bea_cw['wgt']

    bea_cw = bea_cw.groupby(['bea12', 'naics12'])[['wgt', 'value_wgt']].sum().reset_index()
    bea_cw['wgt'] = bea_cw['wgt'] / bea_cw.groupby('naics12')['wgt'].transform('sum')
    bea_cw['wgt_alt'] = bea_cw['value_wgt'] / bea_cw.groupby('naics12')['value_wgt'].transform('sum')
    bea_cw.loc[bea_cw['wgt_alt'].isna(), 'wgt_alt'] = bea_cw.loc[bea_cw['wgt_alt'].isna(), 'wgt']

    bea_cw['wgt'] = weight * bea_cw['wgt'] + (1 - weight) * bea_cw['wgt_alt']
    bea_cw = bea_cw[['bea12', 'naics12', 'wgt']]
    return bea_cw


def cw_bea_sender(weight=0.5, four_digit=True):
    bea_cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype={'naics12': 'str'})
    cw = cw_usa_naics(four_digit=False, include_trade=False)

    cw['naics12'] = cw['industry_to'].map(lambda x: x[7:])
    cw = cw.groupby('naics12')['value'].sum().reset_index()
    bea_cw = merge_df(bea_cw, cw, how='outer', on='naics12', keep_merge=False)
    bea_cw['value_wgt'] = bea_cw['value'] * bea_cw['wgt']
    bea_cw['wgt'] = 1.0

    bea_cw['wgt'] = bea_cw['wgt'] / bea_cw.groupby('bea12')['wgt'].transform('sum')
    bea_cw['wgt_alt'] = bea_cw['value_wgt'] / bea_cw.groupby('bea12')['value_wgt'].transform('sum')
    bea_cw.loc[bea_cw['wgt_alt'].isna(), 'wgt_alt'] = bea_cw.loc[bea_cw['wgt_alt'].isna(), 'wgt']
    if not four_digit:
        bea_cw = bea_cw.groupby(['bea12', 'naics12'])[['wgt', 'wgt_alt']].sum().reset_index()
        bea_cw['wgt'] = weight * bea_cw['wgt'] + (1 - weight) * bea_cw['wgt_alt']
        bea_cw = bea_cw[['bea12', 'naics12', 'wgt']]
        return bea_cw

    bea_cw['naics4'] = bea_cw['naics12'].map(lambda x: x[:4])
    bea_cw = bea_cw.groupby(['bea12', 'naics4'])[['wgt', 'wgt_alt']].sum().reset_index()

    bea_cw['wgt'] = weight * bea_cw['wgt'] + (1 - weight) * bea_cw['wgt_alt']
    bea_cw = bea_cw[['bea12', 'naics4', 'wgt']]
    return bea_cw


def get_usa_consumer_fd(four_digit=True, weight=0.5):
    # select names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    if four_digit:
        naics12_list = naics12_list['naics12'].map(lambda x: x[:4]).unique().tolist()
    else:
        naics12_list = naics12_list['naics12'].unique().tolist()
    fd_list = ['USA_AGG' + i for i in naics12_list]
    fd_matrix = pd.DataFrame(fd_list, columns=['industry'])

    # construct final demand
    bea = pd.read_excel(
        DATA_PATH / 'measurement' / 'accounting' / 'bea' / 'Use_SUT_Framework_2007_2012_DET.xlsx', sheet_name='2012', skiprows=5, skipfooter=5
    )
    bea.fillna(0.0, inplace=True)
    bea['Code'] = bea['Code'].map(str)
    bea.columns = [str(col) for col in bea.columns]
    fd_usa = bea.iloc[0:405, [0, 407, 413, 414, 415, 416, 417, 418, 427]].copy()
    fd_usa['purchases'] = (
            fd_usa['T019'] - fd_usa['T001'] - fd_usa['F06C00'] - fd_usa['F06E00'] - fd_usa['F06N00'] - fd_usa['F06S00']
    )
    fd_usa = fd_usa[['Code', 'purchases']]
    fd_usa.columns = ['industry', 'purchases']
    fd_usa['purchases'] = fd_usa['purchases'].map(lambda x: 0.0 if x < 0.0 else x)
    fd_usa['purchases_sum'] = fd_usa['purchases'].sum()
    fd_usa['share_consumer'] = fd_usa['purchases'] / fd_usa['purchases_sum']
    fd_usa = fd_usa[['industry', 'share_consumer']]
    fd_usa['industry_to'] = 'USA_SEC' + fd_usa['industry']
    fd_usa = fd_usa[['industry_to', 'share_consumer']]

    cw = cw_bea_sender(weight=weight, four_digit=four_digit)
    cw.columns = ['industry_to', 'industry_from', 'wgt']
    cw['industry_to'] = 'USA_SEC' + cw['industry_to']
    cw['industry'] = 'USA_AGG' + cw['industry_from']

    fd_usa = merge_df(fd_usa, cw, on='industry_to', how='outer')
    fd_usa['share_consumer'] = fd_usa['share_consumer'] * fd_usa['wgt']
    fd_usa = fd_usa[['industry', 'share_consumer']]
    fd_usa.columns = ['industry', 'share_consumer_usa']
    fd_usa = fd_usa.groupby('industry')['share_consumer_usa'].sum().reset_index()
    fd_matrix = merge_df(fd_matrix, fd_usa, on='industry', how='left', keep_merge=False)
    fd_matrix['share_consumer_usa'] = fd_matrix['share_consumer_usa'].fillna(0.0)
    return fd_matrix


def get_fd_usa_consumer_variants():
    df1 = get_usa_consumer_fd(four_digit=False, weight=0.0)
    df1.columns = ['industry', 'share_consumer_data']
    df2 = get_usa_consumer_fd(four_digit=False, weight=0.5)
    df2.columns = ['industry', 'share_consumer_mixed']
    df3 = get_usa_consumer_fd(four_digit=False, weight=1.0)
    df3.columns = ['industry', 'share_consumer_equal']
    df = merge_df(df1, df2, on='industry', how='outer', keep_merge=False)
    df = merge_df(df, df3, on='industry', how='outer', keep_merge=False)
    print(df['share_consumer_data'].corr(df['share_consumer_mixed']))
    print(df['share_consumer_data'].corr(df['share_consumer_equal']))
    df.sort_values('industry', inplace=True)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'fd_consumer_usa.csv', index=False)


def get_usa_dom_agg(weight=0.5):
    # select names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].map(lambda x: x[:6]).unique().tolist()
    io_list = list(itertools.product(['USA_DOM' + i for i in naics12_list], ['USA_AGG' + i for i in naics12_list]))
    io_matrix = pd.DataFrame(io_list, columns=['industry_to', 'industry_from'])

    # bea IO
    bea = pd.read_excel(
        DATA_PATH / 'measurement' / 'accounting' / 'bea' / 'Use_SUT_Framework_2007_2012_DET.xlsx', sheet_name='2012', skiprows=5, skipfooter=5
    )
    bea.fillna(0.0, inplace=True)
    bea['Code'] = bea['Code'].map(str)
    bea.columns = [str(col) for col in bea.columns]
    bea = bea.iloc[list(range(405)) + [409], 0:407].copy()
    bea.drop('Commodity Description', axis=1, inplace=True)
    bea['S00500'] = 0.0

    df = bea.melt(id_vars='Code', var_name='Code_to', value_name='value')
    df['value'] = df['value'].map(lambda x: 0.0 if x < 0.0 else x)
    df.columns = ['industry_from', 'industry_to', 'purchase']
    df['purchase_sum'] = df.groupby('industry_to')['purchase'].transform('sum')
    df['share_value'] = df['purchase'] / df['purchase_sum']
    df = df[df['industry_from'] != 'VABAS']
    df = df[['industry_from', 'industry_to', 'share_value']]
    df['share_value'] = df['share_value'].fillna(0.0)
    df.groupby('industry_to')['share_value'].sum()

    cw_receiver = cw_bea_receiver(weight=weight)

    df = merge_df(df, cw_receiver, how='left', left_on='industry_to', right_on='bea12', keep_merge=False)
    df['share_value'] = df['share_value'] * df['wgt']
    df = df[['industry_from', 'naics12', 'share_value']]
    df = df.groupby(['industry_from', 'naics12'])['share_value'].sum().reset_index()
    df.columns = ['industry_from', 'industry_to', 'share_value']
    df = df[['industry_to', 'industry_from', 'share_value']]

    cw_sender = cw_bea_sender(four_digit=False, weight=weight)

    df = merge_df(df, cw_sender, left_on='industry_from', right_on='bea12', how='left', keep_merge=True)
    df['share_value'] = df['share_value'] * df['wgt']
    df = df.groupby(['industry_to', 'naics12'])['share_value'].sum().reset_index()
    df.columns = ['industry_to', 'industry_from', 'share_value']
    df['industry_from'] = 'USA_AGG' + df['industry_from']
    df['industry_to'] = 'USA_DOM' + df['industry_to']

    # self-share correction
    df['share_self'] = df['share_value'] * (
        df['industry_to'].map(lambda x: x[7:]) == df['industry_from'].map(lambda x: x[7:]))
    df['share_sum'] = df.groupby(['industry_to'])['share_value'].transform('sum')
    df['self_sum'] = df.groupby(['industry_to'])['share_self'].transform('sum')
    df['sum'] = df['share_sum'] - df['self_sum']
    df = df[df['industry_to'].map(lambda x: x[7:]) != df['industry_from'].map(lambda x: x[7:])].copy()
    df['share_value'] = df['share_value'] / df['sum'] * df['share_sum']
    df = df[['industry_to', 'industry_from', 'share_value']]

    io_matrix = merge_df(io_matrix, df, on=['industry_to', 'industry_from'], how='left', keep_merge=False)
    io_matrix['share_value'] = io_matrix['share_value'].fillna(0.0)
    return io_matrix


def get_io_usa_variants():
    io_matrix1 = get_usa_dom_agg(weight=0.0)
    io_matrix1.columns = ['industry_to', 'industry_from', 'share_value_data']

    io_matrix2 = get_usa_dom_agg(weight=0.5)
    io_matrix2.columns = ['industry_to', 'industry_from', 'share_value_mixed']

    io_matrix3 = get_usa_dom_agg(weight=1.0)
    io_matrix3.columns = ['industry_to', 'industry_from', 'share_value_equal']

    io_matrix = merge_df(io_matrix1, io_matrix2, on=['industry_to', 'industry_from'], how='outer', keep_merge=False)
    io_matrix = merge_df(io_matrix, io_matrix3, on=['industry_to', 'industry_from'], how='outer', keep_merge=False)
    io_matrix.sort_values(['industry_to', 'industry_from'], inplace=True)
    io_matrix.to_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'io_usa.csv', index=False)


def construct_attrib():
    attrib = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'fd_consumer_usa.csv')
    attrib = attrib[['industry']]
    attrib['country'] = attrib['industry'].map(lambda x: x[:3])
    attrib['type'] = attrib['industry'].map(lambda x: x[4:7])
    attrib['sector'] = attrib['industry'].map(lambda x: x[7:])

    bea_names = pd.read_excel(
        DATA_PATH / 'measurement' / 'accounting' / 'bea' / 'Use_SUT_Framework_2007_2012_DET.xlsx', sheet_name='2012', skiprows=5, skipfooter=5,
        dtype=str
    )
    bea_names = bea_names[['Code', 'Commodity Description']]
    bea_names.columns = ['sector', 'bea_name']
    attrib = merge_df(attrib, bea_names, how='left', on='sector', keep_merge=False)

    naics_names = pd.read_excel(DATA_PATH / 'crosswalks' / 'census' / '6-digit_2012_Codes.xls', skiprows=1, dtype=str)
    naics_names.columns = ['sector', 'naics_name']
    attrib = merge_df(attrib, naics_names, how='left', on='sector', keep_merge=False)

    other_names = pd.DataFrame([
        ['000000', 'Rest of the world'],
        ['999990', 'Unclassified'],
        ['S00101', 'Federal electric utilities'],
        ['S00201', 'State and local government passenger transit'],
        ['S00202', 'State and local government electric utilities'],
        ['271000', 'Oils petroleum, bituminous, distillates, except crude'],
        ['subctr', 'Subcontracts']
    ], columns=['sector', 'other_name'])
    attrib = merge_df(attrib, other_names, how='left', on='sector', keep_merge=False)
    attrib.loc[attrib['type'].isin(['AGG', 'DOM', 'MPT']), 'sector_name'] \
        = attrib.loc[attrib['type'].isin(['AGG', 'DOM', 'MPT']), 'naics_name']
    for col in ['bea_name', 'other_name', 'naics_name']:
        attrib.loc[attrib['sector_name'].isna(), 'sector_name'] = attrib.loc[attrib['sector_name'].isna(), col]

    attrib = attrib[['industry', 'sector_name']]
    attrib['sector'] = attrib['industry'].map(lambda x: x[7:])
    attrib['naics12_two_digit'] = attrib['sector'].map(lambda x: x[:2])
    attrib['naics12_two_digit'] = attrib['naics12_two_digit'].map(
        lambda x: '31-33' if x == '31' or x == '32' or x == '33' else
        '44-45' if x == '44' or x == '45' else
        '48-49' if x == '48' or x == '49' else x
    )
    attrib['naics12_two_digit_desc'] = attrib['naics12_two_digit'].map(
        lambda x: 'Agriculture, Forestry, Fishing and Hunting' if x == '11' else
        'Mining, Quarrying, and Oil and Gas Extraction' if x == '21' else
        'Utilities' if x == '22' else
        'Construction' if x == '23' else
        'Manufacturing' if x == '31-33' else
        'Wholesale Trade' if x == '42' else
        'Retail Trade' if x == '44-45' else
        'Transportation and Warehousing' if x == '48-49' else
        'Information' if x == '51' else
        'Finance and Insurance' if x == '52' else
        'Real Estate and Rental and Leasing' if x == '53' else
        'Professional, Scientific, and Technical Services' if x == '54' else
        'Management of Companies and Enterprises' if x == '55' else
        'Administrative and Support and Waste Management and Remediation Services' if x == '56' else
        'Educational Services' if x == '61' else
        'Health Care and Social Assistance' if x == '62' else
        'Arts, Entertainment, and Recreation' if x == '71' else
        'Accommodation and Food Services' if x == '72' else
        'Other Services (except Public Administration)' if x == '81' else
        'Public Administration' if x == '92' else
        'Unknown'
    )

    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs4_naics12.csv', dtype={'hscode': 'str', 'naics12': 'str'})
    cw = cw.groupby('naics12').first().reset_index()[['naics12']]
    attrib = merge_df(attrib, cw, how='left', left_on='sector', right_on='naics12', keep_merge=False)
    attrib['tradable'] = 1 - attrib['naics12'].isna().astype(int)
    attrib.drop('naics12', axis=1, inplace=True)
    attrib.to_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'attrib.csv', index=False)


def assemble_elasticities_4digit():
    hscode = pd.read_csv(
        OUTPUT_PATH / 'motivation' / 'dualuse_masterfile_hs4.csv',
        dtype={'sector': 'str', 'dualuse': 'str', 'dualuse_categ': 'str', 'dualuse_subct': 'str'}
    )[['sector']]

    # map in elasticities
    elast = pd.read_csv(DATA_PATH / 'measurement' / 'accounting' / 'elasticities' / 'soderberry' / 'elast_LIMLhybrid_hs8.csv', dtype={'good': 'str'})
    elast['good'] = elast['good'].map(lambda x: x.zfill(8))
    elast['hs6'] = elast['good'].map(lambda x: x[:6])

    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs12_combined.csv', dtype={'hscode_from': 'str', 'hscode12': 'str'})
    cw.loc[cw['hscode12'].map(lambda x: x.startswith('2710')), 'hscode12'] = '271000'
    elast = merge_df(elast, cw, left_on='hs6', right_on='hscode_from', how='left')
    elast['wgt_sum'] = elast.groupby('hscode12')['wgt'].transform('sum')
    elast['wt_mappings'] = elast['wgt'] / elast['wgt_sum']
    elast['sigma_wgt'] = elast['sigma'] * elast['wt_mappings']
    elast = elast.groupby('hscode12')['sigma_wgt'].sum().reset_index()
    elast = elast[['hscode12', 'sigma_wgt']]
    elast.columns = ['sector', 'sigma']
    elast = merge_df(hscode, elast, how='left', left_on='sector', right_on='sector', keep_merge=False)
    elast['hs4'] = elast['sector'].map(lambda x: x[:4])
    elast['epsilon_mean'] = elast.groupby('hs4')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['hs2'] = elast['sector'].map(lambda x: x[:2])
    elast['epsilon_mean'] = elast.groupby('hs2')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['epsilon_mean'] = elast['sigma'].mean()
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast = elast.groupby('hs4')['sigma'].mean().reset_index()
    elast1 = elast.copy()
    elast1.columns = ['hs4', 'sigma_sdb']

    # Broda-Weinstein from Soderberry
    elast = pd.read_csv(DATA_PATH / 'measurement' / 'accounting' / 'elasticities' / 'soderberry' / 'elast_FBW_hs8.csv', dtype={'good': 'str'})
    elast['good'] = elast['good'].map(lambda x: x.zfill(8))
    elast['hs6'] = elast['good'].map(lambda x: x[:6])

    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs12_combined.csv', dtype={'hscode_from': 'str', 'hscode12': 'str'})
    cw.loc[cw['hscode12'].map(lambda x: x.startswith('2710')), 'hscode12'] = '271000'
    elast = merge_df(elast, cw, left_on='hs6', right_on='hscode_from', how='left')
    elast['wgt_sum'] = elast.groupby('hscode12')['wgt'].transform('sum')
    elast['wt_mappings'] = elast['wgt'] / elast['wgt_sum']
    elast['sigma_wgt'] = elast['sigma'] * elast['wt_mappings']
    elast = elast.groupby('hscode12')['sigma_wgt'].sum().reset_index()
    elast = elast[['hscode12', 'sigma_wgt']]
    elast.columns = ['sector', 'sigma']
    elast = merge_df(hscode, elast, how='left', left_on='sector', right_on='sector', keep_merge=False)
    elast['hs4'] = elast['sector'].map(lambda x: x[:4])
    elast['epsilon_mean'] = elast.groupby('hs4')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['hs2'] = elast['sector'].map(lambda x: x[:2])
    elast['epsilon_mean'] = elast.groupby('hs2')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['epsilon_mean'] = elast['sigma'].mean()
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast = elast.groupby('hs4')['sigma'].mean().reset_index()
    elast2 = elast.copy()
    elast2.columns = ['hs4', 'sigma_sbw']

    # broda-weinstein original
    elast = pd.read_excel(
        DATA_PATH / 'measurement' / 'accounting' / 'elasticities' / 'broda_weinstein' / 'ElasticitiesBrodaWeinstein90-01_HTS.xls', skiprows=3,
        dtype={'HTS': 'str'})
    elast['HTS'] = elast['HTS'].map(lambda x: x.zfill(10))
    elast['hs6'] = elast['HTS'].map(lambda x: x[:6])
    elast.columns = ['hts', 'sigma', 'hs6']

    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs12_combined.csv', dtype={'hscode_from': 'str', 'hscode12': 'str'})
    cw.loc[cw['hscode12'].map(lambda x: x.startswith('2710')), 'hscode12'] = '271000'
    elast = merge_df(elast, cw, left_on='hs6', right_on='hscode_from', how='left')
    elast['wgt_sum'] = elast.groupby('hscode12')['wgt'].transform('sum')
    elast['wt_mappings'] = elast['wgt'] / elast['wgt_sum']
    elast['sigma_wgt'] = elast['sigma'] * elast['wt_mappings']
    elast = elast.groupby('hscode12')['sigma_wgt'].sum().reset_index()
    elast = elast[['hscode12', 'sigma_wgt']]
    elast.columns = ['sector', 'sigma']
    elast = merge_df(hscode, elast, how='left', left_on='sector', right_on='sector', keep_merge=False)
    elast['hs4'] = elast['sector'].map(lambda x: x[:4])
    elast['epsilon_mean'] = elast.groupby('hs4')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['hs2'] = elast['sector'].map(lambda x: x[:2])
    elast['epsilon_mean'] = elast.groupby('hs2')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['epsilon_mean'] = elast['sigma'].mean()
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast = elast.groupby('hs4')['sigma'].mean().reset_index()
    elast3 = elast.copy()
    elast3.columns = ['hs4', 'sigma_bw']

    # map in elasticities
    elast = pd.read_stata(DATA_PATH / 'measurement' / 'accounting' / 'elasticities' / 'fontagne' / 'elasticity_for_publication_2021_09_29.dta')
    elast = elast[~elast['epsilon_pt'].isna()]
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs07_hs12.csv', dtype={'hs07': 'str', 'hs12': 'str'})
    cw.loc[cw['hs12'].map(lambda x: x.startswith('2710')), 'hs12'] = '271000'
    elast = merge_df(elast, cw, left_on='HS6', right_on='hs07', how='left')
    elast['epsilon_pt'] = elast['epsilon_pt'] * elast['wgt']
    elast['sigma'] = elast.groupby(
        'hs12')['epsilon_pt'].transform('sum') / elast.groupby('hs12')['wgt'].transform('sum')
    elast = elast[['hs12', 'epsilon_pt']]

    hscode = pd.read_csv(
        OUTPUT_PATH / 'motivation' / 'dualuse_masterfile_hs4.csv',
        dtype={'sector': 'str', 'dualuse': 'str', 'dualuse_categ': 'str', 'dualuse_subct': 'str'}
    )[['sector']]
    hscode.columns = ['hs12']
    elast = merge_df(elast, hscode, left_on='hs12', right_on='hs12', how='right')

    elast['hs4'] = elast['hs12'].map(lambda x: x[:4])
    elast['epsilon_mean'] = elast.groupby('hs4')['epsilon_pt'].transform('mean')
    elast['sigma'] = elast['epsilon_pt']
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['hs2'] = elast['hs12'].map(lambda x: x[:2])
    elast['epsilon_mean'] = elast.groupby('hs2')['epsilon_pt'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['epsilon_mean'] = elast['epsilon_pt'].mean()
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['sigma'] = -elast['sigma']
    elast = elast.groupby('hs4')['sigma'].mean().reset_index()
    elast4 = elast.copy()
    elast4.columns = ['hs4', 'sigma_cepii']

    df = merge_df(elast1, elast2, on='hs4', how='outer', keep_merge=False)
    df = merge_df(df, elast3, on='hs4', how='outer', keep_merge=False)
    df = merge_df(df, elast4, on='hs4', how='outer', keep_merge=False)
    inames = ['Soderbery (2015) LIML', 'Soderbery (2015) BW', 'Broda-Weinstein (2006)', 'Fontagne (2021) et al.']
    cnames = ['S2015 LIML', 'S2015 BW', 'BW2006', 'F2021']
    df_corr = df.corr()
    df_corr.columns = cnames
    df_corr.index = inames
    df_corr.to_latex(STATS_PATH / 'measurement' / 'accounting' / 'elast_corr_4digit.tex',
                    float_format='%.3f', column_format='|rcccc|')
    df.to_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'hs12_elast_4digit.csv', index=False)


def assemble_elasticities():
    hscode = pd.read_csv(
        OUTPUT_PATH / 'motivation' / 'dualuse_masterfile_hs4.csv',
        dtype={'sector': 'str', 'dualuse': 'str', 'dualuse_categ': 'str', 'dualuse_subct': 'str'}
    )[['sector']]

    # map in elasticities
    elast = pd.read_csv(DATA_PATH / 'measurement' / 'accounting' / 'elasticities' / 'soderberry' / 'elast_LIMLhybrid_hs8.csv', dtype={'good': 'str'})
    elast['good'] = elast['good'].map(lambda x: x.zfill(8))
    elast['hs6'] = elast['good'].map(lambda x: x[:6])

    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs12_combined.csv', dtype={'hscode_from': 'str', 'hscode12': 'str'})
    cw.loc[cw['hscode12'].map(lambda x: x.startswith('2710')), 'hs12'] = '271000'
    elast = merge_df(elast, cw, left_on='hs6', right_on='hscode_from', how='left')
    elast['wgt_sum'] = elast.groupby('hscode12')['wgt'].transform('sum')
    elast['wt_mappings'] = elast['wgt'] / elast['wgt_sum']
    elast['sigma_wgt'] = elast['sigma'] * elast['wt_mappings']
    elast = elast.groupby('hscode12')['sigma_wgt'].sum().reset_index()
    elast = elast[['hscode12', 'sigma_wgt']]
    elast.columns = ['sector', 'sigma']
    elast = merge_df(hscode, elast, how='left', left_on='sector', right_on='sector', keep_merge=False)
    elast['hs4'] = elast['sector'].map(lambda x: x[:4])
    elast['epsilon_mean'] = elast.groupby('hs4')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['hs2'] = elast['sector'].map(lambda x: x[:2])
    elast['epsilon_mean'] = elast.groupby('hs2')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['epsilon_mean'] = elast['sigma'].mean()
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast1 = elast[['sector', 'sigma']].copy()
    elast1.columns = ['hs12', 'sigma_sdb']

    # broda-weinstein from soderberry
    elast = pd.read_csv(DATA_PATH / 'measurement' / 'accounting' / 'elasticities' / 'soderberry' / 'elast_FBW_hs8.csv', dtype={'good': 'str'})
    elast['good'] = elast['good'].map(lambda x: x.zfill(8))
    elast['hs6'] = elast['good'].map(lambda x: x[:6])

    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs12_combined.csv', dtype={'hscode_from': 'str', 'hscode12': 'str'})
    cw.loc[cw['hscode12'].map(lambda x: x.startswith('2710')), 'hs12'] = '271000'
    elast = merge_df(elast, cw, left_on='hs6', right_on='hscode_from', how='left')
    elast['wgt_sum'] = elast.groupby('hscode12')['wgt'].transform('sum')
    elast['wt_mappings'] = elast['wgt'] / elast['wgt_sum']
    elast['sigma_wgt'] = elast['sigma'] * elast['wt_mappings']
    elast = elast.groupby('hscode12')['sigma_wgt'].sum().reset_index()
    elast = elast[['hscode12', 'sigma_wgt']]
    elast.columns = ['sector', 'sigma']
    elast = merge_df(hscode, elast, how='left', left_on='sector', right_on='sector', keep_merge=False)
    elast['hs4'] = elast['sector'].map(lambda x: x[:4])
    elast['epsilon_mean'] = elast.groupby('hs4')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['hs2'] = elast['sector'].map(lambda x: x[:2])
    elast['epsilon_mean'] = elast.groupby('hs2')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['epsilon_mean'] = elast['sigma'].mean()
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast2 = elast[['sector', 'sigma']].copy()
    elast2.columns = ['hs12', 'sigma_sbw']

    # broda-weinstein original
    elast = pd.read_excel(
        DATA_PATH / 'measurement' / 'accounting' / 'elasticities' / 'broda_weinstein' / 'ElasticitiesBrodaWeinstein90-01_HTS.xls', skiprows=3,
        dtype={'HTS': 'str'})
    elast['HTS'] = elast['HTS'].map(lambda x: x.zfill(10))
    elast['hs6'] = elast['HTS'].map(lambda x: x[:6])
    elast.columns = ['hts', 'sigma', 'hs6']

    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs12_combined.csv', dtype={'hscode_from': 'str', 'hscode12': 'str'})
    cw.loc[cw['hscode12'].map(lambda x: x.startswith('2710')), 'hs12'] = '271000'
    elast = merge_df(elast, cw, left_on='hs6', right_on='hscode_from', how='left')
    elast['wgt_sum'] = elast.groupby('hscode12')['wgt'].transform('sum')
    elast['wt_mappings'] = elast['wgt'] / elast['wgt_sum']
    elast['sigma_wgt'] = elast['sigma'] * elast['wt_mappings']
    elast = elast.groupby('hscode12')['sigma_wgt'].sum().reset_index()
    # elast['sigma'] = elast['sigma_wgt'] - 1
    elast = elast[['hscode12', 'sigma_wgt']]
    elast.columns = ['sector', 'sigma']
    elast = merge_df(hscode, elast, how='left', left_on='sector', right_on='sector', keep_merge=False)
    elast['hs4'] = elast['sector'].map(lambda x: x[:4])
    elast['epsilon_mean'] = elast.groupby('hs4')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['hs2'] = elast['sector'].map(lambda x: x[:2])
    elast['epsilon_mean'] = elast.groupby('hs2')['sigma'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['epsilon_mean'] = elast['sigma'].mean()
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast3 = elast[['sector', 'sigma']].copy()
    elast3.columns = ['hs12', 'sigma_bw']

    # map in elasticities
    elast = pd.read_stata(DATA_PATH / 'measurement' / 'accounting' / 'elasticities' / 'fontagne' / 'elasticity_for_publication_2021_09_29.dta')
    elast = elast[~elast['epsilon'].isna()]
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs07_hs12.csv', dtype={'hs07': 'str', 'hs12': 'str'})
    cw.loc[cw['hs12'].map(lambda x: x.startswith('2710')), 'hs12'] = '271000'
    elast = merge_df(elast, cw, left_on='HS6', right_on='hs07', how='left')
    elast['epsilon_pt'] = elast['epsilon'] * elast['wgt']
    elast['sigma'] = elast.groupby(
        'hs12')['epsilon_pt'].transform('sum') / elast.groupby('hs12')['wgt'].transform('sum')
    elast = elast[['hs12', 'epsilon_pt']]

    hscode = pd.read_csv(
        OUTPUT_PATH / 'motivation' / 'dualuse_masterfile_hs4.csv',
        dtype={'sector': 'str', 'dualuse': 'str', 'dualuse_categ': 'str', 'dualuse_subct': 'str'}
    )[['sector']]
    hscode.columns = ['hs12']
    elast = merge_df(elast, hscode, left_on='hs12', right_on='hs12', how='right')

    elast['hs4'] = elast['hs12'].map(lambda x: x[:4])
    elast['epsilon_mean'] = elast.groupby('hs4')['epsilon_pt'].transform('mean')
    elast['sigma'] = elast['epsilon_pt']
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['hs2'] = elast['hs12'].map(lambda x: x[:2])
    elast['epsilon_mean'] = elast.groupby('hs2')['epsilon_pt'].transform('mean')
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['epsilon_mean'] = elast['epsilon_pt'].mean()
    elast.loc[elast['sigma'].isna(), 'sigma'] = elast.loc[elast['sigma'].isna(), 'epsilon_mean']
    elast['sigma'] = -elast['sigma']
    elast4 = elast[['hs12', 'sigma']].copy()
    elast4.columns = ['hs12', 'sigma_cepii']

    df = merge_df(elast1, elast2, on='hs12', how='outer', keep_merge=False)
    df = merge_df(df, elast3, on='hs12', how='outer', keep_merge=False)
    df = merge_df(df, elast4, on='hs12', how='outer', keep_merge=False)
    df_corr = df.corr()
    inames = ['Soderbery (2015) LIML', 'Soderbery (2015) BW', 'Broda-Weinstein (2006)', 'Fontagne (2021) et al.']
    cnames = ['S2015 LIML', 'S2015 BW', 'BW2006', 'F2021']
    df_corr.columns = cnames
    df_corr.index = inames
    df_corr.to_latex(STATS_PATH / 'measurement' / 'accounting' / 'elast_corr.tex', float_format='%.3f', column_format='|rcccc|')
    df.to_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'hs12_elast.csv', index=False)


def save_fd_io():
    get_fd_usa_military_variants()
    get_fd_usa_consumer_variants()
    get_io_usa_variants()


def main():
    construct_expenditures()
    save_fd_io()
    construct_attrib()
    assemble_elasticities_4digit()
    assemble_elasticities()


if __name__ == '__main__':
    main()

