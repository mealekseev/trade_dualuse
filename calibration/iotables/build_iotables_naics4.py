""" Build USA-CHN input-output network """

import numpy as np
import pandas as pd
import itertools
import os

from settings import DATA_PATH, OUTPUT_PATH, EXTERNAL_PATH
from utilities import merge_df


def get_trade():
    df_xp = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_xp_4digit.csv')
    df_mp = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables'/ 'aux_trade_mp_4digit.csv')
    trade = pd.concat([df_xp, df_mp], axis=0, ignore_index=True)
    assert max(abs(trade.groupby('industry_to')['share_value'].sum() - 1.0)) < 1e-14
    return trade


def get_usa_agg_dom_mpt():
    from measurement.centrality.build_io_usa import cw_usa_naics

    io_matrix = cw_usa_naics(four_digit=True, include_trade=True)
    assert max(abs(io_matrix.groupby('industry_to')['share_value'].sum() - 1.0)) < 1e-14
    return io_matrix


def cw_chn_sender(weight=0.5, trade_wgt=True, old_version=True):
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'chinaiosector18_naics12.csv', dtype={'naics12': 'str', 'w': 'float'})
    cw['wgt'] = cw['w'].astype(float)
    cw = cw[['iosector_code', 'naics12', 'wgt']]

    if trade_wgt:
        cw_trade = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_xp.csv')
        cw_trade = cw_trade[cw_trade['industry_from'].map(lambda x: x[:3] == 'CHN')].copy()
        cw_trade = cw_trade.groupby('industry_from')['value'].sum().reset_index()
        cw_trade['naics12'] = cw_trade['industry_from'].map(lambda x: x[7:])
        cw_trade = cw_trade[['naics12', 'value']]
    else:
        cw_trade = get_usa_consumer_fd(four_digit=False)
        cw_trade['naics12'] = cw_trade['industry'].map(lambda x: x[7:])
        cw_trade['value'] = cw_trade['share_consumer_usa']
        cw_trade = cw_trade[['naics12', 'value']]

    cw = merge_df(cw, cw_trade, on='naics12', how='left', keep_merge=False)
    cw.loc[cw['value'].isna(), 'value'] = 0.0
    cw['value_wgt'] = cw['value'] * 1 / cw.groupby('naics12')['iosector_code'].transform('count')

    cw['wgt_alt'] = cw['value_wgt'] / cw.groupby('iosector_code')['value_wgt'].transform('sum')
    cw.loc[cw['wgt_alt'].isna(), 'wgt_alt'] = cw.loc[cw['wgt_alt'].isna(), 'wgt']
    cw['naics4'] = cw['naics12'].map(lambda x: x[:4])

    if not old_version:
        # fix ice manufacturing
        cw.loc[cw['iosector_code'] == '44098', 'wgt_alt'] = cw.loc[cw['iosector_code'] == '44098', 'wgt'].copy()
    cw['wgt'] = weight * cw['wgt'] + (1 - weight) * cw['wgt_alt']
    cw = cw[['iosector_code', 'naics4', 'wgt']]
    cw = cw.groupby(['iosector_code', 'naics4'])['wgt'].sum().reset_index()
    assert max(abs(cw.groupby('iosector_code')['wgt'].sum() - 1.0)) < 1e-14
    return cw


def cw_chn_receiver(weight=0.5, rmb_usd_exchange=6.63):
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'chinaiosector18_naics12.csv', dtype={'naics12': 'str', 'w': 'float'})
    cw['wgt'] = cw['w'].astype(float)
    cw = cw[['iosector_code', 'naics12', 'wgt']]
    cw['naics4'] = cw['naics12'].map(lambda x: x[:4])

    naics_chn = pd.read_csv(
        OUTPUT_PATH / 'calibration' / 'iotables' / 'sector18_chn.csv', dtype={'iosector_code': 'str'})
    naics_chn['sales'] = (naics_chn['gross_output'] - naics_chn['export']) * 10000 / rmb_usd_exchange

    cw = merge_df(cw, naics_chn, left_on='iosector_code', right_on='iosector', how='left', keep_merge=False)
    cw['sales_wgt'] = cw['sales'] / cw.groupby('iosector')['iosector_code'].transform('count')
    cw['wgt'] = 1 / cw.groupby('naics4')['iosector'].transform('count')
    cw['wgt_alt'] = cw['sales_wgt'] / cw.groupby('naics4')['sales_wgt'].transform('sum')
    cw.loc[cw['wgt_alt'].isna(), 'wgt_alt'] = cw.loc[cw['wgt_alt'].isna(), 'wgt']

    cw['wgt'] = weight * cw['wgt'] + (1 - weight) * cw['wgt_alt']
    cw = cw.groupby(['naics4', 'iosector_code'])[['wgt']].sum().reset_index()
    assert max(abs(cw.groupby('naics4')['wgt'].sum() - 1.0)) < 1e-14
    return cw


def get_chn_agg_dom_mpt(rmb_usd_exchange=6.63):
    # select names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].map(lambda x: x[:4]).unique().tolist()
    io_list = (
            [['CHN_AGG' + i, 'CHN_DOM' + i] for i in naics12_list] +
            [['CHN_AGG' + i, 'CHN_MPT' + i] for i in naics12_list]
    )
    io_matrix = pd.DataFrame(io_list, columns=['industry_to', 'industry_from'])

    cw = cw_chn_sender(weight=0.0)

    naics_chn = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'sector18_chn.csv', dtype={'iosector_code': 'str'})
    naics_chn['sales'] = (naics_chn['gross_output'] - naics_chn['export']) * 10000 / rmb_usd_exchange
    naics_chn = merge_df(naics_chn, cw, how='left', left_on='iosector', right_on='iosector_code', keep_merge=False)
    naics_chn['sales_wgt'] = naics_chn['sales'] * naics_chn['wgt']
    naics_chn = naics_chn.groupby('naics4')['sales_wgt'].sum().reset_index()
    naics_chn['industry_to'] = 'CHN_AGG' + naics_chn['naics4']
    naics_chn['industry_from'] = 'CHN_DOM' + naics_chn['naics4']
    naics_chn.rename({'sales_wgt': 'value'}, axis=1, inplace=True)
    naics_chn = naics_chn[['industry_to', 'industry_from', 'value']]

    trade = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_mp_4digit.csv')
    trade = trade[trade['industry_to'].map(lambda x: x.startswith('CHN'))].copy()
    trade = trade.groupby('industry_to').first().reset_index()
    trade = trade[['industry_to', 'value_sum']]
    trade.columns = ['industry_from', 'value']
    trade['industry_to'] = 'CHN_AGG' + trade['industry_from'].map(lambda x: x[7:])
    trade = trade[['industry_to', 'industry_from', 'value']]

    df = pd.concat([naics_chn, trade], axis=0, ignore_index=True)
    io_matrix = merge_df(io_matrix, df, on=['industry_to', 'industry_from'], how='left', keep_merge=False)
    io_matrix['value'] = io_matrix['value'].fillna(0.0)
    io_matrix['value_sum'] = io_matrix.groupby('industry_to')['value'].transform('sum')
    io_matrix['value_sum'] = io_matrix['value_sum'].map(lambda x: 1 if x == 0 else x)
    io_matrix['share_value'] = io_matrix['value'] / io_matrix['value_sum']
    io_matrix = io_matrix[['industry_to', 'industry_from', 'share_value']]
    io_matrix['share_value_sum'] = io_matrix.groupby('industry_to')['share_value'].transform('sum')
    io_matrix.loc[
        (io_matrix['share_value_sum'] == 0) & (io_matrix['industry_from'].map(lambda x: x[4:7]) == 'DOM'),
        'share_value'
    ] = 1.0
    io_matrix = io_matrix[['industry_to', 'industry_from', 'share_value']]
    assert max(abs(io_matrix.groupby('industry_to')['share_value'].sum() - 1.0)) < 1e-15
    return io_matrix


def cw_bea_receiver(weight=0.5):
    from measurement.centrality.build_io_usa import cw_usa_naics

    bea_cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype={'naics12': 'str'})
    cw = cw_usa_naics(four_digit=False, include_trade=True)

    cw['naics12'] = cw['industry_to'].map(lambda x: x[7:])
    cw = cw.groupby('naics12')['value'].sum().reset_index()
    bea_cw = merge_df(bea_cw, cw, how='outer', on='naics12', keep_merge=False)
    bea_cw['value_wgt'] = bea_cw['value'] * bea_cw['wgt']

    bea_cw['naics4'] = bea_cw['naics12'].map(lambda x: x[:4])
    bea_cw = bea_cw.groupby(['bea12', 'naics4'])[['wgt', 'value_wgt']].sum().reset_index()
    bea_cw['wgt'] = bea_cw['wgt'] / bea_cw.groupby('naics4')['wgt'].transform('sum')
    bea_cw['wgt_alt'] = bea_cw['value_wgt'] / bea_cw.groupby('naics4')['value_wgt'].transform('sum')
    bea_cw.loc[bea_cw['wgt_alt'].isna(), 'wgt_alt'] = bea_cw.loc[bea_cw['wgt_alt'].isna(), 'wgt']

    bea_cw['wgt'] = weight * bea_cw['wgt'] + (1 - weight) * bea_cw['wgt_alt']
    bea_cw = bea_cw[['bea12', 'naics4', 'wgt']]
    return bea_cw


def cw_bea_sender(weight=0.5, four_digit=True):
    from measurement.centrality.build_io_usa import cw_usa_naics

    bea_cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype={'naics12': 'str'})
    cw = cw_usa_naics(four_digit=False, include_trade=True)

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


def get_usa_dom_agg():
    # select names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].map(lambda x: x[:4]).unique().tolist()
    io_list = list(itertools.product(['USA_DOM' + i for i in naics12_list], ['USA_AGG' + i for i in naics12_list]))
    io_matrix = pd.DataFrame(io_list, columns=['industry_to', 'industry_from'])

    # bea IO
    bea = pd.read_excel(
        DATA_PATH / 'measurement' / 'accounting' / 'bea' / 'Use_SUT_Framework_2007_2012_DET.xlsx',
        sheet_name='2012', skiprows=5, skipfooter=5
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

    cw_receiver = cw_bea_receiver(weight=0.0)

    df = merge_df(df, cw_receiver, how='left', left_on='industry_to', right_on='bea12', keep_merge=False)
    df['share_value'] = df['share_value'] * df['wgt']
    df = df[['industry_from', 'naics4', 'share_value']]
    df = df.groupby(['industry_from', 'naics4'])['share_value'].sum().reset_index()
    df.columns = ['industry_from', 'industry_to', 'share_value']
    df = df[['industry_to', 'industry_from', 'share_value']]

    cw_sender = cw_bea_sender(weight=0.0)

    df = merge_df(df, cw_sender, left_on='industry_from', right_on='bea12', how='left', keep_merge=True)
    df['share_value'] = df['share_value'] * df['wgt']
    df = df.groupby(['industry_to', 'naics4'])['share_value'].sum().reset_index()
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


def get_chn_dom_agg(old_version=True):
    # select names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].map(lambda x: x[:4]).unique().tolist()
    io_list = list(itertools.product(['CHN_DOM' + i for i in naics12_list], ['CHN_AGG' + i for i in naics12_list]))
    io_matrix = pd.DataFrame(io_list, columns=['industry_to', 'industry_from'])

    df = pd.read_csv(
        OUTPUT_PATH / 'calibration' / 'iotables' / 'iofromto18_chn.csv', dtype={'iofrom': 'str', 'ioto': 'str'})
    df['share_value'] = df['v'] / df.groupby('ioto')['v'].transform('sum')
    va = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'sector18_chn.csv', dtype={'iosector': 'str'})
    va['share_va'] = va['va'] / va['gross_output']
    va = va[['iosector', 'share_va']]
    df = merge_df(df, va, how='left', left_on='ioto', right_on='iosector', keep_merge=False)
    df['share_value'] = df['share_value'] * (1 - df['share_va'])
    df = df[['iofrom', 'ioto', 'share_value']]

    if not old_version:
        cw_receiver = cw_chn_receiver(weight=0.5)
    else:
        cw_receiver = cw_chn_receiver(weight=0.0)
    df = merge_df(df, cw_receiver, how='left', left_on='ioto', right_on='iosector_code', keep_merge=False)
    df['share_value'] = df['share_value'] * df['wgt']
    df = df.groupby(['naics4', 'iofrom'])['share_value'].sum().reset_index()
    df.columns = ['industry_to', 'industry_from', 'share_value']

    if not old_version:
        cw_sender = cw_chn_receiver(weight=0.5)
    else:
        cw_sender = cw_chn_sender(weight=0.0)
    df = merge_df(df, cw_sender, how='left', left_on='industry_from', right_on='iosector_code', keep_merge=False)
    df['share_value'] = df['share_value'] * df['wgt']
    df = df.groupby(['industry_to', 'naics4'])['share_value'].sum().reset_index()
    df.columns = ['industry_to', 'industry_from', 'share_value']
    df['industry_from'] = 'CHN_AGG' + df['industry_from']
    df['industry_to'] = 'CHN_DOM' + df['industry_to']

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


def get_row_io():
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].map(lambda x: x[:4]).unique().tolist()
    io_matrix = pd.DataFrame(
        [['ROW_LAB0000', 'ROW_LAB0000', 0.0]] +
        [['ROW_LAB0000', 'ROW_MPT' + i, 0.0] for i in naics12_list] +
        [['ROW_MPT' + i, 'ROW_LAB0000', 0.0] for i in naics12_list] +
        [['USA_MPT' + i, 'USA_AGG' + i, 0.0] for i in naics12_list] +
        [['CHN_MPT' + i, 'CHN_AGG' + i, 0.0] for i in naics12_list],
        columns=['industry_to', 'industry_from', 'share_value']
    )
    return io_matrix


def get_military_fd():
    # select names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].map(lambda x: x[:4]).unique().tolist()
    fd_list = ['CHN_DOM' + i for i in naics12_list] + ['USA_DOM' + i for i in naics12_list]
    fd_matrix = pd.DataFrame(fd_list, columns=['industry'])

    # read US shares
    df_usa = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'fd_military_usa.csv')
    df_usa = df_usa[['industry', 'share_military_combined']]
    df_usa['industry'] = df_usa['industry'].map(lambda x: x.replace('USA_AGG', 'USA_DOM'))
    df_usa.columns = ['industry', 'share_value']
    df_usa['industry'] = df_usa['industry'].map(lambda x: x[:(7 + 4)])
    df_usa = df_usa.groupby('industry')['share_value'].sum().reset_index()
    df_usa.columns = ['industry', 'share_military_usa']

    # prepare CHN shares
    df_chn = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_military_chn.csv')
    df_chn.columns = ['industry', 'share_value']
    df_chn['industry'] = df_chn['industry'].map(lambda x: x[:(7 + 4)])
    df_chn = df_chn.groupby('industry')['share_value'].sum().reset_index()
    df_chn.columns = ['industry', 'share_military_chn']

    # merge
    df = merge_df(df_usa, df_chn, on='industry', how='outer', keep_merge=False)
    df['share_military_usa'] = df['share_military_usa'].fillna(0.0)
    df['share_military_chn'] = df['share_military_chn'].fillna(0.0)
    fd_matrix = merge_df(fd_matrix, df, on='industry', how='left', keep_merge=False)
    return fd_matrix


def get_usa_consumer_fd(four_digit=True):
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
        DATA_PATH / 'measurement' / 'accounting' / 'bea' / 'Use_SUT_Framework_2007_2012_DET.xlsx',
        sheet_name='2012', skiprows=5, skipfooter=5
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

    cw = cw_bea_sender(weight=0.0, four_digit=four_digit)
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


def get_chn_consumer_fd(reshipping_version=True):
    # names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].map(lambda x: x[:4]).unique().tolist()
    fd_list = ['CHN_AGG' + i for i in naics12_list]
    fd_matrix = pd.DataFrame(fd_list, columns=['industry'])

    cw = cw_chn_sender(weight=0.0, trade_wgt=False)
    fd_chn = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'sector18_chn.csv', dtype={'iosector': 'str'})
    fd_chn = merge_df(fd_chn, cw, how='left', left_on='iosector', right_on='iosector_code', keep_merge=False)
    fd_chn['value'] = fd_chn['total_hh_consumption'] * fd_chn['wgt']

    fd_chn = fd_chn.groupby('naics4')['value'].sum().reset_index()
    fd_chn['value_sum'] = fd_chn['value'].sum()
    fd_chn['share_consumer'] = fd_chn['value'] / fd_chn['value_sum']
    fd_chn['industry'] = 'CHN_AGG' + fd_chn['naics4']
    fd_chn = fd_chn[['industry', 'share_consumer']]
    fd_chn.columns = ['industry', 'share_consumer_chn']

    fd_matrix = merge_df(fd_matrix, fd_chn, on='industry', how='left', keep_merge=False)
    fd_matrix['share_consumer_chn'] = fd_matrix['share_consumer_chn'].fillna(0.0)
    return fd_matrix


def get_row_fd(home_share=0.9, old_version=True):
    if not old_version:
        trade_naics = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_mp_4digit.csv')
    else:
        trade_naics = pd.read_csv(
            OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_naics_chn_usa_row_naics4_copy.csv')
    trade_naics = trade_naics.groupby('industry_to')[['value_sum']].first().reset_index()
    trade_naics.columns = ['industry', 'value']
    trade_naics = trade_naics[trade_naics['industry'].map(lambda x: x[:3] == 'ROW')]
    trade_naics['share_consumer_row'] = trade_naics['value'] / trade_naics['value'].sum()
    trade_naics['share_consumer_row'] = trade_naics['share_consumer_row'] * (1 - home_share)
    trade_naics['share_military_row'] = 0.0

    trade_add = pd.DataFrame(
        [['ROW_LAB0000', 0.0, home_share, 1.0]],
        columns=['industry', 'value', 'share_consumer_row', 'share_military_row']
    )
    trade_naics = pd.concat([trade_naics, trade_add], axis=0, ignore_index=True)
    trade_naics = trade_naics[['industry', 'share_consumer_row', 'share_military_row']]
    return trade_naics


def construct_final_matrix():
    trade = get_trade()
    usa_agg_dom_mpt = get_usa_agg_dom_mpt()
    usa_dom_agg = get_usa_dom_agg()
    chn_agg_dom_mpt = get_chn_agg_dom_mpt()
    chn_dom_agg = get_chn_dom_agg()
    row_io = get_row_io()
    io_matrix = pd.concat([trade, usa_agg_dom_mpt, usa_dom_agg, chn_agg_dom_mpt, chn_dom_agg, row_io],
                          axis=0, ignore_index=True)
    io_matrix.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4.csv', index=False)

    fd_list = sorted(
        list(set(io_matrix['industry_to'].unique().tolist() + io_matrix['industry_from'].unique().tolist())))
    fd_matrix = pd.DataFrame(fd_list, columns=['industry'])
    military_fd = get_military_fd()
    usa_consumer_fd = get_usa_consumer_fd()
    chn_consumer_fd = get_chn_consumer_fd()
    row_fd = get_row_fd()
    fd = pd.concat([military_fd, row_fd], axis=0, ignore_index=True)
    fd = merge_df(fd, usa_consumer_fd, how='outer', on='industry', keep_merge=False)
    fd = merge_df(fd, chn_consumer_fd, how='outer', on='industry', keep_merge=False)
    fd_matrix = merge_df(fd_matrix, fd, how='left', on='industry', keep_merge=False)
    for agent in ['consumer', 'military']:
        for cty in ['chn', 'usa', 'row']:
            fd_matrix[f'share_{agent}_{cty}'] = fd_matrix[f'share_{agent}_{cty}'].fillna(0.0)
    fd_matrix.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4.csv', index=False)


def construct_attrib():
    attrib = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4.csv')
    attrib = attrib[['industry']]
    attrib['country'] = attrib['industry'].map(lambda x: x[:3])
    attrib['type'] = attrib['industry'].map(lambda x: x[4:7])
    attrib['sector'] = attrib['industry'].map(lambda x: x[7:])

    bea_names = pd.read_excel(
        DATA_PATH / 'measurement' / 'accounting' / 'bea' / 'Use_SUT_Framework_2007_2012_DET.xlsx',
        sheet_name='2012', skiprows=5, skipfooter=5, dtype=str
    )
    bea_names = bea_names[['Code', 'Commodity Description']]
    bea_names.columns = ['sector', 'bea_name']
    bea_names['sector'] = bea_names['sector'].map(lambda x: x[:4])
    bea_names = bea_names.groupby('sector').first().reset_index()
    attrib = merge_df(attrib, bea_names, how='left', on='sector', keep_merge=False)

    naics_names = pd.read_excel(DATA_PATH / 'crosswalks' / 'census' / '6-digit_2012_Codes.xls', skiprows=1, dtype=str)
    naics_names.columns = ['sector', 'naics_name']
    naics_names['sector'] = naics_names['sector'].map(lambda x: x[:4])
    naics_names = naics_names.groupby('sector').first().reset_index()
    attrib = merge_df(attrib, naics_names, how='left', on='sector', keep_merge=False)

    other_names = pd.DataFrame([
        ['000000', 'Rest of the world'],
        ['999990', 'Unclassified'],
        ['S00101', 'Federal electric utilities'],
        ['S00201', 'State and local government passenger transit'],
        ['S00202', 'State and local government electric utilities'],
        ['271000', 'Oils petroleum, bituminous, distillates, except crude'],
    ], columns=['sector', 'other_name'])
    other_names['sector'] = other_names['sector'].map(lambda x: x[:4])
    other_names = other_names.groupby('sector').first().reset_index()
    attrib = merge_df(attrib, other_names, how='left', on='sector', keep_merge=False)

    hscode = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'H4_codes.csv', dtype=str)
    attrib = merge_df(attrib, hscode, how='left', on='sector', keep_merge=False)
    attrib.loc[attrib['type'].isin(['EXP', 'IMP']), 'sector_name'] \
        = attrib.loc[attrib['type'].isin(['EXP', 'IMP']), 'hscode_name']
    attrib.loc[attrib['type'].isin(['AGG', 'DOM', 'MPT']), 'sector_name'] \
        = attrib.loc[attrib['type'].isin(['AGG', 'DOM', 'MPT']), 'naics_name']
    for col in ['bea_name', 'other_name', 'naics_name']:
        attrib.loc[attrib['sector_name'].isna(), 'sector_name'] = attrib.loc[attrib['sector_name'].isna(), col]
    attrib = attrib[['industry', 'country', 'type', 'sector', 'sector_name']]

    dualuse = pd.read_csv(OUTPUT_PATH / 'motivation' / 'dualuse_masterfile_hs4_4digit.csv', dtype=str)
    cols = [col for col in dualuse.columns if col == 'dualuse_2018' or col == 'military']
    dualuse['dualuse_outcome'] = dualuse[cols].apply(lambda row: max(row), axis=1)
    dualuse = dualuse[['sector', 'dualuse_outcome', 'gta_export_usa_post22']]

    attrib = merge_df(attrib, dualuse, how='left', on='sector', keep_merge=False)
    attrib.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'attrib_naics4.csv', index=False)


def construct_elast():
    elast = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4.csv')
    elast = elast[['industry']]
    elast['country'] = elast['industry'].map(lambda x: x[:3])
    elast['type'] = elast['industry'].map(lambda x: x[4:7])
    elast['sector'] = elast['industry'].map(lambda x: x[7:])
    elast['sigma'] = 1.0
    elast.loc[elast['type'] == 'IMP', 'sigma'] = 2.53
    elast.loc[elast['type'] == 'MPT', 'sigma'] = 1.53
    elast.loc[elast['type'] == 'AGG', 'sigma'] = 1.19
    elast.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'elast_naics4.csv', index=False)


def construct_hetero_elast():
    elast = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4.csv')
    elast = elast[['industry']]
    elast['country'] = elast['industry'].map(lambda x: x[:3])
    elast['type'] = elast['industry'].map(lambda x: x[4:7])
    elast['sector'] = elast['industry'].map(lambda x: x[7:])
    elast['sigma'] = 1.0
    elast.loc[elast['type'] == 'IMP', 'sigma'] = 2.53
    elast.loc[elast['type'] == 'MPT', 'sigma'] = 1.53
    elast.loc[elast['type'] == 'AGG', 'sigma'] = 1.19

    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'hs12_elast_4digit.csv', dtype={'hs4': 'str'})
    df = df[['hs4', 'sigma_sdb']]
    df.columns = ['sector', 'sigma_wgt']

    elast = merge_df(elast, df, left_on='sector', right_on='sector', how='left', keep_merge=False)
    elast.loc[~elast['sigma_wgt'].isna() & (elast['type'] == 'IMP'), 'sigma'] =\
        elast.loc[~elast['sigma_wgt'].isna() & (elast['type'] == 'IMP'), 'sigma_wgt']
    elast = elast[['industry', 'country', 'type', 'sector', 'sigma']]
    elast.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'elast_hetero_naics4.csv', index=False)


def construct_price_indices():
    # construct us price index
    file_list = sorted(os.listdir(DATA_PATH / 'calibration' / 'prices'))
    file_list = [f for f in file_list if f.startswith('pc.data.')]

    fd_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4.csv')
    fd_matrix = fd_matrix.loc[
        fd_matrix['industry'].map(lambda x: x.startswith('USA_AGG'))
    ][['industry', 'share_consumer_usa']]
    naics_list = list(fd_matrix['industry'].map(lambda x: x[-4:]))

    series = pd.read_csv(DATA_PATH / 'calibration' / 'prices' / 'pc.series.txt', sep = '\t')
    series.columns = [col.replace(' ', '') for col in series.columns]
    series = series[['series_id', 'industry_code', 'product_code']]
    df_list = []
    for f in file_list:
        df_list = df_list + [pd.read_csv(DATA_PATH / 'calibration' / 'prices' / f, sep = '\t')]
    df = pd.concat(df_list, ignore_index=True)
    df = df[df['period'] == 'M13'].copy()
    df.columns = [col.replace(' ', '') for col in df.columns]
    df = merge_df(df, series, on='series_id', how='left', keep_merge=False)
    df = df.loc[df['year'] == 2018].copy()
    df = df.loc[df['industry_code'] == df['product_code']].copy()
    df = df.groupby('industry_code').first().reset_index()
    df_industry = df.loc[df['industry_code'].map(lambda x: x.endswith('--') and not x.endswith('---'))].copy()
    df_industry = df_industry[['industry_code', 'value']]
    df_industry['industry_code'] = df_industry['industry_code'].map(lambda x: x.replace('--', ''))
    df_industry = df_industry[['industry_code', 'value']]
    list_industry = [x.replace('--', '') for x in df_industry['industry_code'].unique()]

    df = df.loc[df['industry_code'].map(lambda x: not x[:4] in list_industry)].copy()
    df_threedig = df.loc[df['industry_code'].map(lambda x: x.endswith('---'))].copy()
    df_threedig['industry_list'] = df_threedig['industry_code'].map(lambda x: [n for n in naics_list if n[:3] == x[:3]])
    df_threedig = df_threedig.explode('industry_list')
    df_threedig = df_threedig.loc[~df_threedig['industry_list'].isna()].copy()
    df_threedig = df_threedig.loc[df_threedig['industry_list'].map(lambda x: not x in list_industry)].copy()
    df_threedig = df_threedig[['industry_list', 'value']]
    df_threedig.columns = ['industry_code', 'value']
    list_threedig = list(df_threedig['industry_code'].unique())

    df = df.loc[df['industry_code'].map(
        lambda x: (not x[:4] in (list_industry + list_threedig)) and (not x.endswith('---'))
    )].copy()
    df = df.loc[df['industry_code'].map(
        lambda x: not x in [
            '23811X', '23816X', '449110', '449121', '531120', '531130', '531311', '531312',
            '541810', '561510', '561612', '561720', '621991', '623210', '713910', '713940'])
    ].copy()
    df['industry_code'] = df['industry_code'].map(lambda x: x[:4])
    df = df.groupby('industry_code')['value'].mean().reset_index()
    df = pd.concat([df, df_industry, df_threedig], axis=0, ignore_index=True)
    df.sort_values('industry_code', inplace=True)

    industry_list = list(df['industry_code'].unique())
    df_twodigit = df.copy()
    df_twodigit['industry_code'] = df_twodigit['industry_code'].map(lambda x: x[:2])
    df_twodigit = df_twodigit.groupby('industry_code')['value'].mean().reset_index()
    df_twodigit['industry_list'] = df_twodigit['industry_code'].map(lambda x: [n for n in naics_list if n[:2] == x])
    df_twodigit = df_twodigit.explode('industry_list')
    df_twodigit = df_twodigit.loc[~df_twodigit['industry_list'].isna()]
    df_twodigit = df_twodigit.loc[df_twodigit['industry_list'].map(lambda x: x not in industry_list)]
    df_twodigit['industry_code'] = df_twodigit['industry_list'].copy()
    df_twodigit.drop('industry_list', axis=1, inplace=True)
    df = pd.concat([df, df_twodigit], axis=0, ignore_index=True)

    industry_list = list(df['industry_code'].unique())
    df_onedigit = df.copy()
    df_onedigit['industry_code'] = df_onedigit['industry_code'].map(lambda x: x[:1])
    df_onedigit = df_onedigit.groupby('industry_code')['value'].mean().reset_index()
    df_onedigit['industry_list'] = df_onedigit['industry_code'].map(lambda x: [n for n in naics_list if n[:1] == x])
    df_onedigit = df_onedigit.explode('industry_list')
    df_onedigit = df_onedigit.loc[~df_onedigit['industry_list'].isna()]
    df_onedigit = df_onedigit.loc[df_onedigit['industry_list'].map(lambda x: x not in industry_list)]
    df_onedigit['industry_code'] = df_onedigit['industry_list'].copy()
    df_onedigit.drop('industry_list', axis=1, inplace=True)
    df = pd.concat([df, df_onedigit], axis=0, ignore_index=True)
    df['industry'] = 'USA_AGG' + df['industry_code']
    df = df[['industry', 'value']]

    fd_matrix = merge_df(fd_matrix, df, on='industry', how='inner', keep_merge=True)
    fd_matrix['share_consumer_usa'] = fd_matrix['share_consumer_usa'] / fd_matrix['share_consumer_usa'].sum()
    fd_matrix['value'] = fd_matrix['value'] * fd_matrix['share_consumer_usa']
    P_C_USA = fd_matrix['value'].sum()

    fd_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4.csv')
    fd_matrix = fd_matrix.loc[
        fd_matrix['industry'].map(lambda x: x.startswith('USA_DOM'))
    ][['industry', 'share_military_usa']]
    df['industry'] = df['industry'].map(lambda x: x.replace('USA_AGG', 'USA_DOM'))
    fd_matrix = merge_df(fd_matrix, df, on='industry', how='inner', keep_merge=True)
    fd_matrix['share_military_usa'] = fd_matrix['share_military_usa'] / fd_matrix['share_military_usa'].sum()
    fd_matrix['value'] = fd_matrix['value'] * fd_matrix['share_military_usa']
    P_M_USA = fd_matrix['value'].sum()
    P_M_USA = P_M_USA / P_C_USA
    P_C_USA = 1.0

    ppp_wb = pd.read_csv(
        DATA_PATH / 'calibration' / 'prices' / 'API_PA.NUS.PPP_DS2_en_csv_v2_1057047'
            / 'API_PA.NUS.PPP_DS2_en_csv_v2_1057047.csv', skiprows=4
    )
    ppp_chn = ppp_wb.loc[ppp_wb['Country Code'] == 'CHN', '2018'].values[0]
    P_C_CHN = 1 / ppp_chn

    df_chn = pd.read_csv(
        OUTPUT_PATH / 'calibration' / 'iotables' / 'share_rev_by_ind_chn.csv', dtype={'naics12': 'str'})
    df_chn = df_chn[df_chn['year'] == 2018].copy()
    df_chn = df_chn[['industrycode2012', 'share_rev.A']]
    ind_chn = pd.read_excel(
        EXTERNAL_PATH / 'data' / 'calibration' / 'csmar' / 'STK_INDUSTRYCLASS' / 'STK_INDUSTRYCLASS.xlsx')
    ind_chn = ind_chn.groupby(['IndustryCode', 'IndustryName']).first().reset_index()[['IndustryCode', 'IndustryName']]
    df_chn = merge_df(
        df_chn, ind_chn, left_on='industrycode2012', right_on='IndustryCode', how='left', keep_merge=False)
    d = {
        '综合': '总指数',
        '非金属矿物制品业': '非金属矿物制品业',
        '零售业': '总指数',
        '计算机、通信和其他电子设备制造业': '计算机、通信和其他电子设备制造业',
        '电气机械及器材制造业': '电气机械和器材制造业',
        '航空运输业': '总指数',
        '专用设备制造业': '专用设备制造业',
        '印刷业': '印刷和记录媒介复制业',
        '化学原料及化学制品制造业': '化学原料和化学制品制造业',
        '房地产业': '总指数',
        '汽车制造业': '汽车制造业',
        '造纸及纸制品业': '造纸和纸制品业',
        '化学纤维制造业': '化学纤维制造业',
        '橡胶和塑料制品业': '橡胶和塑料制品业',
        '通用设备制造业': '通用设备制造业',
        '软件和信息技术服务业': '总指数',
        '有色金属冶炼及压延加工业': '有色金属冶炼和压延加工业',
        '文教体育用品制造业': '文教、工美、体育和娱乐用品',
        '铁路、船舶、航空航天和其它运输设备制造业': '铁路、船舶、航空航天和其他运输',
        '造纸及纸制品业': '造纸和纸制品业',
        '黑色金属冶炼及压延加工业': '黑色金属冶炼和压延加工业',
        '金属制品业': '金属制品业',
        '纺织业': '纺织业',
        '仪器仪表制造业': '仪器仪表制造业',
        '纺织服装、服饰业': '纺织服装、服饰业',
        '互联网和相关服务': '总指数',
        '建筑装饰和其他建筑业': '总指数',
        '专业技术服务业': '总指数',
        '道路运输业': '总指数',
        '开采辅助活动': '开采辅助活动',
        '商务服务业': '总指数',
        '土木工程建筑业': '总指数',
        '皮革、毛皮、羽毛及其制品和制鞋业': '皮革、毛皮、羽毛及其制品和制鞋业',
        '其他制造业': '其他制造业',
        '石油加工及炼焦业': '石油加工、炼焦和核燃料加工业',
        '有色金属矿采选业': '有色金属矿采选业',
        '其他金融业': '总指数',
        '农业': '总指数',
        '电信、广播电视和卫星传输服务': '总指数',
        '科技推广和应用服务业': '总指数'
    }
    df_chn['industry_name'] = df_chn['IndustryName'].map(lambda x: d[x])
    chn_prices = pd.read_excel(DATA_PATH / 'calibration' / 'prices' / 'ppi_chn' / 'ppi_14-17.xls', skiprows=3)
    chn_prices.columns = ['industry', '2014', '2015', '2016', '2017']
    chn_prices = chn_prices.loc[~chn_prices['2017'].isna()].copy()
    chn_prices = chn_prices[['industry', '2017']]
    df_chn = merge_df(df_chn, chn_prices, left_on='industry_name', right_on='industry', how='left', keep_merge=False)
    df_chn['index'] = df_chn['2017'] * df_chn['share_rev.A']
    P_mil = df_chn['index'].sum()
    ratio_mil = P_mil / chn_prices.loc[chn_prices['industry'] == '总指数', '2017'].values[0]
    P_M_CHN = P_C_CHN * ratio_mil

    prices = pd.DataFrame({'P_CHN': [P_C_CHN, P_M_CHN], 'P_USA': [P_C_USA, P_M_USA], 'P_ROW': [1.0, 1.0]})
    prices.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'prices.csv', index=False)

    # military stocks: DoD total assets + ally expenditures
    mil = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'mil.csv')
    df_ally = pd.read_csv(OUTPUT_PATH / 'calibration' / 'tullock' / 'allies_2018.csv')
    df_prices = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'prices.csv')
    P_M_CHN = df_prices['P_CHN'][1]
    P_M_USA = df_prices['P_USA'][1]

    stock_usa = 2.7 * 1e3 / P_M_USA
    ratio_stock_usa = mil['USA'].values[0] / (stock_usa + mil['USA'].values[0])
    ratio_stock_chn = ratio_stock_usa
    stock_chn = mil['CHN'].values[0] / P_M_CHN * (1 / ratio_stock_chn - 1)
    ally_stock_usa = stock_usa + df_ally['USA'][0] / 1000 / P_M_USA
    ally_stock_chn = stock_chn + df_ally['CHN'][0] / 1000 / P_M_CHN
    total_stock_usa = stock_usa + df_ally['USA'][0] / 1000 / P_M_USA * (1 / ratio_stock_usa - 1)
    total_stock_chn = stock_chn + df_ally['CHN'][0] / 1000 / P_M_CHN * (1 / ratio_stock_chn - 1)
    stock = pd.DataFrame({'CHN': [total_stock_chn], 'USA': [total_stock_usa], 'ROW': [0.0]})
    stock.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'stock.csv', index=False)
    stock_variants = pd.DataFrame({
        'variant': ['baseline', '+ stockpiling', '+ allies', "+ allies' stockpiles"],
        'CHN': [0.0, stock_chn, ally_stock_chn, total_stock_chn],
        'USA': [0.0, stock_usa, ally_stock_usa, total_stock_usa]
    })
    stock_variants.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'stock_variants.csv', index=False)


def main():
    construct_final_matrix()
    construct_attrib()
    construct_elast()
    construct_hetero_elast()
    construct_price_indices()


if __name__ == '__main__':
    main()

