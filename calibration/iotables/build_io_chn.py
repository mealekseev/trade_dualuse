""" Build USA closed-economy centrality """

import numpy as np
import pandas as pd
import itertools

from settings import DATA_PATH, OUTPUT_PATH
from utilities import merge_df


def cw_chn_sender(weight=0.5, trade_wgt=True):
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'chinaiosector18_naics12.csv', dtype={'naics12': 'str', 'w': 'float'})
    cw['wgt'] = cw['w'].astype(float)
    cw = cw[['iosector_code', 'naics12', 'wgt']]

    if trade_wgt:
        cw_trade = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'aux_trade_xp.csv')
        cw_trade.loc[cw_trade['industry_from'] == 'CHN_DOM312113', 'value'] = 0.0
        cw_trade = cw_trade[cw_trade['industry_from'].map(lambda x: x[:3] == 'CHN')].copy()
        cw_trade = cw_trade.groupby('industry_from')['value'].sum().reset_index()
        cw_trade['naics12'] = cw_trade['industry_from'].map(lambda x: x[7:])
        cw_trade = cw_trade[['naics12', 'value']]
    else:
        cw_trade = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'fd_consumer_usa.csv')
        cw_trade = cw_trade[['industry', 'share_consumer_data']]
        cw_trade.columns = ['industry', 'share_consumer_usa']
        cw_trade['naics12'] = cw_trade['industry'].map(lambda x: x[7:])
        cw_trade['value'] = cw_trade['share_consumer_usa']
        cw_trade = cw_trade[['naics12', 'value']]

    cw = merge_df(cw, cw_trade, on='naics12', how='left', keep_merge=False)
    cw.loc[cw['value'].isna(), 'value'] = 0.0
    cw['value_wgt'] = cw['value'] * 1 / cw.groupby('naics12')['iosector_code'].transform('count')

    cw['wgt_alt'] = cw['value_wgt'] / cw.groupby('iosector_code')['value_wgt'].transform('sum')
    cw.loc[cw['wgt_alt'].isna(), 'wgt_alt'] = cw.loc[cw['wgt_alt'].isna(), 'wgt']

    cw.loc[cw['iosector_code'] == '44098', 'wgt_alt'] = cw.loc[cw['iosector_code'] == '44098', 'wgt'].copy()
    cw['wgt'] = weight * cw['wgt'] + (1 - weight) * cw['wgt_alt']
    cw = cw[['iosector_code', 'naics12', 'wgt']]
    cw = cw.groupby(['iosector_code', 'naics12'])['wgt'].sum().reset_index()
    assert max(abs(cw.groupby('iosector_code')['wgt'].sum() - 1.0)) < 1e-14
    return cw


def cw_chn_receiver(weight=0.5, rmb_usd_exchange=6.63):
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'chinaiosector18_naics12.csv', dtype={'naics12': 'str', 'w': 'float'})
    cw['wgt'] = cw['w'].astype(float)
    cw = cw[['iosector_code', 'naics12', 'wgt']]
    cw = cw.groupby(['iosector_code', 'naics12'])['wgt'].sum().reset_index()

    naics_chn = pd.read_csv(
        OUTPUT_PATH / 'calibration' / 'iotables' / 'sector18_chn.csv', dtype={'iosector_code': 'str'})
    naics_chn['sales'] = (naics_chn['gross_output'] - naics_chn['export']) * 10000 / rmb_usd_exchange

    cw = merge_df(cw, naics_chn, left_on='iosector_code', right_on='iosector', how='left', keep_merge=False)
    cw['sales_wgt'] = cw['sales'] / cw.groupby('iosector')['iosector_code'].transform('count')
    cw['wgt'] = 1 / cw.groupby('naics12')['iosector'].transform('count')
    cw['wgt_alt'] = cw['sales_wgt'] / cw.groupby('naics12')['sales_wgt'].transform('sum')
    cw.loc[cw['wgt_alt'].isna(), 'wgt_alt'] = cw.loc[cw['wgt_alt'].isna(), 'wgt']

    cw['wgt'] = weight * cw['wgt'] + (1 - weight) * cw['wgt_alt']
    cw = cw.groupby(['naics12', 'iosector_code'])[['wgt']].sum().reset_index()
    assert max(abs(cw.groupby('naics12')['wgt'].sum() - 1.0)) < 1e-14
    return cw


def get_military_fd():
    # select names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].unique().tolist()
    fd_list = ['CHN_DOM' + i for i in naics12_list]
    fd_matrix = pd.DataFrame(fd_list, columns=['industry'])

    # build crosswalk
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)['naics12'].unique().tolist()
    gbt_isic = pd.read_csv(
        OUTPUT_PATH / 'crosswalks' / 'gbt2017_isic4.csv',
        dtype={'gbt17': 'str', 'isic4': 'str', 'gbt17_2': 'str', 'gbt17_3': 'str', 'gbt17_4': 'str'}
    )
    naics12_isic = pd.read_csv(
        OUTPUT_PATH / 'crosswalks' / 'naics12_isic4.csv', dtype={'isic': 'str', 'naics12': 'str'}
    )
    naics12_isic.sort_values('naics12', ascending=True, inplace=True, ignore_index=True)
    naics12_isic = naics12_isic[['isic4', 'naics12', 'w_isic']]
    gbt_naics = merge_df(gbt_isic, naics12_isic, on='isic4', how='left')
    gbt_naics = gbt_naics[['gbt17_2', 'naics12']]
    gbt_naics.loc[gbt_naics['gbt17_2'].map(int) > 90, 'gbt17_2'] = '99'
    gbt_naics.loc[gbt_naics['gbt17_2'].map(int) > 90, 'naics12'] = '92'
    gbt_naics['naics12_list'] = gbt_naics['naics12'].map(lambda x: [n for n in naics12_list if n.startswith(x)])
    gbt_naics = gbt_naics.explode(column='naics12_list')
    gbt_naics = gbt_naics.groupby(['gbt17_2', 'naics12_list'])['naics12'].first().reset_index()
    gbt_naics = gbt_naics[['gbt17_2', 'naics12_list']]
    gbt_naics.columns = ['gbt17_2', 'naics12']
    gbt_naics['wgt'] = 1 / gbt_naics.groupby('naics12')['gbt17_2'].transform('count')

    # weight by the US military procurement
    fd_military = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'fd_military_usa.csv')
    fd_military = fd_military[['industry', 'share_military_combined']]
    fd_military['naics12'] = fd_military['industry'].map(lambda x: x[-6:])
    gbt_naics = merge_df(gbt_naics, fd_military, on='naics12', how='left', keep_merge=False)
    gbt_naics['share_military_combined'] = gbt_naics['share_military_combined'] * gbt_naics['wgt']
    gbt_naics['share_value'] = (gbt_naics['share_military_combined']
                                / gbt_naics.groupby(['gbt17_2'])['share_military_combined'].transform('sum'))
    gbt_naics = gbt_naics[['gbt17_2', 'naics12', 'share_value']]

    # assemble procurement
    df_chn = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'share_rev_by_ind_chn.csv', dtype={'naics12': 'str'})
    df_chn = df_chn[(df_chn['year'] > 2010) & (df_chn['year'] < 2020)]
    cpi = pd.read_csv(DATA_PATH / 'calibration' / 'chinaio' / 'CHNCPIALLAINMEI.csv')
    cpi.columns = ['year', 'index']
    cpi['year'] = cpi['year'].map(lambda x: int(x[:4]))
    cpi['index'] = 100 / cpi['index']
    df_chn = merge_df(df_chn, cpi, how='left', on='year')
    df_chn['tot_rev.A'] = df_chn['tot_rev.A'] * df_chn['index']
    df_chn['tot_rev.B'] = df_chn['tot_rev.B'] * df_chn['index']
    df_chn = df_chn.groupby('industrycode2012')[['tot_rev.A', 'tot_rev.B']].sum().reset_index()
    df_chn['industrycode2012'] = df_chn['industrycode2012'].map(lambda x: x[-2:])

    # crosswalk into naics
    df_chn = merge_df(df_chn, gbt_naics, left_on='industrycode2012', right_on='gbt17_2', how='left', keep_merge=False)
    df_chn['value'] = df_chn['tot_rev.A'] * df_chn['share_value']
    df_chn['share_value'] = df_chn['value'] / df_chn['value'].sum()
    df_chn = df_chn[['naics12', 'share_value']]
    df_chn['industry'] = 'CHN_DOM' + df_chn['naics12']
    df_chn = df_chn.groupby('industry')['share_value'].sum().reset_index()
    df_chn.columns = ['industry', 'share_military_chn']

    fd_matrix = merge_df(fd_matrix, df_chn, on='industry', how='outer', keep_merge=False)
    fd_matrix['share_military_chn'] = fd_matrix['share_military_chn'].fillna(0.0)
    return fd_matrix


def get_fd_chn_military():
    df = get_military_fd()
    df.columns = ['industry', 'share_military_chn']
    df.sort_values('industry', inplace=True)
    df.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_military_chn.csv', index=False)


def get_chn_consumer_fd(weight=0.5):
    # names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].map(lambda x: x[:6]).unique().tolist()
    fd_list = ['CHN_AGG' + i for i in naics12_list]
    fd_matrix = pd.DataFrame(fd_list, columns=['industry'])

    cw = cw_chn_sender(weight=weight, trade_wgt=False)
    fd_chn = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'sector18_chn.csv', dtype={'iosector': 'str'})
    fd_chn = merge_df(fd_chn, cw, how='left', left_on='iosector', right_on='iosector_code', keep_merge=False)
    fd_chn['value'] = fd_chn['total_hh_consumption'] * fd_chn['wgt']

    fd_chn = fd_chn.groupby('naics12')['value'].sum().reset_index()
    fd_chn['value_sum'] = fd_chn['value'].sum()
    fd_chn['share_consumer'] = fd_chn['value'] / fd_chn['value_sum']
    fd_chn['industry'] = 'CHN_AGG' + fd_chn['naics12']
    fd_chn = fd_chn[['industry', 'share_consumer']]
    fd_chn.columns = ['industry', 'share_consumer_chn']

    fd_matrix = merge_df(fd_matrix, fd_chn, on='industry', how='left', keep_merge=False)
    fd_matrix['share_consumer_chn'] = fd_matrix['share_consumer_chn'].fillna(0.0)
    return fd_matrix


def get_fd_chn_consumer_variants():
    df1 = get_chn_consumer_fd(weight=0.0)
    df1.columns = ['industry', 'share_consumer_data']
    df2 = get_chn_consumer_fd(weight=0.5)
    df2.columns = ['industry', 'share_consumer_mixed']
    df3 = get_chn_consumer_fd(weight=1.0)
    df3.columns = ['industry', 'share_consumer_equal']
    df = merge_df(df1, df2, on='industry', how='outer', keep_merge=False)
    df = merge_df(df, df3, on='industry', how='outer', keep_merge=False)
    print(df['share_consumer_data'].corr(df['share_consumer_mixed']))
    print(df['share_consumer_data'].corr(df['share_consumer_equal']))
    df.sort_values('industry', inplace=True)
    df.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_consumer_chn.csv', index=False)


def get_chn_dom_agg(weight=0.5):
    # select names
    naics12_list = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype=str)[['naics12']]
    naics12_list = naics12_list['naics12'].map(lambda x: x[:6]).unique().tolist()
    io_list = list(itertools.product(['CHN_DOM' + i for i in naics12_list], ['CHN_AGG' + i for i in naics12_list]))
    io_matrix = pd.DataFrame(io_list, columns=['industry_to', 'industry_from'])

    df = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'iofromto18_chn.csv', dtype={'iofrom': 'str', 'ioto': 'str'})
    df['share_value'] = df['v'] / df.groupby('ioto')['v'].transform('sum')
    va = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'sector18_chn.csv', dtype={'iosector': 'str'})
    va['share_va'] = va['va'] / va['gross_output']
    va = va[['iosector', 'share_va']]
    df = merge_df(df, va, how='left', left_on='ioto', right_on='iosector', keep_merge=False)
    df['share_value'] = df['share_value'] * (1 - df['share_va'])
    df = df[['iofrom', 'ioto', 'share_value']]

    cw_receiver = cw_chn_receiver(weight=weight)
    df = merge_df(df, cw_receiver, how='left', left_on='ioto', right_on='iosector_code', keep_merge=False)
    df['share_value'] = df['share_value'] * df['wgt']
    df = df.groupby(['naics12', 'iofrom'])['share_value'].sum().reset_index()
    df.columns = ['industry_to', 'industry_from', 'share_value']

    cw_sender = cw_chn_sender(weight=weight)
    df = merge_df(df, cw_sender, how='left', left_on='industry_from', right_on='iosector_code', keep_merge=False)
    df['share_value'] = df['share_value'] * df['wgt']
    df = df.groupby(['industry_to', 'naics12'])['share_value'].sum().reset_index()
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


def get_io_chn_variants():
    io_matrix1 = get_chn_dom_agg(weight=0.0)
    io_matrix1.columns = ['industry_to', 'industry_from', 'share_value_data']

    io_matrix2 = get_chn_dom_agg(weight=0.5)
    io_matrix2.columns = ['industry_to', 'industry_from', 'share_value_mixed']

    io_matrix3 = get_chn_dom_agg(weight=1.0)
    io_matrix3.columns = ['industry_to', 'industry_from', 'share_value_equal']

    io_matrix = merge_df(io_matrix1, io_matrix2, on=['industry_to', 'industry_from'], how='outer', keep_merge=False)
    io_matrix = merge_df(io_matrix, io_matrix3, on=['industry_to', 'industry_from'], how='outer', keep_merge=False)
    io_matrix.sort_values(['industry_to', 'industry_from'], inplace=True)
    io_matrix.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_chn.csv', index=False)


def main():
    get_fd_chn_military()
    get_fd_chn_consumer_variants()
    get_io_chn_variants()


if __name__ == '__main__':
    main()

