""" Build USA-CHN input-output network with reshipping """

import numpy as np
import pandas as pd
import itertools

from settings import DATA_PATH, OUTPUT_PATH, output
from utilities import merge_df


def amend_io_matrix(tau=2.0, sigma=8.0, baseline=False):
    io_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4.csv')
    io_matrix = io_matrix[['industry_to', 'industry_from', 'share_value']]
    io_matrix['industry_to'] = io_matrix['industry_to'].map(
        lambda x: x[:4] + 'RCV' + x[7:] if x[4:7] == 'IMP' and x[:3] != 'CHN' else x
    )
    io_matrix['industry_from'] = io_matrix['industry_from'].map(
        lambda x: x[:4] + 'RCV' + x[7:] if x[4:7] == 'IMP' and x[:3] != 'CHN' else x)
    io_subset = io_matrix[
        (io_matrix['industry_to'].map(lambda x: x[4:7] == 'RCV'))
        & (io_matrix['industry_from'].map(lambda x: x[:3] == 'CHN'))
    ].copy()
    io_matrix = io_matrix[
        (io_matrix['industry_to'].map(lambda x: x[4:7] != 'RCV'))
        | (
            (io_matrix['industry_to'].map(lambda x: x[4:7] == 'RCV'))
            & (io_matrix['industry_from'].map(lambda x: x[:3] != 'CHN'))
        )
    ].copy()

    # first convert subset of countries
    io_subset['industry'] = io_subset['industry_to'].map(lambda x: x[4:])
    io_subset['country'] = io_subset['industry_from'].map(lambda x: x[:3])
    io_cty = io_subset.groupby(['industry_to', 'country'])['share_value'].sum().reset_index()
    io_cty['industry_from'] = (
        io_cty['industry_to'].map(
            lambda x: x[:3]) + '_' + io_cty['country'] + io_cty['industry_to'].map(lambda x: x[7:])
    )
    io_cty = io_cty[['industry_to', 'industry_from', 'share_value']]

    io_route = io_cty.copy()
    io_route = io_route.groupby(['industry_from'])['share_value'].first().reset_index()
    io_route['industry_to'] = io_route['industry_from']
    io_route['industry_from'] = io_route['industry_from'].map(lambda x: 'CHN_IMP' + x[4:])
    io_route2 = io_route.copy()
    io_route2['industry_from'] = io_route2['industry_from'].map(lambda x: x.replace('CHN_', 'USA_'))
    io_route3 = io_route.copy()
    io_route3['industry_from'] = io_route3['industry_from'].map(lambda x: x.replace('CHN_', 'ROW_'))
    io_route = pd.concat([io_route, io_route2, io_route3], ignore_index=True)
    io_route = io_route[['industry_to', 'industry_from', 'share_value']]
    io_route['share_value'] = 1.0
    io_route = io_route[io_route['industry_from'].map(lambda x: x[:3] != x[7:10])].copy()
    if baseline:
        io_route.loc[
            io_route['industry_to'].map(lambda x: x[:3]) == io_route['industry_from'].map(lambda x: x[:3]),
            'share_value'
        ] = 1.0
        io_route.loc[
            io_route['industry_to'].map(lambda x: x[:3]) != io_route['industry_from'].map(lambda x: x[:3]),
            'share_value'
        ] = 0.0
    else:
        io_route.loc[
            io_route['industry_to'].map(lambda x: x[:3]) == io_route['industry_from'].map(lambda x: x[:3]),
            'share_value'
        ] = 1 / (1 + tau ** (1 - sigma))
        io_route.loc[
            io_route['industry_to'].map(lambda x: x[:3]) != io_route['industry_from'].map(lambda x: x[:3]),
            'share_value'
        ] = tau ** (1 - sigma) / (1 + tau ** (1 - sigma))

    io_subset['share_value_sum'] = io_subset.groupby(['industry_to', 'country'])['share_value'].transform('sum')
    io_subset['share_value_dom'] = io_subset['share_value'] / io_subset['share_value_sum']
    io_subset = io_subset.groupby(['industry_from', 'country', 'industry'])['share_value_dom'].mean().reset_index()
    io_subset['connector'] = 'IMP' + io_subset['country'] + io_subset['industry'].map(lambda x: x[3:])
    route_rcv = io_route.groupby('industry_from').first().reset_index()[['industry_from']]
    route_rcv.columns = ['industry_to']
    route_rcv['connector'] = route_rcv['industry_to'].map(lambda x: x[4:])
    io_subset = merge_df(route_rcv, io_subset, how='left', on='connector')
    io_subset = io_subset[['industry_to', 'industry_from', 'share_value_dom']]
    io_subset.columns = ['industry_to', 'industry_from', 'share_value']

    io_matrix = pd.concat([io_matrix, io_cty, io_route, io_subset], ignore_index=True)
    io_matrix[io_matrix['industry_to'].map(lambda x: x[4:7] == 'IMP')].groupby(
        'industry_to')['share_value'].sum().value_counts()
    io_matrix[io_matrix['industry_to'].map(lambda x: x[4:7] == 'RCV')].groupby(
        'industry_to')['share_value'].sum().value_counts()
    io_matrix[io_matrix['industry_to'].map(lambda x: x[4:7] == 'CHN')].groupby(
        'industry_to')['share_value'].sum().value_counts()

    if baseline:
        io_matrix.to_csv(
            OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_reshipping_baseline_chn.csv', index=False)
    else:
        io_matrix.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_reshipping_chn.csv', index=False)


def amend_final_demand():
    fd_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_reshipping_chn.csv')
    industry_list = list(fd_matrix['industry_to'].unique())
    fd_matrix = pd.DataFrame(industry_list, columns=['industry'])

    df = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4.csv')
    df['industry'] = df['industry'].map(lambda x: x[0:4] + 'RCV' + x[7:] if x[4:7] == 'IMP' else x)

    fd_matrix = merge_df(fd_matrix, df, how='left', on='industry', keep_merge=False)
    cols = [col for col in fd_matrix.columns if col.startswith('share_')]
    for col in cols:
        fd_matrix[col] = fd_matrix[col].fillna(0.0)
    fd_matrix.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_reshipping_chn.csv', index=False)


def amend_attrib():
    io_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_matrix_naics4_reshipping_chn.csv')
    industry_list = list(io_matrix['industry_to'].unique())
    attrib = pd.DataFrame(industry_list, columns=['industry'])
    attrib['country'] = attrib['industry'].map(lambda x: x[:3])
    attrib['type'] = attrib['industry'].map(lambda x: x[4:7])
    attrib['sector'] = attrib['industry'].map(lambda x: x[7:])

    attrib_old = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'attrib_naics4.csv')
    attrib_imp = attrib_old[attrib_old['type'] == 'IMP'].copy()
    attrib_old['industry'] = attrib_old['industry'].map(lambda x: x[0:4] + 'RCV' + x[7:] if x[4:7] == 'IMP' else x)
    attrib_old['type'] = attrib_old['type'].map(lambda x: 'RCV' if x == 'IMP' else x)
    attrib_copy1 = attrib_old[attrib_old['type'] == 'RCV'].copy()
    attrib_copy2 = attrib_old[attrib_old['type'] == 'RCV'].copy()
    attrib_copy3 = attrib_old[attrib_old['type'] == 'RCV'].copy()
    attrib_copy1['industry'] = attrib_copy1['industry'].map(lambda x: x[0:4] + 'CHN' + x[7:] if x[4:7] == 'RCV' else x)
    attrib_copy1['type'] = 'CHN'
    attrib_copy1 = attrib_copy1[attrib_copy1['type'] != attrib_copy1['country']].copy()
    attrib_copy2['industry'] = attrib_copy2['industry'].map(lambda x: x[0:4] + 'USA' + x[7:] if x[4:7] == 'RCV' else x)
    attrib_copy2['type'] = 'USA'
    attrib_copy2 = attrib_copy2[attrib_copy2['type'] != attrib_copy2['country']].copy()
    attrib_copy3['industry'] = attrib_copy3['industry'].map(lambda x: x[0:4] + 'ROW' + x[7:] if x[4:7] == 'RCV' else x)
    attrib_copy3['type'] = 'ROW'
    attrib_copy3 = attrib_copy3[attrib_copy3['type'] != attrib_copy3['country']].copy()
    attrib_copy = pd.concat([attrib_imp, attrib_copy1, attrib_copy2, attrib_copy3], ignore_index=True)

    attrib_mpt = attrib_copy.copy()
    attrib_mpt['industry'] = attrib_mpt['industry'].map(lambda x: x[0:4] + 'IMP' + x[4:])
    attrib_mpt['type'] = 'IMP'
    attrib_mpt['sector'] = attrib_mpt['industry'].map(lambda x: x[7:10]) + attrib_mpt['sector']

    attrib_this = pd.concat([attrib_old, attrib_copy, attrib_mpt], ignore_index=True)

    attrib = merge_df(attrib, attrib_this, how='left', on=['industry', 'country', 'type', 'sector'], keep_merge=True)
    attrib.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'attrib_naics4_reshipping_chn.csv', index=False)


def amend_elast():
    elast = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_reshipping_chn.csv')
    elast = elast[['industry']]
    elast['country'] = elast['industry'].map(lambda x: x[:3])
    elast['type'] = elast['industry'].map(lambda x: x[4:7])
    elast['sector'] = elast['industry'].map(lambda x: x[7:])
    elast['sigma'] = 1.0
    elast.loc[elast['type'] == 'RCV', 'sigma'] = 2.53
    elast.loc[(elast['type'] == 'IMP') & (elast['country'] == 'CHN'), 'sigma'] = 2.53
    elast.loc[elast['type'] == 'MPT', 'sigma'] = 1.53
    elast.loc[elast['type'] == 'AGG', 'sigma'] = 1.19
    elast.loc[elast['type'].map(lambda x: x in ['CHN', 'USA', 'ROW']), 'sigma'] = 8.00
    elast.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'elast_naics4_reshipping_chn.csv', index=False)


def amend_het_elast():
    elast = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_matrix_naics4_reshipping_chn.csv')
    elast = elast[['industry']]
    elast['country'] = elast['industry'].map(lambda x: x[:3])
    elast['type'] = elast['industry'].map(lambda x: x[4:7])
    elast['sector'] = elast['industry'].map(lambda x: x[7:])
    elast['sigma'] = 1.0
    elast.loc[elast['type'] == 'RCV', 'sigma'] = 2.53
    elast.loc[(elast['type'] == 'IMP') & (elast['country'] == 'CHN'), 'sigma'] = 2.53
    elast.loc[elast['type'] == 'MPT', 'sigma'] = 1.53
    elast.loc[elast['type'] == 'AGG', 'sigma'] = 1.19
    elast.loc[elast['type'].map(lambda x: x in ['CHN', 'USA', 'ROW']), 'sigma'] = 8.00

    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'hs12_elast_4digit.csv', dtype={'hs4': 'str'})
    df['sigma'] = df['sigma_sdb']
    df = df[['hs4', 'sigma']]
    df.columns = ['sector', 'sigma_wgt']

    elast = merge_df(elast, df, left_on='sector', right_on='sector', how='left', keep_merge=False)
    elast.loc[~elast['sigma_wgt'].isna() & (elast['type'] == 'RCV'), 'sigma'] = \
        elast.loc[~elast['sigma_wgt'].isna() & (elast['type'] == 'RCV'), 'sigma_wgt']
    elast.loc[~elast['sigma_wgt'].isna() & (elast['type'] == 'IMP') & (elast['country'] == 'CHN'), 'sigma'] = \
        elast.loc[~elast['sigma_wgt'].isna() & (elast['type'] == 'IMP') & (elast['country'] == 'CHN'), 'sigma_wgt']
    elast = elast[['industry', 'country', 'type', 'sector', 'sigma']]
    elast.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'elast_hetero_naics4_reshipping_chn.csv', index=False)


def main():
    amend_io_matrix(tau=2.0, sigma=8.0)
    amend_io_matrix(tau=2.0, sigma=8.0, baseline=True)
    amend_final_demand()
    amend_attrib()
    amend_elast()
    amend_het_elast()


if __name__ == '__main__':
    main()

