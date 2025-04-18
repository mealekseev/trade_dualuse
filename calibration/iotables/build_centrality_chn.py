# Build USA centrality measures

import numpy as np
import pandas as pd

from settings import DATA_PATH, OUTPUT_PATH, output
from utilities import merge_df, divide_robust
import matplotlib.pyplot as plt


def filter_io_tables():
    mil = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'mil.csv')
    M = mil['CHN'].values[0]

    gdp = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'gdp.csv')
    C = gdp['CHN'].values[0]

    fd_consumer = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_consumer_chn.csv')
    fd_consumer['industry'] = fd_consumer['industry'].map(lambda x: x.replace('AGG', 'DOM'))
    industry_list = fd_consumer['industry'].to_list()
    s_C = fd_consumer['share_consumer_data'].values

    fd_military = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'fd_military_chn.csv')
    assert fd_military['industry'].to_list() == industry_list
    s_M = fd_military['share_military_chn'].values

    io_matrix = pd.read_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'io_chn.csv')
    io_matrix['industry_from'] = io_matrix['industry_from'].map(lambda x: x.replace('AGG', 'DOM'))
    io_matrix['share_value_data'] = io_matrix['share_value_mixed'] # THIS!!!
    io_matrix = io_matrix[['industry_to', 'industry_from', 'share_value_data']]
    io_matrix = io_matrix.pivot(index='industry_to', columns='industry_from', values='share_value_data')
    assert io_matrix.index.to_list() == industry_list
    assert io_matrix.columns.to_list() == industry_list
    Omega = io_matrix.values
    return s_C, s_M, C, M, Omega


def build_centrality():
    s_C, s_M, C, M, Omega = filter_io_tables()

    # expenditures and sales shares
    Psi = np.linalg.inv(np.eye(s_C.shape[0]) - Omega)
    E = s_M * M + s_C * C
    S_M = s_M / divide_robust(E) * M
    S_C = s_C / divide_robust(E) * C
    X = Psi.T @ E

    # network centrality
    cent_M = np.einsum('kl,k->l', Psi, s_M)
    cent_C = np.einsum('kl,k->l', Psi, s_C)

    # adjusted sales
    omega = np.einsum('l,lk->kl', E, Psi / divide_robust(X)[np.newaxis, :])
    C_C = np.einsum('kl,l->k', omega, S_C)
    C_M = np.einsum('kl,l->k', omega, S_M)

    # add attributes
    attrib = pd.read_csv(OUTPUT_PATH / 'measurement' / 'accounting' / 'attrib.csv')
    attrib['industry'] = attrib['industry'].map(lambda x: x.replace('USA', 'CHN'))
    attrib['s_C'] = s_C
    attrib['s_M'] = s_M
    attrib['S_M'] = S_M
    attrib['S_C'] = S_C
    attrib['C_C'] = C_C
    attrib['C_M'] = C_M
    attrib['X'] = X
    attrib['E'] = s_M * M + s_C * C
    attrib['E_M'] = s_M * M
    attrib['E_C'] = s_C * C
    attrib['ratio'] = (s_M * M + s_C * C) / X
    attrib['ratio_M'] = (s_M * M) / X
    attrib['ratio_C'] = (s_C * C) / X
    attrib['cent_M'] = cent_M
    attrib['cent_C'] = cent_C
    attrib['final_M'] = s_M / cent_M
    attrib['final_C'] = s_C / cent_C
    attrib['final'] = (s_M * M + s_C * C) / X
    return attrib


def build_centrality_variants():
    attrib = build_centrality()
    attrib.to_csv(OUTPUT_PATH / 'calibration' / 'iotables' / 'centrality_naics_chn.csv', index=False)


def main():
    build_centrality_variants()


if __name__ == '__main__':
    main()

