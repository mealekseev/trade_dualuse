# Build crosswalks

import numpy as np
import pandas as pd
import re

from settings import DATA_PATH, OUTPUT_PATH, output
from utilities import merge_df


def multiply_cw(cw1, cw2, var, keepvars, collapse=True):
    cw = pd.merge(cw1, cw2, how='outer', on=var, validate='m:m', indicator=True)
    output(cw['_merge'].value_counts())
    cw['wt_mappings'] = cw['wt_mappings_x'] * cw['wt_mappings_y']
    cw = cw[keepvars]
    if collapse:
        cw = cw.groupby([keepvars[0], keepvars[1]], dropna=False)['wt_mappings'].sum().reset_index()
    return cw


def build_hs12_names():
    df = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'unstats' / 'HSCodeandDescription.xlsx', sheet_name='HS12', dtype='str')
    df = df[['Code', 'Description']]
    df.columns = ['sector', 'hscode_name']
    df.to_csv(OUTPUT_PATH / 'crosswalks' / 'H4_codes.csv', index=False)

    df = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'unstats' / 'HSCodeandDescription.xlsx', sheet_name='HS92', dtype='str')
    df = df[['Code', 'Description']]
    df.columns = ['sector', 'hscode_name']
    df.to_csv(OUTPUT_PATH / 'crosswalks' / 'H0_codes.csv', index=False)

    df = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'unstats' / 'SITCCodeandDescription.xlsx', sheet_name='SITC2', dtype='str')
    df = df[df['Classification'] == 'S2']
    df = df.copy()
    df = df[['Code', 'Description']]
    df.columns = ['sector', 'sitc_name']
    df.to_csv(OUTPUT_PATH / 'crosswalks' / 'S2_codes.csv', index=False)


def build_hs_versions(base='hs12'):
    cw = pd.read_excel(DATA_PATH / 'crosswalks' / 'unstats' / 'hs_sitc_bec_2022.xlsx', dtype=str)
    cw.columns = [x.lower() for x in cw.columns]
    cw = cw[~cw[base].isna()].copy()

    cw_combined = cw[[base]].copy()
    basecol = 'hscode' + base[-2:]
    cw_combined.columns = [basecol]
    cw_combined['hscode_from'] = cw_combined[basecol]
    cw_combined['wgt'] = 1.0
    cw_combined = cw_combined.groupby(['hscode_from', basecol]).first().reset_index()

    cw_year = cw_combined.copy()
    cw_plus = cw_combined.copy()
    cw_plus['hscode_from'] = cw_plus['hscode_from'].map(lambda x: x[0:4] + '00')
    cw_plus = cw_plus.groupby(['hscode_from', basecol]).first().reset_index()
    cw_plus['wgt'] = cw_plus.groupby('hscode_from')[basecol].transform('count')
    cw_plus['wgt'] = 1 / cw_plus['wgt']
    cw_plus = cw_plus[
        cw_plus['hscode_from'].map(lambda x: x not in cw_year[basecol])]
    cw_plus = pd.concat([cw_year, cw_plus], axis=0, ignore_index=True)
    cw_plus.to_csv(OUTPUT_PATH / 'crosswalks' / f'{base}_{base}_plus.csv', index=False)

    col_list = ['hs12', 'hs17', 'hs07', 'hs22', 'hs02', 'hs96', 'hs92']
    col_list = [c for c in col_list if c != base]
    for col in col_list:
        output(f"Processing {col}")
        cw_year = cw[[col, base]]
        cw_year = cw_year.groupby([col, base]).first().reset_index()
        cw_year['wgt'] = cw_year.groupby(col)[base].transform('count')
        cw_year['wgt'] = 1 / cw_year['wgt']
        cw_year.to_csv(OUTPUT_PATH / 'crosswalks' / f'{col}_{base}.csv', index=False)

        cw_plus = cw_year.copy()
        cw_plus[col] = cw_plus[col].map(lambda x: x[0:4] + '00')
        cw_plus = cw_plus.groupby([col, base]).first().reset_index()
        cw_plus['wgt'] = cw_plus.groupby(col)[base].transform('count')
        cw_plus['wgt'] = 1 / cw_plus['wgt']
        cw_plus = cw_plus[
            cw_plus[col].map(lambda x: x not in cw_year[col])]
        cw_plus = pd.concat([cw_year, cw_plus], axis=0, ignore_index=True)
        cw_plus.rename({col: 'hscode_from', base: basecol}, axis=1, inplace=True)
        cw_plus.to_csv(OUTPUT_PATH / 'crosswalks' / f'{col}_{base}_plus.csv', index=False)

        cw_new = cw_year[cw_year[col].map(lambda x: x not in cw_combined['hscode_from'].tolist())].copy()
        cw_new.rename({col: 'hscode_from', base: basecol}, axis=1, inplace=True)
        cw_combined = pd.concat([cw_combined, cw_new], axis=0, ignore_index=True)
    cw_combined.to_csv(OUTPUT_PATH / 'crosswalks' / f'{base}_combined.csv', index=False)

    cw_combined_plus = cw_combined.copy()
    cw_combined_plus['hscode_from'] = cw_combined_plus['hscode_from'].map(lambda x: x[0:4] + '00')
    cw_combined_plus = cw_combined_plus.groupby(['hscode_from', basecol]).first().reset_index()
    cw_combined_plus['wgt'] = cw_combined_plus.groupby('hscode_from')[basecol].transform('count')
    cw_combined_plus['wgt'] = 1 / cw_combined_plus['wgt']
    cw_combined_plus = cw_combined_plus[
        cw_combined_plus['hscode_from'].map(lambda x: x not in list(cw_combined['hscode_from']))]
    cw_combined_plus = pd.concat([cw_combined, cw_combined_plus], axis=0, ignore_index=True)
    cw_combined_plus.to_csv(OUTPUT_PATH / 'crosswalks' / f'{base}_combined_plus.csv', index=False)


def build_sitc75_hs12():
    cw = pd.read_excel(DATA_PATH / 'crosswalks' / 'unstats' / 'hs_sitc_bec_2022.xlsx', dtype=str)
    cw = cw[['HS12', 'SITC2']]
    cw.columns = ['hs12', 'sitc2']
    cw['hs12'] = cw['hs12'].map(lambda x: '271000' if str(x).startswith('2710') else x)
    cw = cw[~cw['sitc2'].isna()]
    cw['hs12'] = cw['hs12'].fillna('999999')
    cw['sitc'] = cw['sitc2'].map(lambda x: x[:4])
    cw['sitc'] = cw['sitc'].map(lambda x: '3340' if str(x).startswith('334') else x)
    cw['wgt'] = 1 / cw.groupby('sitc')['sitc2'].transform('count')
    cw = cw.groupby(['sitc', 'hs12'])['wgt'].sum().reset_index()
    cw.columns = ['sitc75', 'hs12', 'wgt']
    cw.to_csv(OUTPUT_PATH / 'crosswalks' / 'sitc75_hs12.csv', index=False)


def build_naics():
    # combined into 12
    naics_combined = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'census' / '6-digit_2012_Codes.xls', skiprows=1, dtype=str
    )
    naics_combined.columns = ['naics_from', 'naics12']
    naics_combined['naics12'] = naics_combined['naics_from']
    naics_combined['classification'] = '2012'
    naics_combined['wt_mappings'] = 1

    # 17 to 12
    naics_17_12 = pd.read_excel(DATA_PATH / 'crosswalks' / 'census' / '2017_to_2012_NAICS.xlsx', skiprows=2, dtype=str)
    naics_17_12 = naics_17_12.iloc[:, 0:4]
    naics_17_12.columns = ['naics17', 'naics17_title', 'naics12', 'naics12_title']
    naics_17_12['wt_mappings'] = 1 / naics_17_12.groupby('naics17')['naics12'].transform('count')
    naics_17_12.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics17_naics12.csv', index=False)
    naics_17_12_aux = naics_17_12[['naics17', 'naics12', 'wt_mappings']].copy()
    naics_17_12_aux.columns = ['naics_from', 'naics12', 'wt_mappings']
    naics_17_12_aux = naics_17_12_aux[
        naics_17_12_aux['naics_from'].map(lambda x: x not in naics_combined['naics_from'].to_list())
    ]
    naics_17_12_aux['classification'] = '2017'
    naics_combined = pd.concat([naics_combined, naics_17_12_aux])

    # 22 to 17
    naics_22_17 = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'census' / '2022_to_2017_NAICS.xlsx', skiprows=2, dtype='str'
    )
    naics_22_17 = naics_22_17.iloc[:, 0:4]
    naics_22_17.columns = ['naics22', 'naics22_title', 'naics17', 'naics17_title']
    naics_22_17['wt_mappings'] = 1 / naics_22_17.groupby('naics22')['naics17'].transform('count')
    naics_22_17.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics22_naics17.csv', index=False)

    # 22 to 12
    naics_22_12 = multiply_cw(naics_22_17, naics_17_12, 'naics17', ['naics22', 'naics12', 'wt_mappings'])
    naics_22_12.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics22_naics12.csv', index=False)
    naics_22_12_aux = naics_22_12[['naics22', 'naics12', 'wt_mappings']].copy()
    naics_22_12_aux.columns = ['naics_from', 'naics12', 'wt_mappings']
    naics_22_12_aux = naics_22_12_aux[
        naics_22_12_aux['naics_from'].map(lambda x: x not in naics_combined['naics_from'].to_list())
    ]
    naics_22_12_aux['classification'] = '2022'
    naics_combined = pd.concat([naics_combined, naics_22_12_aux])

    # 07 to 12
    naics_07_12 = pd.read_csv(DATA_PATH / 'crosswalks' / 'efsy_final_concordances' / 'full_naics07_naics12.csv')
    naics_07_12 = naics_07_12[naics_07_12['naics07'].map(lambda x: not x.endswith('/') and not x.endswith('-'))]
    naics_07_12 = naics_07_12[['naics07', 'naics12', 'wt_mappings']]
    naics_07_12.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics07_naics12.csv', index=False)
    naics_07_12_aux = naics_07_12.copy()
    naics_07_12_aux = naics_07_12_aux[
        naics_07_12_aux['naics07'].map(lambda x: x not in naics_combined['naics_from'].to_list())
    ]
    naics_07_12_aux.columns = ['naics_from', 'naics12', 'wt_mappings']
    naics_07_12_aux['classification'] = '2007'
    naics_combined = pd.concat([naics_combined, naics_07_12_aux])

    # 02 to 12
    naics_02_07 = pd.read_csv(DATA_PATH / 'crosswalks' / 'efsy_final_concordances' / 'full_naics02_naics07.csv')
    naics_02_12 = multiply_cw(naics_02_07, naics_07_12, 'naics07', ['naics02', 'naics12', 'wt_mappings'])
    naics_02_12 = naics_02_12[naics_02_12['naics02'].map(lambda x: not x.endswith('/') and not x.endswith('-'))]
    naics_02_12.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics02_naics12.csv', index=False)
    naics_02_12_aux = naics_02_12.copy()
    naics_02_12_aux = naics_02_12_aux[
        naics_02_12_aux['naics02'].map(lambda x: x not in naics_combined['naics_from'].to_list())
    ]
    naics_02_12_aux.columns = ['naics_from', 'naics12', 'wt_mappings']
    naics_02_12_aux['classification'] = '2002'
    naics_combined = pd.concat([naics_combined, naics_02_12_aux])

    # 97 to 12
    naics_97_02 = pd.read_csv(DATA_PATH / 'crosswalks' / 'efsy_final_concordances' / 'full_naics97_naics02.csv')
    naics_97_12 = multiply_cw(naics_97_02, naics_02_12, 'naics02', ['naics97', 'naics12', 'wt_mappings'])
    naics_97_12 = naics_97_12[naics_97_12['naics97'].map(lambda x: not x.endswith('/') and not x.endswith('-'))]
    naics_97_12['naics12'] = naics_97_12['naics12'].fillna('999990')
    naics_97_12.loc[naics_97_12['naics12'] == '999990', 'wt_mappings'] = 1.
    naics_97_12.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics97_naics12.csv', index=False)
    naics_97_12_aux = naics_97_12.copy()
    naics_97_12_aux = naics_97_12_aux[
        naics_97_12_aux['naics97'].map(lambda x: x not in naics_combined['naics_from'].to_list())
    ]
    naics_97_12_aux.columns = ['naics_from', 'naics12', 'wt_mappings']
    naics_97_12_aux['classification'] = '1997'
    naics_combined = pd.concat([naics_combined, naics_97_12_aux])

    # combined
    naics_combined = naics_combined.sort_values('naics_from')
    naics_combined.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics_combined.csv', index=False)

    naics_combined['naics'] = naics_combined.apply(lambda x: (x['naics12'], x['wt_mappings']), axis=1)
    naics_combined = naics_combined.groupby('naics_from')['naics'].apply(list).reset_index()
    naics_combined.to_pickle(OUTPUT_PATH / 'crosswalks' / 'naics_combined.pkl')

    # 12 to 07
    naics_12_07 = pd.read_csv(DATA_PATH / 'crosswalks' / 'efsy_final_concordances' / 'full_naics07_naics12.csv')
    naics_12_07 = naics_12_07[naics_12_07['naics07'].map(lambda x: not x.endswith('/') and not x.endswith('-'))]
    naics_12_07['wt_mappings'] = (
            naics_12_07['wt_mappings'] / naics_12_07.groupby('naics12')['wt_mappings'].transform('sum'))
    naics_12_07 = naics_12_07[['naics12', 'naics07', 'wt_mappings']]
    naics_12_07.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_naics07.csv', index=False)

    # 07 to 02
    naics_07_02 = pd.read_csv(DATA_PATH / 'crosswalks' / 'efsy_final_concordances' / 'full_naics02_naics07.csv')
    naics_07_02 = naics_07_02[naics_07_02['naics02'].map(lambda x: not x.endswith('/') and not x.endswith('-'))]
    naics_07_02['wt_mappings'] = (
            naics_07_02['wt_mappings'] / naics_07_02.groupby('naics07')['wt_mappings'].transform('sum'))
    naics_07_02 = naics_07_02[['naics07', 'naics02', 'wt_mappings']]
    naics_07_02.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics07_naics02.csv', index=False)

    # 02 to 97
    naics_02_97 = pd.read_csv(DATA_PATH / 'crosswalks' / 'efsy_final_concordances' / 'full_naics97_naics02.csv')
    naics_02_97 = naics_02_97[naics_02_97['naics97'].map(lambda x: not x.endswith('/') and not x.endswith('-'))]
    naics_02_97['wt_mappings'] = (
            naics_02_97['wt_mappings'] / naics_02_97.groupby('naics02')['wt_mappings'].transform('sum'))
    naics_02_97 = naics_02_97[['naics02', 'naics97', 'wt_mappings']]
    naics_02_97.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics02_naics97.csv', index=False)

    naics_12_02 = multiply_cw(naics_12_07, naics_07_02, 'naics07', ['naics12', 'naics02', 'wt_mappings'])
    naics_12_02.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_naics02.csv', index=False)

    naics_12_97 = multiply_cw(naics_12_02, naics_02_97, 'naics02', ['naics12', 'naics97', 'wt_mappings'])
    naics_12_97 = naics_12_97[~naics_12_97['naics97'].isna()]
    naics_12_97.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_naics97.csv', index=False)


def build_naics_bea():
    naics12 = pd.read_excel(DATA_PATH / 'crosswalks' / 'census' / '6-digit_2012_Codes.xls', skiprows=1, dtype=str)
    naics12.columns = ['naics12', 'other']
    naics12 = naics12[['naics12']]
    naics12_list = naics12['naics12'].to_list()

    bea = pd.read_excel(
        DATA_PATH / 'measurement' / 'accounting' / 'bea' / 'Use_SUT_Framework_2007_2012_DET.xlsx', sheet_name='NAICS Codes', skiprows=4, dtype=str
    )
    bea.columns = ['1', '2', '3', 'bea12', 'bea12_text', 'notes', 'naicscode']
    bea = bea[['bea12', 'naicscode']]
    bea = bea[bea['bea12'].map(lambda x: len(str(x)) == 5 or len(str(x)) == 6)]
    bea['flag'] = bea['naicscode'].map(lambda x: int(x == 'n.a.'))

    def process_naicsstr(text):

        def pull_naics(t):
            s = t.replace('*', '').replace('\n', '')
            if '-' not in s:
                return [n for n in naics12_list if n.startswith(s)]

            s0, s1 = s.split('-')
            core = s0[:-1]
            core_list = [core + str(i) for i in range(int(s0[-1]), int(s1) + 1)]
            naics_list = [n for n in naics12_list if any([n.startswith(s) for s in core_list])]
            return naics_list

        return [i for t in text.split(', ') for i in pull_naics(t)]

    bea['naics12'] = bea['naicscode'].map(process_naicsstr)
    bea = bea.apply(lambda x: x.explode()).reset_index()
    bea = bea[['naics12', 'bea12']]
    bea.loc[bea['naics12'].isna(), 'naics12'] = bea.loc[bea['naics12'].isna(), 'bea12']
    bea = bea.sort_values(['bea12', 'naics12'])
    bea['wgt'] = bea.groupby('naics12')['bea12'].transform('count')
    bea['wgt'] = 1 / bea['wgt']

    naics12_other = naics12[~naics12['naics12'].isin(bea['naics12'].to_list())].copy()
    naics12_other['bea12'] = naics12_other['naics12']
    naics12_other['wgt'] = 1.
    naics12_special = pd.DataFrame([['999990', '999990', 1.]], columns=['naics12', 'bea12', 'wgt'])

    bea = pd.concat([bea, naics12_other, naics12_special])
    bea.to_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', index=False)


def build_hs_naics12():
    naics12 = pd.read_excel(DATA_PATH / 'crosswalks' / 'census' / '6-digit_2012_Codes.xls', skiprows=1, dtype=str)
    naics12.columns = ['naics12', 'other']
    naics12_list = naics12['naics12'].to_list() + ['999990']
    naics17 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics17_naics12.csv', dtype=str)
    naics17_list = naics17['naics17'].to_list() + ['999990']
    naics22 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics22_naics12.csv', dtype=str)
    naics22_list = naics22['naics22'].to_list() + ['999990']
    naics07 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics07_naics12.csv', dtype=str)
    naics07_list = naics07['naics07'].to_list() + ['999990']
    naics02 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics02_naics12.csv', dtype=str)
    naics02_list = naics02['naics02'].to_list() + ['999990']
    naics97 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics97_naics12.csv', dtype=str)
    naics97_list = naics97['naics97'].to_list() + ['999990']

    hs_naics_exp = pd.read_csv(
        DATA_PATH / 'crosswalks' / 'hssicnaics' / 'hs_sic_naics_exports_89_121_20220627.csv', sep='\t', dtype='str'
    )
    hs_naics_exp['commodity'] = hs_naics_exp['commodity'].map(lambda x: x.zfill(10))
    hs_naics_exp = hs_naics_exp[~hs_naics_exp['naics'].isna()]
    hs_naics_imp = pd.read_csv(
        DATA_PATH / 'crosswalks' / 'hssicnaics' / 'hs_sic_naics_imports_89_121_20220627.csv', sep='\t', dtype='str'
    )
    hs_naics_imp['commodity'] = hs_naics_exp['commodity'].map(lambda x: x.zfill(10))
    hs_naics_imp = hs_naics_exp[~hs_naics_exp['naics'].isna()]

    hs_naics = pd.concat([hs_naics_exp, hs_naics_imp], axis=0)
    hs_naics = hs_naics[hs_naics['naics'] != '.'].copy()
    hs_naics['hscode'] = hs_naics['commodity'].map(lambda x: x[:6])
    hs_naics['count'] = hs_naics.groupby(['hscode', 'year'])['hscode'].transform('count')
    hs_naics['wgt'] = 1 / hs_naics['count']
    hs_naics = hs_naics.groupby(['hscode', 'year', 'naics'])['wgt'].sum().reset_index()

    hs_naics['naics_match97'] = hs_naics['naics'].map(lambda x: x in naics97_list)
    hs_naics['naics_match02'] = hs_naics['naics'].map(lambda x: x in naics02_list)
    hs_naics['naics_match07'] = hs_naics['naics'].map(lambda x: x in naics07_list)
    hs_naics['naics_match12'] = hs_naics['naics'].map(lambda x: x in naics12_list)
    hs_naics['naics_match17'] = hs_naics['naics'].map(lambda x: x in naics17_list)
    hs_naics['naics_match22'] = hs_naics['naics'].map(lambda x: x in naics22_list)
    hs_naics.groupby('year')[
        ['naics_match97', 'naics_match02', 'naics_match07', 'naics_match12', 'naics_match17', 'naics_match22']
    ].sum().reset_index()

    def process_naicsstr(text, naics_list):
        text = text.replace('X', '')
        if text.startswith('9') and (text.endswith('X') or text.endswith('0000')):
            text = '9999'
        res = [n for n in naics_list if n.startswith(text)]
        return res

    hs_naics07 = hs_naics[hs_naics['year'].map(int) < 2013].copy()
    hs_naics07['naics07'] = hs_naics07['naics'].map(lambda x: process_naicsstr(x, naics07_list))
    hs_naics07['wgt_naics'] = 1 / hs_naics07['naics07'].map(len)
    hs_naics07 = hs_naics07.apply(lambda x: x.explode()).reset_index()
    hs_naics07 = merge_df(hs_naics07, naics07, left_on='naics07', right_on='naics07', how='left')
    hs_naics07['wt_mappings'] = hs_naics07['wgt'] * hs_naics07['wgt_naics'] * hs_naics07['wt_mappings'].map(float)
    hs_naics07 = hs_naics07.groupby(['hscode', 'year', 'naics12'])['wt_mappings'].sum().reset_index()

    hs_naics12 = hs_naics[(hs_naics['year'].map(int) < 2018) & (hs_naics['year'].map(int) >= 2013)].copy()
    hs_naics12['naics12'] = hs_naics12['naics'].map(lambda x: process_naicsstr(x, naics12_list))
    hs_naics12['wgt_naics'] = 1 / hs_naics12['naics12'].map(len)
    hs_naics12 = hs_naics12.apply(lambda x: x.explode()).reset_index()
    hs_naics12['wt_mappings'] = hs_naics12['wgt'] * hs_naics12['wgt_naics']
    hs_naics12 = hs_naics12.groupby(['hscode', 'year', 'naics12'])['wt_mappings'].sum().reset_index()

    hs_naics17 = hs_naics[(hs_naics['year'].map(int) < 2023) & (hs_naics['year'].map(int) >= 2018)].copy()
    hs_naics17['naics17'] = hs_naics17['naics'].map(lambda x: process_naicsstr(x, naics17_list))
    hs_naics17['wgt_naics'] = 1 / hs_naics17['naics17'].map(len)
    hs_naics17 = hs_naics17.apply(lambda x: x.explode()).reset_index()
    hs_naics17 = merge_df(hs_naics17, naics17, left_on='naics17', right_on='naics17', how='left')
    hs_naics17['wt_mappings'] = hs_naics17['wgt'] * hs_naics17['wgt_naics'] * hs_naics17['wt_mappings'].map(float)
    hs_naics17 = hs_naics17.groupby(['hscode', 'year', 'naics12'])['wt_mappings'].sum().reset_index()

    hs_naics = pd.concat([hs_naics07, hs_naics12, hs_naics17], axis=0, ignore_index=True)
    hs_naics.to_csv(OUTPUT_PATH / 'crosswalks' / 'hs_naics12.csv', index=False)

    # HS0 1988, HS1 1996, HS2 2002, HS3 2007, HS4 2012, HS5 2017, HS6 2022
    hs_naics.groupby('hscode').first().reset_index()['year'].value_counts()
    row_last = pd.DataFrame([{'hscode': '999999', 'naics12': '999990', 'wt_mappings': 1.0}])

    hs0_naics = hs_naics[hs_naics['year'].map(int) < 1996].copy()
    hs0_naics = hs0_naics[hs0_naics['year'] == hs0_naics.groupby('hscode')['year'].transform('max')].drop('year', axis=1)
    hs0_naics = pd.concat([hs0_naics, row_last], ignore_index=True)
    hs0_naics.to_csv(OUTPUT_PATH / 'crosswalks' / 'hs0_naics12.csv', index=False)

    hs1_naics = hs_naics[(hs_naics['year'].map(int) < 2002) & (hs_naics['year'].map(int) >= 1996)].copy()
    hs1_naics = hs1_naics[hs1_naics['year'] == hs1_naics.groupby('hscode')['year'].transform('max')].drop('year', axis=1)
    hs1_naics = pd.concat([hs1_naics, row_last], ignore_index=True)
    hs1_naics.to_csv(OUTPUT_PATH / 'crosswalks' / 'hs1_naics12.csv', index=False)

    hs2_naics = hs_naics[(hs_naics['year'].map(int) < 2007) & (hs_naics['year'].map(int) >= 2002)].copy()
    hs2_naics = hs2_naics[hs2_naics['year'] == hs2_naics.groupby('hscode')['year'].transform('max')].drop('year', axis=1)
    hs2_naics = pd.concat([hs2_naics, row_last], ignore_index=True)
    hs2_naics.to_csv(OUTPUT_PATH / 'crosswalks' / 'hs2_naics12.csv', index=False)

    hs3_naics = hs_naics[(hs_naics['year'].map(int) < 2012) & (hs_naics['year'].map(int) >= 2007)].copy()
    hs3_naics = hs3_naics[hs3_naics['year'] == hs3_naics.groupby('hscode')['year'].transform('max')].drop('year', axis=1)
    hs3_naics = pd.concat([hs3_naics, row_last], ignore_index=True)
    hs3_naics.to_csv(OUTPUT_PATH / 'crosswalks' / 'hs3_naics12.csv', index=False)

    hs4_naics = hs_naics[(hs_naics['year'].map(int) < 2017) & (hs_naics['year'].map(int) >= 2012)].copy()
    hs4_naics = hs4_naics[hs4_naics['year'] == hs4_naics.groupby('hscode')['year'].transform('max')].drop('year', axis=1)
    hs4_naics = pd.concat([hs4_naics, row_last], ignore_index=True)
    hs4_naics.to_csv(OUTPUT_PATH / 'crosswalks' / 'hs4_naics12.csv', index=False)

    hs5_naics = hs_naics[(hs_naics['year'].map(int) < 2023) & (hs_naics['year'].map(int) >= 2018)].copy()
    hs5_naics = hs5_naics[hs5_naics['year'] == hs5_naics.groupby('hscode')['year'].transform('max')].drop('year', axis=1)
    hs5_naics = pd.concat([hs5_naics, row_last], ignore_index=True)
    hs5_naics.to_csv(OUTPUT_PATH / 'crosswalks' / 'hs5_naics12.csv', index=False)


def build_hs_naics12_4digit():
    for v in range(0, 6):
        hs_naics = pd.read_csv(OUTPUT_PATH / 'crosswalks' / f'hs{v}_naics12.csv', dtype={'hscode': str, 'naics12': str})
        hs_naics['hs4'] = hs_naics['hscode'].map(lambda x: x[:4])
        hs_naics = hs_naics.groupby(['hs4', 'naics12'])['wt_mappings'].sum().reset_index()

        hs_naics_hs = hs_naics.copy()
        hs_naics_hs['wt_mappings'] = (
                hs_naics_hs['wt_mappings'] / hs_naics_hs.groupby('hs4')['wt_mappings'].transform('sum'))
        hs_naics_hs.to_csv(OUTPUT_PATH / 'crosswalks' / f'hs{v}_naics12_4digit.csv', index=False)

        naics_hs = hs_naics.copy()
        naics_hs['wt_mappings'] = (
                naics_hs['wt_mappings'] / naics_hs.groupby('naics12')['wt_mappings'].transform('sum'))
        naics_hs = naics_hs[['naics12', 'hs4', 'wt_mappings']]
        naics_hs = naics_hs.sort_values('naics12')
        naics_hs.to_csv(OUTPUT_PATH / 'crosswalks' / f'naics12_hs{v}_4digit.csv', index=False)


def build_psc_naics12():
    cw = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'psc_naics' / 'DLA NAICS to FSC or PSC Cross Reference.xlsx', dtype=str
    )
    cw = cw[~cw['PSC Code'].isna() & ~cw['NAICS Code'].isna()]
    cw = cw[cw['NAICS Code'] != 'NONE']
    cw = cw[['PSC Code', 'NAICS Code']]
    cw.columns = ['psc', 'naics_from']
    cw['wgt'] = 1
    cw['wgt'] = 1 / cw.groupby('psc')['wgt'].transform('sum')

    naics_combined = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics_combined.csv', dtype=str)
    cw = merge_df(cw, naics_combined, on='naics_from', how='left')
    cw['wt_mappings'] = cw['wgt'] * cw['wt_mappings'].map(float)
    cw = cw[['psc', 'naics12', 'classification', 'wt_mappings']]
    cw.to_csv(OUTPUT_PATH / 'crosswalks' / 'psc_naics12.csv', index=False)


def build_sic_naics12():
    naics_97_12 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics97_naics12.csv', dtype=str)
    naics_97_12['wt_mappings'] = naics_97_12['wt_mappings'].astype(float)
    naics97_list = list(naics_97_12['naics97'].unique())

    def process_naicsstr(text, naics97_list):
        if '/' not in text:
            return [text]
        text = text.replace('/', '')
        naics_list = [n for n in naics97_list if n.startswith(text)]
        if len(naics_list) == 0:
            return ['999990']
        return naics_list

    sic87_naics97 = pd.read_csv(DATA_PATH / 'crosswalks' / 'efsy_final_concordances' / 'full_sic87_naics97.csv')
    sic87_naics97 = sic87_naics97[['sic87', 'naics97', 'weight_hybrid']]
    sic87_naics97.columns = ['sic87', 'naics97', 'wt_mappings']
    sic87_naics97 = sic87_naics97[sic87_naics97['sic87'].map(lambda x: not x.endswith('-') and not x.endswith('\\'))]
    sic87_naics97['naics_list'] = sic87_naics97['naics97'].map(lambda x: process_naicsstr(x, naics97_list))
    sic87_naics97['wgt'] = sic87_naics97['naics_list'].map(lambda x: 1 / len(x))
    sic87_naics97 = sic87_naics97.reset_index().apply(lambda x: x.explode()).reset_index()
    sic87_naics97['wt_mappings'] = sic87_naics97['wt_mappings'] * sic87_naics97['wgt']
    sic87_naics97 = sic87_naics97.groupby(['sic87', 'naics_list'])['wt_mappings'].sum().reset_index()
    sic87_naics97.columns = ['sic87', 'naics97', 'wt_mappings']
    sic87_naics12 = multiply_cw(sic87_naics97, naics_97_12, 'naics97', ['sic87', 'naics12', 'wt_mappings'])
    sic87_naics12 = sic87_naics12[~sic87_naics12['sic87'].isna()]
    sic87_naics12['naics12'] = sic87_naics12['naics12'].fillna('999990')
    sic87_naics12.loc[sic87_naics12['naics12'] == '999990', 'wt_mappings'] = 1.
    sic87_naics12.to_csv(OUTPUT_PATH / 'crosswalks' / 'sic87_naics12.csv', index=False)

    sic77_sic87 = pd.read_csv(DATA_PATH / 'crosswalks' / 'efsy_final_concordances' / 'full_sic77_sic87.csv')
    sic77_sic87 = sic77_sic87[['sic77', 'sic87', 'weight_mappings']]
    sic77_sic87.columns = ['sic77', 'sic87', 'wt_mappings']
    sic77_sic87 = sic77_sic87[sic77_sic87['sic77'].map(lambda x: not x.endswith('-') and not x.endswith('\\'))]
    sic77_naics12 = multiply_cw(sic77_sic87, sic87_naics12, 'sic87', ['sic77', 'naics12', 'wt_mappings'])
    sic77_naics12 = sic77_naics12[~sic77_naics12['sic77'].isna()]
    sic77_naics12['naics12'] = sic77_naics12['naics12'].fillna('999990')
    sic77_naics12.loc[sic77_naics12['naics12'] == '999990', 'wt_mappings'] = 1.
    sic77_naics12.to_csv(OUTPUT_PATH / 'crosswalks' / 'sic77_naics12.csv', index=False)

    sic72_sic77 = pd.read_csv(DATA_PATH / 'crosswalks' / 'efsy_final_concordances' / 'full_sic72_sic77.csv')
    sic72_sic77 = sic72_sic77[['sic72', 'sic77', 'weight_mappings']]
    sic72_sic77.columns = ['sic72', 'sic77', 'wt_mappings']
    sic72_sic77 = sic72_sic77[sic72_sic77['sic77'].map(lambda x: not x.endswith('-') and not x.endswith('\\'))]
    sic72_naics12 = multiply_cw(sic72_sic77, sic77_naics12, 'sic77', ['sic72', 'naics12', 'wt_mappings'])
    sic72_naics12 = sic72_naics12[~sic72_naics12['sic72'].isna()]
    sic72_naics12.to_csv(OUTPUT_PATH / 'crosswalks' / 'sic72_naics12.csv', index=False)

    sic87_naics12.columns = ['sic', 'naics12', 'wt_mappings']
    sic87_naics12['classification'] = 1987
    sic77_naics12.columns = ['sic', 'naics12', 'wt_mappings']
    sic77_naics12['classification'] = 1977
    sic72_naics12.columns = ['sic', 'naics12', 'wt_mappings']
    sic72_naics12['classification'] = 1972

    sic77_naics12 = sic77_naics12[sic77_naics12['sic'].map(lambda x: not x in sic87_naics12['sic'].to_list())]
    sic_combined = pd.concat([sic77_naics12, sic87_naics12], axis=0)
    sic72_naics12 = sic72_naics12[sic72_naics12['sic'].map(lambda x: not x in sic_combined['sic'].to_list())]
    sic_combined = pd.concat([sic72_naics12, sic_combined], axis=0)
    sic_combined.to_csv(OUTPUT_PATH / 'crosswalks' / 'sic_combined_naics12.csv', index=False)


def build_nace2_naics12():
    cw_nace2 = pd.read_csv(DATA_PATH / 'crosswalks' / 'unstats' / 'NACE2_ISIC4.txt')
    cw_nace2 = cw_nace2[['NACE2code', 'ISIC4code']]
    cw_nace2.columns = ['nace', 'isic4']
    cw_nace2['wgt_nace2'] = 1 / cw_nace2.groupby('nace')['isic4'].transform('count')
    cw_nace2['nace'] = cw_nace2['nace'].map(lambda x: x.replace('.', ''))
    cw_isic4 = pd.read_csv(
        DATA_PATH / 'crosswalks' / 'unstats' / 'ISIC4-NAICS2012US.txt',
        dtype={'ISIC4Code': 'str', 'NAICS2012Code': 'str'}
    )
    cw_isic4 = cw_isic4[['ISIC4Code', 'NAICS2012Code']]
    cw_isic4.columns = ['isic4', 'naics12']
    cw_isic4['wgt_isic4'] = 1 / cw_isic4.groupby('isic4')['naics12'].transform('count')
    cw_nace2 = merge_df(cw_nace2, cw_isic4, left_on='isic4', right_on='isic4', how='inner', keep_merge=False)
    cw_nace2['wgt_nace2'] = cw_nace2['wgt_nace2'] * cw_nace2['wgt_isic4']
    cw_nace2 = cw_nace2[['nace', 'naics12', 'wgt_nace2']]
    cw_nace2.to_csv(OUTPUT_PATH / 'crosswalks' / 'nace2_naics12.csv', index=False)


def empty_build_hs10_naics12():
    naics12 = pd.read_excel(DATA_PATH / 'crosswalks' / 'census' / '6-digit_2012_Codes.xls', skiprows=1, dtype=str)
    naics12.columns = ['naics12', 'other']
    naics12_list = naics12['naics12'].to_list()
    naics17 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics17_naics12.csv', dtype=str)
    naics17_list = naics17['naics17'].to_list()
    naics22 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics22_naics12.csv', dtype=str)
    naics22_list = naics22['naics22'].to_list()
    naics07 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics07_naics12.csv', dtype=str)
    naics07_list = naics07['naics07'].to_list()
    naics02 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics02_naics12.csv', dtype=str)
    naics02_list = naics02['naics02'].to_list()
    naics97 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics97_naics12.csv', dtype=str)
    naics97_list = naics97['naics97'].to_list()

    hs_naics_exp = pd.read_csv(
        DATA_PATH / 'crosswalks' / 'hssicnaics' / 'hs_sic_naics_exports_89_121_20220627.csv', sep='\t', dtype='str'
    )
    hs_naics_exp['commodity'] = hs_naics_exp['commodity'].map(lambda x: x.zfill(10))
    hs_naics_exp = hs_naics_exp[~hs_naics_exp['naics'].isna()]
    hs_naics_imp = pd.read_csv(
        DATA_PATH / 'crosswalks' / 'hssicnaics' / 'hs_sic_naics_imports_89_121_20220627.csv', sep='\t', dtype='str'
    )
    hs_naics_imp['commodity'] = hs_naics_exp['commodity'].map(lambda x: x.zfill(10))
    hs_naics_imp = hs_naics_exp[~hs_naics_exp['naics'].isna()]

    hs_naics['naics_match97'] = hs_naics['naics'].map(lambda x: x in naics97_list)
    hs_naics['naics_match02'] = hs_naics['naics'].map(lambda x: x in naics02_list)
    hs_naics['naics_match07'] = hs_naics['naics'].map(lambda x: x in naics07_list)
    hs_naics['naics_match12'] = hs_naics['naics'].map(lambda x: x in naics12_list)
    hs_naics['naics_match17'] = hs_naics['naics'].map(lambda x: x in naics17_list)
    hs_naics['naics_match22'] = hs_naics['naics'].map(lambda x: x in naics22_list)
    hs_naics.groupby('year')[
        ['naics_match97', 'naics_match02', 'naics_match07', 'naics_match12', 'naics_match17', 'naics_match22']
    ].sum().reset_index()

    def process_naicsstr(text, naics_list):
        text = text.replace('X', '')
        if text.startswith('9') and (text.endswith('X') or text.endswith('0000')):
            text = '9'
        res = [n for n in naics_list if n.startswith(text)]
        return res

    hs_naics07 = hs_naics[hs_naics['year'].map(int) < 2013].copy()
    hs_naics07['naics07'] = hs_naics07['naics'].map(lambda x: process_naicsstr(x, naics07_list))
    hs_naics07['wgt_naics'] = 1 / hs_naics07['naics07'].map(len)
    hs_naics07 = hs_naics07.apply(lambda x: x.explode()).reset_index()
    hs_naics07 = merge_df(hs_naics07, naics07, left_on='naics07', right_on='naics07', how='left')
    hs_naics07['wt_mappings'] = hs_naics07['wgt'] * hs_naics07['wgt_naics'] * hs_naics07['wt_mappings'].map(float)
    hs_naics07 = hs_naics07.groupby(['commodity', 'year', 'naics12'])['wt_mappings'].sum().reset_index()

    hs_naics12 = hs_naics[(hs_naics['year'].map(int) < 2018) & (hs_naics['year'].map(int) >= 2013)].copy()
    hs_naics12['naics12'] = hs_naics12['naics'].map(lambda x: process_naicsstr(x, naics12_list))
    hs_naics12['wgt_naics'] = 1 / hs_naics12['naics12'].map(len)
    hs_naics12 = hs_naics12.apply(lambda x: x.explode()).reset_index()
    hs_naics12['wt_mappings'] = hs_naics12['wgt'] * hs_naics12['wgt_naics']
    hs_naics12 = hs_naics12.groupby(['commodity', 'year', 'naics12'])['wt_mappings'].sum().reset_index()

    hs_naics17 = hs_naics[(hs_naics['year'].map(int) < 2023) & (hs_naics['year'].map(int) >= 2018)].copy()
    hs_naics17['naics17'] = hs_naics17['naics'].map(lambda x: process_naicsstr(x, naics17_list))
    hs_naics17['wgt_naics'] = 1 / hs_naics17['naics17'].map(len)
    hs_naics17 = hs_naics17.apply(lambda x: x.explode()).reset_index()
    hs_naics17 = merge_df(hs_naics17, naics17, left_on='naics17', right_on='naics17', how='left')
    hs_naics17['wt_mappings'] = hs_naics17['wgt'] * hs_naics17['wgt_naics'] * hs_naics17['wt_mappings'].map(float)
    hs_naics17 = hs_naics17.groupby(['commodity', 'year', 'naics12'])['wt_mappings'].sum().reset_index()

    hs_naics = pd.concat([hs_naics07, hs_naics12, hs_naics17], axis=0)
    hs_naics = hs_naics.sort_values('year')
    hs_naics.to_csv(OUTPUT_PATH / 'crosswalks' / 'hs10_naics12.csv', index=False)


def build_naics12_sitc75():
    cw = pd.read_excel(DATA_PATH / 'crosswalks' / 'unstats' / 'hs_sitc_bec_2022.xlsx', dtype=str)
    cw = cw[['HS12', 'SITC2']]
    cw.columns = ['hs12', 'sitc2']
    cw['hs12'] = cw['hs12'].map(lambda x: '271000' if str(x).startswith('2710') else x)
    cw = cw[~cw['sitc2'].isna()]
    cw['hs12'] = cw['hs12'].fillna('999999')
    cw['sitc'] = cw['sitc2'].map(lambda x: x[:4])
    cw['sitc'] = cw['sitc'].map(lambda x: '3340' if str(x).startswith('334') else x)
    cw['wgt'] = 1 / cw.groupby('sitc')['sitc2'].transform('count')
    cw = cw.groupby(['sitc', 'hs12'])['wgt'].sum().reset_index()

    naics_hs = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs4_naics12.csv', dtype={'hscode': str, 'naics12': str})
    naics_hs['hscode'] = naics_hs['hscode'].map(lambda x: '271000' if str(x).startswith('2710') else x)
    naics_hs = naics_hs.groupby(['hscode', 'naics12']).first().reset_index()
    cw = merge_df(cw, naics_hs, left_on='hs12', right_on='hscode', how='left')

    cw['wgt'] = cw['wgt'] * cw['wt_mappings']
    cw = cw.groupby(['sitc', 'naics12'])['wgt'].sum().reset_index()
    cw.columns = ['sitc75', 'naics12', 'wgt']
    cw.to_csv(OUTPUT_PATH / 'crosswalks' / 'sitc75_naics12.csv', index=False)


def empty_build_fsc_sitc_old():
    cw = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'psc_naics' / 'DLA NAICS to FSC or PSC Cross Reference.xlsx', dtype=str
    )
    cw = cw[~cw['FSC'].isna() & ~cw['NAICS Code'].isna()]
    cw = cw[['NAICS Code', 'NAICS Description', 'FSC']]
    cw = cw[cw['NAICS Code'] != 'NONE'].copy()
    cw.columns = ['naics12', 'naics_desc', 'fsc']
    cw['naics12'] = cw['naics12'].map(lambda x: '314120' if x == '314129' else x)

    cw_naics = pd.read_csv(
        OUTPUT_PATH / 'crosswalks' / 'naics12_naics97.csv', dtype={'naics12': 'str', 'naics97': 'str'}
    )
    cw = merge_df(cw, cw_naics, on='naics12', how='left')
    cw = cw[['fsc', 'naics97', 'wt_mappings']]
    cw.to_csv(OUTPUT_PATH / 'crosswalks' / 'fsc_naics97.csv', index=False)

    cw_mp = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'sitc_naics' / 'commodity_translation_wizard.xlsb',
        dtype=str,
        sheet_name='Import Concordance'
    )
    cw_mp = cw_mp[~cw_mp['naics'].isna()]
    cw_mp = cw_mp[cw_mp['year'].map(lambda x: int(x) >= 1997 and int(x) <= 2001)]
    cw_xp = pd.read_excel(
        DATA_PATH / 'crosswalks' / 'sitc_naics' / 'commodity_translation_wizard.xlsb',
        dtype=str,
        sheet_name='Export Concordance'
    )
    cw_xp = cw_xp[~cw_xp['naics'].isna()]
    cw_xp = cw_xp[cw_xp['year'].map(lambda x: int(x) >= 1997 and int(x) <= 2001)]
    cw_tr = pd.concat([cw_mp, cw_xp], axis=0, ignore_index=True)
    cw_tr = cw_tr[['naics', 'sitc']]
    cw_tr['naics'] = cw_tr['naics'].map(lambda x: x.replace('X', ''))
    naics_list = list(cw['naics97'].unique())
    cw_tr['naics'] = cw_tr['naics'].map(lambda x: [i for i in naics_list if i.startswith(x)])
    cw_tr = cw_tr.apply(lambda x: x.explode()).reset_index()
    cw_tr['naics_count'] = cw_tr.groupby('naics')['index'].transform('count')
    cw_tr['pair_count'] = cw_tr.groupby(['naics', 'sitc'])['index'].transform('count')
    cw_tr['wgt'] = cw_tr['pair_count'] / cw_tr['naics_count']
    cw_tr = cw_tr.groupby(['naics', 'sitc'])['wgt'].first().reset_index()
    cw_tr.columns = ['naics97', 'sitc3', 'wgt']
    cw_tr['sitc3'] = cw_tr['sitc3'].map(lambda x: x[0:4] if x[-1] == '0' else x)

    cw_sitc = pd.read_excel(DATA_PATH / 'crosswalks' / 'sitc_naics' / 'SITC3-SITC2.xls', sheet_name='S3S2', dtype='str')
    cw_sitc.columns = ['sitc3', 'sitc2']
    cw_sitc['sitc3'] = cw_sitc['sitc3'].map(lambda x: x.replace('.', ''))
    cw_sitc['sitc2'] = cw_sitc['sitc2'].map(lambda x: x.replace('.', ''))
    cw_sitc = merge_df(cw_tr, cw_sitc, on='sitc3', how='inner')
    cw_sitc['sitc'] = cw_sitc['sitc2'].map(lambda x: x[:4])
    cw_sitc['wgt_naics'] = cw_sitc.groupby(['naics97'])['wgt'].transform('sum')
    cw_sitc['wgt_pair'] = cw_sitc.groupby(['naics97', 'sitc'])['wgt'].transform('sum')
    cw_sitc['wt_mapping'] = cw_sitc['wgt_pair'] / cw_sitc['wgt_naics']
    cw_sitc = cw_sitc.groupby(['naics97', 'sitc'])['wt_mapping'].first().reset_index()

    cw = merge_df(cw, cw_sitc, on='naics97', how='inner')
    cw['wgt'] = cw['wt_mappings'] * cw['wt_mapping']
    cw['wgt_fsc'] = cw.groupby(['fsc'])['wgt'].transform('sum')
    cw['wgt_pair'] = cw.groupby(['fsc', 'sitc'])['wgt'].transform('sum')
    cw['wgt'] = cw['wgt_pair'] / cw['wgt_fsc']
    cw = cw.groupby(['fsc', 'sitc'])['wgt'].first().reset_index()
    cw = cw[['fsc', 'sitc', 'wgt']]
    cw.sort_values('fsc', inplace=True)
    cw.to_csv(OUTPUT_PATH / 'crosswalks' / 'fsc_sitc.csv', index=False)


def empty_build_china_io_bea():
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'chinaiosector18_naics12.csv', dtype={'naics12': 'str', 'w': 'float'})
    bea_cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics12_bea12.csv', dtype={'wgt': 'float'})
    cw_new = merge_df(cw, bea_cw, on='naics12', how='left')
    cw_new['wgt'] = cw_new['wgt'] * cw_new['w']
    cw_new = cw_new.groupby(['iosector_code', 'bea12'])['wgt'].sum().reset_index()
    cw_new.to_csv(OUTPUT_PATH / 'crosswalks' / 'chinaiosector18_bea12.csv', index=False)


def main():
    # build_hs12_names()
    # build_hs_versions(base='hs12')
    # build_sitc75_hs12()
    # build_naics()
    # build_naics_bea()
    # build_hs_naics12()
    # build_hs_naics12_4digit()
    # build_psc_naics12()
    # build_sic_naics12()
    # build_nace2_naics12()
    build_naics12_sitc75()
    # empty_build_fsc_sitc()
    # empty_build_china_io_bea()
    # empty_build_hs10_naics12()


if __name__ == '__main__':
    main()

