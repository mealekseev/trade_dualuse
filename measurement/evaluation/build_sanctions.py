""" Read open sanctions data from the web """

import numpy as np
import pandas as pd
from pathlib import Path
import xml.etree.ElementTree as ET
from collections import Counter
import os
import json

from settings import DATA_PATH, OUTPUT_PATH, output
from utilities import merge_df
import requests


EXTERNAL_PATH = Path('/Volumes/Seagate/trade_defense')


def read_data():
    # df = pd.read_csv(EXTERNAL_PATH / 'open_sanctions' / 'statements.csv', nrows=10_000_000)
    schema_list = {
        'Address': False,
        'Airplane': False,
        'Associate': False,
        'Company': True,
        'CryptoWallet': False,
        'Directorship': False,
        'Employment': False,
        'Family': False,
        'Identification': False,
        'LegalEntity': True,
        'Membership': False,
        'Occupancy': False,
        'Organization': True,
        'Ownership': False,
        'Passport': False,
        'Person': False,
        'Position': False,
        'PublicBody': False,
        'Representation': False,
        'Sanction': True,
        'Security': True,
        'Succession': False,
        'UnknownLink': True,
        'Vessel': False
    }
    yes_list = []
    for (k, v) in schema_list.items():
        if v:
            yes_list.append(k)
    l = 0
    chunk_size = 100_000
    df = pd.DataFrame()
    for chunk in pd.read_csv(EXTERNAL_PATH / 'open_sanctions' / 'statements.csv', chunksize=chunk_size):
        print(l)
        l += (chunk_size / 1000)
        if len(chunk[~chunk['schema'].isin(schema_list.keys())]) > 0:
            print(chunk['schema'].value_counts())
            raise ValueError('Unknown schema')
        chunk = chunk[chunk['schema'].isin(yes_list)].copy()
        if len(df) == 0:
            df = chunk.copy()
        else:
            df = pd.concat([df, chunk], ignore_index=True)
    df.to_csv(EXTERNAL_PATH / 'open_sanctions' / 'statements_filtered.csv', index=False)


def filter_rus():
    df = pd.read_csv(EXTERNAL_PATH / 'open_sanctions' / 'statements_filtered.csv')
    egrul = df.loc[df['dataset'] == 'ext_ru_egrul'].copy()
    for v in ['inn', 'ogrn', 'kpp']:
        egrul.loc[egrul['prop'] == f'{v}Code', v] = egrul.loc[egrul['prop'] == f'{v}Code', 'value']
        egrul[v] = egrul.groupby('canonical_id')[v].transform(lambda x: x.fillna(method='ffill'))
        egrul[v] = egrul.groupby('canonical_id')[v].transform(lambda x: x.fillna(method='bfill'))
    egrul = egrul.groupby('canonical_id').first().reset_index()[['canonical_id', 'inn', 'ogrn', 'kpp']]

    df = df[df['dataset'] != 'ext_ru_egrul'].copy()
    df = df[df['dataset'] != 'ru_nsd_isin'].copy()
    df = merge_df(df, egrul, on='canonical_id', how='inner')
    df = df.groupby(['canonical_id', 'dataset']).first().reset_index()[['canonical_id', 'dataset', 'inn', 'ogrn', 'kpp']]
    df['nunique'] = df.groupby(['canonical_id'])['dataset'].transform('nunique')
    df = df[~((df['nunique'] > 1) & (df['dataset'] == 'default'))].copy()

    with open(EXTERNAL_PATH / 'open_sanctions' / 'index.json', 'r') as file:
        json_data = json.load(file)
    datasets = json_data['datasets']
    df_data = pd.DataFrame(datasets)
    df_data = df_data[['name', 'title']]
    df = merge_df(df, df_data, left_on='dataset', right_on='name', how='left', keep_merge=False)
    df.to_csv(EXTERNAL_PATH / 'open_sanctions' / 'statements_filtered_rus.csv', index=False)


def read_egrul():
    def list_files():
        xml_files = []
        for root, dirs, files in os.walk(EXTERNAL_PATH / 'open_sanctions' / 'egrul'):
            for file in files:
                if file.endswith('.XML'):
                    xml_files.append(os.path.join(root, file))
        return xml_files

    xml_list = list_files()
    for f in xml_list:
        output(f)
        csv_path = EXTERNAL_PATH / 'open_sanctions' / 'egrul_csv' / f.split('/')[-1].replace('.XML', '.csv')
        if csv_path.exists():
            continue
        df = pd.read_xml(f, encoding='windows-1251', attrs_only=True)
        col_group = list(df.columns)
        tree = ET.parse(f)
        root = tree.getroot()

        def extract_keys_values(element, path=''):
            data = []
            row = {}
            if element.attrib:  # If element has attributes, add them as keys
                for key, value in element.attrib.items():
                    row[f"{path}/{key}"] = value
            if element.text and element.text.strip():  # If element has text, add it as a value
                row[path] = element.text.strip()
            data.append(row)
            for child in element:
                data.extend(extract_keys_values(child, path=f"{path}/{child.tag}"))
            return data

        all_data = extract_keys_values(root)
        df = pd.DataFrame(all_data)
        # my_list = ['_'.join(col.split('/')[-5:]).strip('_') for col in df.columns]
        my_list = ['_'.join(col.split('/')).strip('_') for col in df.columns]
        counter = Counter(my_list)
        if max(counter.values()) > 1:
            raise('Error')
        df.columns = my_list
        df = df.fillna(method='bfill')
        df = df.fillna(method='ffill')
        col_group_new = list(df.columns[0:(len(col_group) + 1)])
        for j, col in enumerate(col_group_new[1:]):
            if (not col.endswith(col_group[j])):
                output(col_group)
                output(col_group_new)
        df = df.groupby(col_group_new).first().reset_index()
        df.to_csv(csv_path, index=False)


def gen_okved_naics_crosswalk():
    def list_files():
        xml_files = []
        for root, dirs, files in os.walk(EXTERNAL_PATH / 'open_sanctions' / 'egrul_csv'):
            for file in files:
                if file.endswith('.csv'):
                    xml_files.append(os.path.join(root, file))
        return xml_files

    csv_list = list_files()
    col_main = [
        'СвЮЛ_СвОКВЭД_СвОКВЭДОсн_КодОКВЭД',
        'СвЮЛ_СвОКВЭД_СвОКВЭДОсн_НаимОКВЭД',
        'СвЮЛ_СвОКВЭД_СвОКВЭДДоп_КодОКВЭД',
        'СвЮЛ_СвОКВЭД_СвОКВЭДДоп_НаимОКВЭД'
    ]
    df_all = pd.DataFrame()
    for j, p in enumerate(csv_list):
        print(f"{j} {j/len(csv_list)} {p}")
        try:
            df = pd.read_csv(p, usecols=col_main, dtype='str')
        except:
            df = pd.read_csv(p, dtype='str')
            if len([col for col in df.columns if 'ОКВЭД' in col]) == 0:
                continue
            if ('СвЮЛ_СвОКВЭД_СвОКВЭДОсн_КодОКВЭД' in df.columns
                    and 'СвЮЛ_СвОКВЭД_СвОКВЭДОсн_НаимОКВЭД' in df.columns
                    and not 'СвЮЛ_СвОКВЭД_СвОКВЭДДоп_КодОКВЭД' in df.columns
                    and not 'СвЮЛ_СвОКВЭД_СвОКВЭДДоп_НаимОКВЭД' in df.columns):
                df['СвЮЛ_СвОКВЭД_СвОКВЭДДоп_КодОКВЭД'] = df['СвЮЛ_СвОКВЭД_СвОКВЭДОсн_КодОКВЭД']
                df['СвЮЛ_СвОКВЭД_СвОКВЭДДоп_НаимОКВЭД'] = df['СвЮЛ_СвОКВЭД_СвОКВЭДОсн_НаимОКВЭД']
            else:
                print([col for col in df.columns if 'ОКВЭД' in col])
                raise("Error")
        df1 = df[['СвЮЛ_СвОКВЭД_СвОКВЭДОсн_КодОКВЭД', 'СвЮЛ_СвОКВЭД_СвОКВЭДОсн_НаимОКВЭД']]
        df1.columns = ['okved', 'okved_desc']
        df2 = df[['СвЮЛ_СвОКВЭД_СвОКВЭДДоп_КодОКВЭД', 'СвЮЛ_СвОКВЭД_СвОКВЭДДоп_НаимОКВЭД']]
        df2.columns = ['okved', 'okved_desc']
        df = pd.concat([df1, df2], ignore_index=True, axis=0)
        if len(df_all) == 0:
            df_all = df.copy()
        else:
            df_all = pd.concat([df_all, df], ignore_index=True, axis=0)
            df_all = df_all.drop_duplicates(ignore_index=True)
    df_all.sort_values('okved', inplace=True, ignore_index=True)
    df_all.to_csv(EXTERNAL_PATH / 'open_sanctions' / 'egrul_okved.csv', index=False)


def gen_crosswalk_centrality():
    # centrality dataset
    cent = pd.read_csv(OUTPUT_PATH / 'network_stats' / 'centrality_naics.csv')
    cent = cent[['sector', 'C_M']]
    cent.columns = ['naics2012', 'C_M']
    cent['rank_C_M'] = (cent['C_M'].rank() - 1) / cent.shape[0]

    # nace cw
    cw_nace11 = pd.read_csv(DATA_PATH / 'crosswalks' / 'unstats' / 'NACE11-ISIC31.txt')
    cw_nace11 = cw_nace11[['Nace11Code', 'ISIC31code']]
    cw_nace11.columns = ['nace', 'isic3_1']
    cw_nace11['wgt_nace'] = 1 / cw_nace11.groupby('nace')['isic3_1'].transform('count')

    cw_nace2 = pd.read_csv(DATA_PATH / 'crosswalks' / 'unstats' / 'NACE2_ISIC4.txt')
    cw_nace2 = cw_nace2[['NACE2code', 'ISIC4code']]
    cw_nace2.columns = ['nace', 'isic4']
    cw_nace2['wgt_nace2'] = 1 / cw_nace2.groupby('nace')['isic4'].transform('count')

    cw_nace1 = pd.read_csv(
        DATA_PATH / 'crosswalks' / 'unstats' / 'NACEISIC.TXT', dtype={'NACE Rev.1': 'str', 'ISIC Rev.3': 'str'}
    )
    cw_nace1.columns = ['nace', 'isic3']
    cw_nace1['wgt_nace1'] = 1 / cw_nace1.groupby('nace')['isic3'].transform('count')

    # okved split
    cw_okved = pd.read_csv(EXTERNAL_PATH / 'open_sanctions' / 'egrul_okved.csv')
    cw_okved = cw_okved.groupby('okved').first().reset_index()[['okved']]
    cw_okved['nace'] = cw_okved['okved'].str[0:5]
    cw_okved11 = cw_okved[cw_okved['nace'].map(lambda x: x in list(cw_nace11['nace']))].copy()
    cw_okved11 = merge_df(cw_okved11, cw_nace11, on='nace', how='left', keep_merge=False)
    cw_okved2 = cw_okved[cw_okved['nace'].map(
        lambda x: x in list(cw_nace2['nace']) and x not in list(cw_nace11['nace']))].copy()
    cw_okved2 = merge_df(cw_okved2, cw_nace2, on='nace', how='left', keep_merge=False)
    cw_okved1 = cw_okved[cw_okved['nace'].map(
        lambda x: x not in list(cw_nace2['nace']) and x not in list(cw_nace11['nace']))].copy()
    cw_okved1 = merge_df(cw_okved1, cw_nace1, on='nace', how='left', keep_merge=False)

    # okved2
    cw_isic4 = pd.read_csv(
        DATA_PATH / 'crosswalks' / 'unstats' / 'ISIC4-NAICS2012US.txt',
        dtype={'ISIC4Code': 'str', 'NAICS2012Code': 'str'}
    )
    cw_isic4 = cw_isic4[['ISIC4Code', 'NAICS2012Code']]
    cw_isic4.columns = ['isic4', 'naics2012']
    cw_isic4['wgt_isic4'] = 1 / cw_isic4.groupby('isic4')['naics2012'].transform('count')
    cw_isic4.loc[cw_isic4['wgt_isic4'] > 1.01, 'wgt_isic4'] = np.nan
    cw_isic4_plus3 = cw_isic4.copy()
    cw_isic4_plus3['isic4'] = cw_isic4_plus3['isic4'].str[0:3]
    cw_isic4_plus3['wgt_isic4'] = cw_isic4_plus3['wgt_isic4'] / cw_isic4_plus3.groupby('isic4')['wgt_isic4'].transform('sum')
    cw_isic4_plus3 = cw_isic4_plus3.groupby(['isic4', 'naics2012'])['wgt_isic4'].sum().reset_index()
    cw_isic4_plus2 = cw_isic4.copy()
    cw_isic4_plus2['isic4'] = cw_isic4_plus2['isic4'].str[0:2]
    cw_isic4_plus2['wgt_isic4'] = cw_isic4_plus2['wgt_isic4'] / cw_isic4_plus2.groupby('isic4')['wgt_isic4'].transform('sum')
    cw_isic4_plus2 = cw_isic4_plus2.groupby(['isic4', 'naics2012'])['wgt_isic4'].sum().reset_index()
    cw_isic4 = pd.concat([cw_isic4, cw_isic4_plus3, cw_isic4_plus2], ignore_index=True)
    cw_okved2 = merge_df(cw_okved2, cw_isic4, on='isic4', how='left', keep_merge=False)
    cw_okved2['wgt'] = cw_okved2['wgt_isic4'] * cw_okved2['wgt_nace2']
    cw_okved2 = cw_okved2.groupby(['okved', 'naics2012'])['wgt'].sum().reset_index()
    cw_okved2 = merge_df(cw_okved2, cent, on='naics2012', how='left', keep_merge=False)
    cw_okved2 = cw_okved2[['okved', 'wgt', 'C_M', 'rank_C_M']]

    # okved1.1
    cw_isic3_1 = pd.read_csv(
        DATA_PATH / 'crosswalks' / 'unstats' / 'ISIC31_ISIC4.txt', dtype={'ISIC31code': 'str', 'ISIC4code': 'str'}
    )
    cw_isic3_1 = cw_isic3_1[['ISIC31code', 'ISIC4code']]
    cw_isic3_1.columns = ['isic3_1', 'isic4']
    cw_isic3_1['wgt_isic3_1'] = 1 / cw_isic3_1.groupby('isic3_1')['isic4'].transform('count')
    cw_isic3_1 = merge_df(cw_isic3_1, cw_isic4, on='isic4', how='left', keep_merge=False)
    cw_isic3_1['wgt_isic3_1'] = cw_isic3_1['wgt_isic4'] * cw_isic3_1['wgt_isic3_1']
    cw_isic3_1 = cw_isic3_1[['isic3_1', 'naics2012', 'wgt_isic3_1']]
    cw_isic3_1_plus3 = cw_isic3_1.copy()
    cw_isic3_1_plus3['isic3_1'] = cw_isic3_1_plus3['isic3_1'].str[0:3]
    cw_isic3_1_plus3['wgt_isic3_1'] = cw_isic3_1_plus3['wgt_isic3_1'] / cw_isic3_1_plus3.groupby('isic3_1')['wgt_isic3_1'].transform('sum')
    cw_isic3_1_plus3 = cw_isic3_1_plus3.groupby(['isic3_1', 'naics2012'])['wgt_isic3_1'].sum().reset_index()
    cw_isic3_1_plus2 = cw_isic3_1.copy()
    cw_isic3_1_plus2['isic3_1'] = cw_isic3_1_plus2['isic3_1'].str[0:2]
    cw_isic3_1_plus2['wgt_isic3_1'] = cw_isic3_1_plus2['wgt_isic3_1'] / cw_isic3_1_plus2.groupby('isic3_1')['wgt_isic3_1'].transform('sum')
    cw_isic3_1_plus2 = cw_isic3_1_plus2.groupby(['isic3_1', 'naics2012'])['wgt_isic3_1'].sum().reset_index()
    cw_isic3_1 = pd.concat([cw_isic3_1, cw_isic3_1_plus3, cw_isic3_1_plus2], ignore_index=True)
    cw_okved11 = merge_df(cw_okved11, cw_isic3_1, on='isic3_1', how='left', keep_merge=False)
    cw_okved11['wgt'] = cw_okved11['wgt_isic3_1'] * cw_okved11['wgt_nace']
    cw_okved11 = cw_okved11.groupby(['okved', 'naics2012'])['wgt'].sum().reset_index()
    cw_okved11 = merge_df(cw_okved11, cent, on='naics2012', how='left', keep_merge=False)
    cw_okved11 = cw_okved11[['okved', 'wgt', 'C_M', 'rank_C_M']]

    # okved1
    cw_isic3 = pd.read_csv(
        DATA_PATH / 'crosswalks' / 'unstats' / 'ISIC_Rev_3-ISIC_Rev_3_1_correspondence.txt',
        dtype={'Rev3': 'str', 'Rev31': 'str'}
    )
    cw_isic3 = cw_isic3[['Rev3', 'Rev31']]
    cw_isic3.columns = ['isic3', 'isic3_1']
    cw_isic3['wgt_isic3'] = 1 / cw_isic3.groupby('isic3')['isic3_1'].transform('count')
    cw_isic3 = merge_df(cw_isic3, cw_isic3_1, on='isic3_1', how='left', keep_merge=False)
    cw_isic3['wgt_isic3'] = cw_isic3['wgt_isic3'] * cw_isic3['wgt_isic3_1']
    cw_isic3 = cw_isic3[['isic3', 'naics2012', 'wgt_isic3']]
    cw_isic3_plus3 = cw_isic3.copy()
    cw_isic3_plus3['isic3'] = cw_isic3_plus3['isic3'].str[0:3]
    cw_isic3_plus3['wgt_isic3'] = cw_isic3_plus3['wgt_isic3'] / cw_isic3_plus3.groupby('isic3')['wgt_isic3'].transform('sum')
    cw_isic3_plus3 = cw_isic3_plus3.groupby(['isic3', 'naics2012'])['wgt_isic3'].sum().reset_index()
    cw_isic3_plus2 = cw_isic3.copy()
    cw_isic3_plus2['isic3'] = cw_isic3_plus2['isic3'].str[0:2]
    cw_isic3_plus2['wgt_isic3'] = cw_isic3_plus2['wgt_isic3'] / cw_isic3_plus2.groupby('isic3')['wgt_isic3'].transform('sum')
    cw_isic3 = pd.concat([cw_isic3, cw_isic3_plus3, cw_isic3_plus2], ignore_index=True)
    cw_okved1 = merge_df(cw_okved1, cw_isic3, on='isic3', how='left', keep_merge=False)
    cw_okved1['wgt'] = cw_okved1['wgt_isic3'] * cw_okved1['wgt_nace1']
    cw_okved1 = cw_okved1.groupby(['okved', 'naics2012'])['wgt'].sum().reset_index()
    cw_okved1 = merge_df(cw_okved1, cent, on='naics2012', how='left', keep_merge=False)
    cw_okved1 = cw_okved1[['okved', 'wgt', 'C_M', 'rank_C_M']]

    cw = pd.concat([cw_okved1, cw_okved11, cw_okved2], ignore_index=True)
    cw.to_csv(EXTERNAL_PATH / 'open_sanctions' / 'centrality_okved.csv', index=False)


def gen_centrality_okved():
    df_sanctions = pd.read_csv(EXTERNAL_PATH / 'open_sanctions' / 'statements_filtered_rus.csv')
    inn_list = list(df_sanctions.loc[~df_sanctions['inn'].isna(), 'inn'])
    ogrn_list = list(df_sanctions.loc[~df_sanctions['ogrn'].isna(), 'ogrn'])
    # kpp_list = list(df_sanctions.loc[~df_sanctions['kpp'].isna(), 'kpp'])

    def list_files():
        xml_files = []
        for root, dirs, files in os.walk(EXTERNAL_PATH / 'open_sanctions' / 'egrul_csv'):
            for file in files:
                if file.endswith('.csv'):
                    xml_files.append(os.path.join(root, file))
        return xml_files

    csv_list = list_files()
    col_main = [
        'СвЮЛ_ОГРН',
        'СвЮЛ_ИНН',
        # 'СвЮЛ_КПП',
        'СвЮЛ_СвОКВЭД_СвОКВЭДОсн_КодОКВЭД',
        # 'СвЮЛ_СвОКВЭД_СвОКВЭДОсн_НаимОКВЭД',
        'СвЮЛ_СвОКВЭД_СвОКВЭДДоп_КодОКВЭД',
        # 'СвЮЛ_СвОКВЭД_СвОКВЭДДоп_НаимОКВЭД'
    ]
    df_all = pd.DataFrame()
    p = csv_list[0]
    for p in csv_list:
        print(p)
        try:
            df = pd.read_csv(p, usecols=col_main)
            df.columns = ['ogrn', 'inn', 'okved', 'okved_extra']
            df = df[(df['ogrn'].isin(ogrn_list) | (df['inn'].isin(inn_list)))].copy()
        except:
            df = pd.read_csv(p)
            if len([col for col in df.columns if 'ОКВЭД' in col]) == 0:
                print([col for col in df.columns if 'ОКВЭД' in col])
                continue
            if ('СвЮЛ_СвОКВЭД_СвОКВЭДОсн_КодОКВЭД' in df.columns
                    and 'СвЮЛ_СвОКВЭД_СвОКВЭДОсн_НаимОКВЭД' in df.columns
                    and not 'СвЮЛ_СвОКВЭД_СвОКВЭДДоп_КодОКВЭД' in df.columns
                    and not 'СвЮЛ_СвОКВЭД_СвОКВЭДДоп_НаимОКВЭД' in df.columns):
                df['СвЮЛ_СвОКВЭД_СвОКВЭДДоп_КодОКВЭД'] = df['СвЮЛ_СвОКВЭД_СвОКВЭДОсн_КодОКВЭД']
                df['СвЮЛ_СвОКВЭД_СвОКВЭДДоп_НаимОКВЭД'] = df['СвЮЛ_СвОКВЭД_СвОКВЭДОсн_НаимОКВЭД']
                continue
            raise("Error")
        if len(df_all) == 0:
            df_all = df.copy()
        else:
            df_all = pd.concat([df_all, df], ignore_index=True, axis=0)
            df_all = df_all.groupby(['ogrn', 'inn']).first().reset_index()

    df_sanctions['id'] = range(len(df_sanctions))
    df_base = df_sanctions.copy()
    df_list = []
    for col in ['ogrn', 'inn']:
        cols = [col, 'okved', 'okved_extra']
        df_wave = merge_df(df_base[~df_base[col].isna()].copy(), df_all[cols], on=col, how='inner', keep_merge=False)
        df_base = df_base[~(df_base['id'].isin(list(df_wave['id'])))].copy()
        df_list.append(df_wave.copy())
    df_new = pd.concat(df_list, ignore_index=True)

    df_new1 = df_new[['name', 'title', 'okved']].copy()
    df_new1['wgt'] = 0.75
    df_new2 = df_new[['name', 'title', 'okved_extra']].copy()
    df_new2.columns = ['name', 'title', 'okved']
    df_new2['wgt'] = 0.25
    df_new = pd.concat([df_new1, df_new2], axis=0, ignore_index=True)

    cent = pd.read_csv(EXTERNAL_PATH / 'open_sanctions' / 'centrality_okved.csv')
    df_new = merge_df(df_new, cent, on='okved', how='left', keep_merge=False)
    df_new['wgt'] = df_new['wgt_x'] * df_new['wgt_y']
    df_new = df_new[['name', 'title', 'wgt', 'C_M', 'rank_C_M']]
    df_new.to_csv(OUTPUT_PATH / 'open_sanctions' / 'centrality_sanctions.csv', index=False)


if __name__ == '__main__':
    # read_data()
    # filter_rus()
    # read_egrul()
    # gen_okved_naics_crosswalk()
    # gen_crosswalk_centrality()
    gen_centrality_okved()

