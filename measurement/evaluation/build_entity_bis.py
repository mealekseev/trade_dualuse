### Build entity lists combining BIS and Orbis

import numpy as np
import pandas as pd
from pathlib import Path
import re
from collections import Counter

import Levenshtein
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import linear_kernel

from settings import DATA_PATH, STATS_PATH, OUTPUT_PATH, EXTERNAL_PATH, output
from utilities import merge_df, divide_robust


def get_entities():
    date_list = pd.read_csv(DATA_PATH / 'measurement' / 'policy_lists' / 'bis' / 'dates_entity_list.csv')
    date_list['date'] = date_list['date'].map(
        lambda x: '20' + x.split('/')[2] + '-' + x.split('/')[0].zfill(2) + '-' + x.split('/')[1].zfill(2))
    for d in date_list['date'].tolist():
        url = (f'https://www.ecfr.gov/api/renderer/v1/content/enhanced/'
           + d
           + '/title-15?subtitle=B&chapter=VII&subchapter=C&part=744&appendix=Supplement%20No.%204%20to%20Part%20744')
        print('entity: ' + d)
        path = DATA_PATH / 'measurement' / 'policy_lists' / 'bis' / f'entity-{d}.csv'
        if path.exists() or d == '2020-08-13':
            continue

        df = pd.read_html(url, flavor='bs4')[0]
        df.to_csv(DATA_PATH / 'measurement' / 'policy_lists' / 'bis' /  f'entity-{d}.csv', index=False)

    date_list = pd.read_csv(DATA_PATH / 'measurement' / 'policy_lists' / 'bis' / 'dates_military_end_user.csv')
    date_list['date'] = date_list['date'].map(
        lambda x: '20' + x.split('/')[2] + '-' + x.split('/')[0].zfill(2) + '-' + x.split('/')[1].zfill(2))
    for d in date_list['date'].tolist():
        url = (f'https://www.ecfr.gov/api/renderer/v1/content/enhanced/'
            + d
            + '/title-15?subtitle=B&chapter=VII&subchapter=C&part=744&appendix=Supplement%20No.%207%20to%20Part%20744')
        print('meu: ' + d)
        path = DATA_PATH / 'measurement' / 'policy_lists' / 'bis' / f'meu-{d}.csv'
        if path.exists() or d <= '2020-08-13':
            continue

        df = pd.read_html(url, flavor='bs4')[0]
        df.to_csv(DATA_PATH / 'measurement' / 'policy_lists' / 'bis' / f'meu-{d}.csv', index=False)


def list_orbis_format():
    dpl = pd.read_csv(DATA_PATH / 'measurement' / 'policy_lists' / 'bis' / 'dpl.txt', sep='\t')
    dpl = dpl[['Name', 'Street_Address', 'City', 'State', 'Country', 'Postal_Code']]
    dpl.columns = ['name', 'street_address', 'city', 'state', 'country', 'postal_code']
    dpl['entity_type'] = 'dpl'

    admin_debarred = pd.read_csv(DATA_PATH / 'measurement' / 'policy_lists' / 'bis' / 'Admin Debarred Parties_06.01.23.csv')
    admin_debarred = admin_debarred[['Name']]
    admin_debarred.columns = ['name']
    admin_debarred['entity_type'] = 'admin_debarred'

    stat_debarred = pd.read_csv(DATA_PATH / 'measurement' / 'policy_lists' / 'bis' / 'Stat Debarred Parties_20240626.csv')
    stat_debarred['name'] = stat_debarred['Party Name']
    stat_debarred = stat_debarred[['name']]
    stat_debarred['entity_type'] = 'stat_debarred'

    unverified = pd.read_html(DATA_PATH / 'measurement' / 'policy_lists' / 'bis' / 'unverified_list.html', flavor='bs4')[0]
    unverified['Country'] = unverified['Country'].ffill()
    unverified['Name'] = unverified['Listed person and address'].map(lambda x: x.split(',')[0])
    unverified['Address'] = unverified['Listed person and address'].map(lambda x: ', '.join(x.split(', ')[1:]))
    unverified = unverified[['Name', 'Address', 'Country']]
    unverified.columns = ['name', 'address', 'country']
    unverified['entity_type'] = 'unverified'

    path = DATA_PATH / 'measurement' / 'policy_lists' / 'bis'
    files = sorted(path.glob('entity*.csv'))
    entity_list = []
    for f in files[-1:]:
        entity = pd.read_csv(f)
        entity = entity[
            ~entity['Country'].map(
                lambda x: str(x).startswith('1') | str(x).startswith('2')
                          | str(x).startswith('3') | str(x).startswith('4'))
        ].copy()
        series = entity['Entity'].map(lambda x: str(x).startswith('For all items')).copy()
        entity.loc[series, 'Entity'] = entity.loc[series, 'Country']
        entity.loc[series, 'Country'] = np.nan
        if sum(entity['Country'].map(lambda x: str(x).startswith('Jo'))) > 0:
            raise("this")
        entity['date'] = f.stem.replace('entity-', '')
        entity_list.append(entity)
    entity = pd.concat(entity_list)
    entity = entity[~entity['Entity'].isna()].copy()
    entity['entity'] = entity['Entity']
    entity['country'] = entity['Country'].ffill()
    entity['name'] = entity['entity'].map(lambda x: str(x).split(',')[0])
    entity['address'] = entity['entity'].map(lambda x: ', '.join(str(x).split(', ')[1:]))
    entity = entity[['name', 'address', 'country']]
    entity = entity[~entity['name'].isin(
        ['Subordinate instituion:', 'Subordinate institution:', '[Reserved]', '[RESERVED]'])].copy()
    entity['entity_type'] = 'entity'

    meu_list = []
    files = sorted(path.glob('meu*.csv'))
    for f in files[-1:]:
        meu = pd.read_csv(f)
        meu['date'] = f.stem.replace('meu-', '')
        meu_list.append(meu)
    meu = pd.concat(meu_list)
    meu['entity'] = meu['Entity']
    meu['country'] = meu['Country'].ffill()
    meu['name'] = meu['entity'].map(lambda x: str(x).split(',')[0])
    meu['address'] = meu['entity'].map(lambda x: ', '.join(str(x).split(', ')[1:]))
    meu = meu[['name', 'address', 'country']]
    meu = meu[~meu['name'].isin(
        ['Subordinate instituion:', 'Subordinate institution:', '[Reserved]', '[RESERVED]'])].copy()
    meu['entity_type'] = 'meu'

    df = pd.concat([dpl, admin_debarred, stat_debarred, unverified, meu, entity], ignore_index=True)
    df = df.drop_duplicates(ignore_index=True)
    df['id'] = range(len(df))
    df = df[['id', 'name', 'address', 'country', 'entity_type']]

    def fix_country(x):
        x_list = str(x).split(' ')
        cty = ' '.join([col[0] + col[1:].lower() for col in x_list])
        return cty

    df['country'] = df['country'].map(lambda x: fix_country(x))
    country_dict = {
        'Nan': '',
        'Russia': 'Russian Federation',
        'China, People\'s Republic Of': 'China',
        'Us': 'United States of America',
        'Iran': 'Islamic Republic of Iran',
        'Ae': 'United Arab Emirates',
        'Crimea Region of Ukraine': 'Ukraine',
        'Syria': 'Syrian Arab Republic',
        'Gb': 'United Kingdom',
        'Burma': 'Myanmar/Burma',
        'Ir': 'Islamic Republic of Iran',
        'Fr': 'France',
        'Ru': 'Russian Federation',
        'Iq': 'Iraq',
        'Cn': 'China',
        'Cy': 'Cyprus',
        'De': 'Germany',
        'Taiwan': 'Taiwan, China',
        'Tr': 'Turkey',
        'Mx': 'Mexico',
        'Ca': 'Canada',
        'Türkiye': 'Turkey',
        'Ly': 'Libya',
        'Hk': 'Hong Kong SAR, China',
        'Lb': 'Lebanon',
        'Ch': 'Switzerland',
        'Jo': 'Jordan',
        'South Korea': 'Republic of Korea',
        'Th': 'Thailand',
        'Tw': 'Taiwan, China',
        'Id': 'Indonesia',
        'Rs': 'Serbia',
        'Sy': 'Syrian Arab Republic',
        'Sg': 'Singapore',
        'Sa': 'Saudi Arabia',
        'Lu': 'Luxembourg',
        'Vg': 'Virgin Islands (British)',
        'Es': 'Spain',
        'Bg': 'Bulgaria',
        'Se': 'Sweden',
        'At': 'Austria',
        'By': 'Belarus',
        'In': 'India',
        'Nm': 'United States of America',
        'Laos': 'Lao People\'s Democratic Republic',
        'Li': 'Liechtenstein',
        'Ua': 'Ukraine',
        'Mt': 'Malta'
    }
    df['country'] = df['country'].map(lambda x: country_dict[x] if x in country_dict.keys() else x)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'evaluation' / 'bis_list.csv', index=False)


def count_orbis():
    cols = ['bvdid', 'NAME_INTERNAT', 'CITY_INTERNAT', 'COUNTRY', 'NACEPCOD2', 'NAICSPCOD2017', 'USSICPCOD']
    l = 0
    chunk_size = 1_000_000
    ix = 0

    dict_countries = {
        'Virgin Islands (British)': 'United Kingdom',
        'Puerto Rico': 'United States of America',
        'IN': 'India',
        'BR': 'Brazil',
        'TZ': 'Tanzania',
        'KR': 'Republic of Korea',
        'MX': 'Mexico',
        'TR': 'Turkey',
        'US': 'United States of America',
        'BE': 'Belgium',
        'CN': 'China',
        'LU': 'Luxembourg',
        'KV': 'Kosovo',
        'IT': 'Italy',
        'IE': 'Ireland',
        'TW': 'Taiwan, China',
        'FI': 'Finland',
        'GR': 'Greece',
        'CR': 'Costa Rica'
    }

    dict_counter = {}
    for chunk in pd.read_csv(
            EXTERNAL_PATH / 'data' / 'measurement' / 'orbis' / 'orbis_entities.csv', usecols=cols, chunksize=chunk_size,
            dtype={'NACEPCOD2': 'str', 'NAICSPCOD2017': 'str', 'USSICPCOD': 'str'}
    ):
        print(l)
        l += (chunk_size / 1000)
        c = 'United States of America'
        chunk = chunk[(~chunk['NAME_INTERNAT'].isna()) & (~chunk['COUNTRY'].isna())].copy()
        chunk['COUNTRY'] = chunk['COUNTRY'].map(
            lambda x: dict_countries[x] if x in dict_countries.keys()
            else re.search(r"\((.*?)\)", x).group(1) if re.search(r"\((.*?)\)", x)
            else x
        )
        country_count = chunk.groupby('bvdid').first()['COUNTRY'].value_counts().reset_index()
        d = dict(zip(country_count['index'].values, country_count['COUNTRY'].values))
        dict_counter = dict(Counter(dict_counter) + Counter(d))
    country_count = pd.DataFrame(dict_counter.items(), columns=['country', 'count'])
    country_count.sort_values('count', ascending=False, inplace=True)
    country_count.to_csv(OUTPUT_PATH / 'measurement' / 'evaluation' / 'country_count.csv', index=False)


def lookup_orbis():
    df_keys = pd.read_csv(OUTPUT_PATH / 'measurement' / 'evaluation' / 'bis_list.csv')
    df_keys['country'] = df_keys['country'].fillna('')

    def clean_company_name(s):
        s = s.upper()
        s = re.sub(r'[^a-zA-Z0-9 ]', '', s)
        words_to_remove = [
            'INC', 'CORP', 'COMP', 'LLC', 'LTD', 'LIMITED', 'CORPORATION', 'COMPANY', 'CO', 'SA', 'SAS', 'SRL']
        for word in words_to_remove:
            s = re.sub(r'\b' + re.escape(word) + r'\b', '', s, flags=re.IGNORECASE)
        s = re.sub(r'\s+', ' ', s).strip()
        return s

    df_keys['name'] = df_keys['name'].map(lambda x: clean_company_name(x))
    vectorizer = TfidfVectorizer()
    tfidf_matrix = vectorizer.fit_transform(df_keys['name'])
    country_list = list(df_keys['country'].unique())
    df_keys_dict = {}
    c = country_list[0]
    for c in country_list:
        df_country = df_keys[(df_keys['country'] == c) | (df_keys['country'].isna())].copy()
        tfidf_matrix = vectorizer.transform(df_country['name'])
        df_keys_dict[c] = [df_keys[(df_keys['country'] == c) | (df_keys['country'].isna())].copy(), tfidf_matrix.copy()]

    def ld(str1, str2):
        l = Levenshtein.distance(str1, str2)
        ml = max(len(str1), len(str2))
        return (1 - l / ml)

    cols = ['bvdid', 'NAME_INTERNAT', 'CITY_INTERNAT', 'COUNTRY', 'NACEPCOD2', 'NAICSPCOD2017', 'USSICPCOD']
    l = 0
    chunk_size = 1_000_000
    ix = 0
    for chunk in pd.read_csv(
            EXTERNAL_PATH / 'data' / 'measurement' / 'orbis' / 'orbis_entities.csv', usecols=cols, chunksize=chunk_size,
            dtype={'NACEPCOD2': 'str', 'NAICSPCOD2017': 'str', 'USSICPCOD': 'str'}
    ):
        l += (chunk_size / 1000)
        c = 'United States of America'
        chunk = chunk[~chunk['NAME_INTERNAT'].isna()].copy()
        chunk['NAME_INTERNAT'] = chunk['NAME_INTERNAT'].map(lambda x: clean_company_name(x))
        for c in country_list:
            print(l, c, ix)
            df = df_keys_dict[c][0]
            tfidf_matrix1 = df_keys_dict[c][1]
            chunk_c = chunk[((chunk['COUNTRY'] == c) | (chunk['COUNTRY'].isna()))].copy()
            if len(chunk_c) == 0:
                continue
            chunk_c['id_helper'] = range(len(chunk_c))
            tfidf_matrix2 = vectorizer.transform(chunk_c['NAME_INTERNAT'])
            cos = linear_kernel(tfidf_matrix1, tfidf_matrix2)

            df = df.assign(best_match_index=cos.argmax(axis=1))
            df = df.assign(best_match_score=cos.max(axis=1))
            df = pd.merge(df, chunk_c, left_on='best_match_index', right_on='id_helper')
            df = df[df['best_match_score'] >= 0.8].copy()
            if len(df) > 0:
                df['ld'] = df[['name', 'NAME_INTERNAT']].apply(lambda x: ld(*x), axis=1)
                vectorizer_new = TfidfVectorizer()
                tfidf_matrix2 = vectorizer_new.fit_transform(chunk_c['NAME_INTERNAT'])
                tfidf_matrix1 = vectorizer_new.transform(df['name'])
                reverse_match_score = [
                    linear_kernel(tfidf_matrix1, tfidf_matrix2)[i, list(df['best_match_index'])[i]]
                    for i in range(len(df))
                ]
                df['reverse_match_index'] = reverse_match_score
                df.to_csv(EXTERNAL_PATH / 'output' / 'measurement' / 'orbis' / f'orbis_entities_{ix}.csv', index=False)
                ix = ix + 1


def merge_orbis():
    path = EXTERNAL_PATH / 'output' / 'measurement' / 'orbis'
    files = sorted(path.glob('orbis_*.csv'))
    df_list = []
    for f in files:
        df = pd.read_csv(f, dtype={'NACEPCOD2': 'str', 'NAICSPCOD2017': 'str', 'USSICPCOD': 'str'})
        df_list.append(df.copy())
    df = pd.concat(df_list, axis=0, ignore_index=True)
    df['best_match_score_max'] = df.groupby(['id'])['best_match_score'].transform(max)
    df = df[df['best_match_score'] == df['best_match_score_max']].copy()
    df['reverse_match_index_max'] = df.groupby(['id'])['reverse_match_index'].transform(max)
    df = df[df['reverse_match_index'] == df['reverse_match_index_max']].copy()
    df = df[df['reverse_match_index'] > 0.8].copy()
    df['industry_present'] = (~(df['NACEPCOD2'].isna() & df['NAICSPCOD2017'].isna() & df['USSICPCOD'].isna())).map(int)

    df_sanctions = pd.read_csv(OUTPUT_PATH / 'measurement' / 'evaluation' / 'bis_list.csv')
    df_sanctions = merge_df(
        df_sanctions,
        df[[
            'id', 'bvdid', 'NAME_INTERNAT', 'CITY_INTERNAT', 'COUNTRY', 'NACEPCOD2', 'NAICSPCOD2017', 'USSICPCOD',
            'industry_present']],
        on='id', how='outer', keep_merge=True
    )
    df_sanctions.to_csv(OUTPUT_PATH / 'measurement' / 'evaluation' / 'bis_match_check.csv', index=False)
    df_sanctions['matched'] = (df_sanctions['_merge'] == 'both').map(int)

    # quick stats summary
    sanctions_stats = df_sanctions.groupby(
        ['id', 'entity_type', 'country'])['industry_present', 'matched'].max().reset_index()
    sanctions_stats['industry_present'].fillna(0.0, inplace=True)
    entity_stats = sanctions_stats.groupby(['entity_type'])['industry_present', 'matched'].mean().reset_index()
    country_stats = sanctions_stats.groupby(['country'])['industry_present', 'matched'].mean().reset_index()
    entity_country_stats = sanctions_stats.groupby(
        ['entity_type', 'country'])['industry_present', 'matched'].mean().reset_index()
    entity_stats.to_csv(STATS_PATH / 'measurement' / 'evaluation' / 'bis' / 'entity_stats.csv', index=False)
    country_stats.to_csv(STATS_PATH / 'measurement' / 'evaluation' / 'bis' / 'country_stats.csv', index=False)
    entity_country_stats.to_csv(
        STATS_PATH / 'measurement' / 'evaluation' / 'bis' / 'entity_country_stats.csv', index=False)

    df_sanctions = df_sanctions[df_sanctions['industry_present'] == 1].copy()
    df_sanctions['is_naics'] = ~df_sanctions['NAICSPCOD2017'].isna()
    df_sanctions['is_naics'] = df_sanctions.groupby('id')['is_naics'].transform('max')
    df_sanctions['is_ussic'] = ~df_sanctions['USSICPCOD'].isna()
    df_sanctions['is_ussic'] = df_sanctions.groupby('id')['is_ussic'].transform('max')
    df_sanctions['is_nace'] = ~df_sanctions['NACEPCOD2'].isna()
    df_sanctions['is_nace'] = df_sanctions.groupby('id')['is_nace'].transform('max')

    df_naics = df_sanctions[(df_sanctions['is_naics'] == 1) & ~df_sanctions['NAICSPCOD2017'].isna()][
        ['id', 'name', 'country', 'entity_type', 'NAICSPCOD2017']].copy()
    df_naics['wgt'] = 1 / df_naics.groupby(['id'])['id'].transform('count')
    df_ussic = df_sanctions[
        (df_sanctions['is_naics'] == 0) & (df_sanctions['is_ussic'] == 1) & ~df_sanctions['USSICPCOD'].isna()
    ][['id', 'name', 'country', 'entity_type', 'USSICPCOD']].copy()
    df_ussic['wgt'] = 1 / df_ussic.groupby(['id'])['id'].transform('count')
    df_nace = df_sanctions[
        (df_sanctions['is_naics'] == 1) & (df_sanctions['is_naics'] == 0) & (df_sanctions['is_ussic'] == 0)
        & ~df_sanctions['NACEPCOD2'].isna()
    ][['id', 'name', 'country', 'entity_type', 'NACEPCOD2']].copy()
    df_nace['wgt'] = 1 / df_nace.groupby('id')['id'].transform('count')

    cw_17 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics17_naics12.csv', dtype={'naics17': 'str', 'naics12': 'str'})
    cw_22 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'naics22_naics12.csv', dtype={'naics22': 'str', 'naics12': 'str'})
    cw_12 = list(cw_17['naics12'].unique())
    df_naics17 = df_naics[df_naics['NAICSPCOD2017'].map(lambda x: x in cw_17['naics17'].tolist())].copy()
    df_naics22 = df_naics[df_naics['NAICSPCOD2017'].map(
        lambda x: not x in cw_17['naics17'].tolist() and x in cw_22['naics22'].tolist())].copy()
    df_naics12 = df_naics[df_naics['NAICSPCOD2017'].map(lambda x: len(x) < 6)].copy()
    assert len(df_naics17) + len(df_naics22) + len(df_naics12) == len(df_naics)

    df_naics17 = merge_df(df_naics17, cw_17, left_on='NAICSPCOD2017', right_on='naics17', how='left', keep_merge=True)
    df_naics22 = merge_df(df_naics22, cw_22, left_on='NAICSPCOD2017', right_on='naics22', how='left', keep_merge=True)
    df_naics12['naics12'] = df_naics12['NAICSPCOD2017'].map(lambda x: [n for n in cw_12 if n.startswith(x)])
    df_naics12['wt_mappings'] = 1 / df_naics12['naics12'].map(len)
    df_naics17['wgt'] = df_naics17['wgt'] * df_naics17['wt_mappings']
    df_naics22['wgt'] = df_naics22['wgt'] * df_naics22['wt_mappings']
    df_naics12['wgt'] = df_naics12['wgt'] * df_naics12['wt_mappings']
    df_naics12 = df_naics12.explode('naics12')
    df_naics = pd.concat([df_naics17, df_naics22, df_naics12], ignore_index=True)
    df_naics = df_naics[['id', 'name', 'country', 'entity_type', 'naics12', 'wgt']]

    cw_sic_naics = pd.read_csv(
        OUTPUT_PATH / 'crosswalks' / 'sic_combined_naics12.csv', dtype={'sic': str, 'naics12': str})
    df_ussic = merge_df(df_ussic, cw_sic_naics, left_on='USSICPCOD', right_on='sic', how='left', keep_merge=False)
    df_ussic = df_ussic[~df_ussic['naics12'].isna()].copy()
    df_ussic['wgt'] = df_ussic['wgt'] * df_ussic['wt_mappings']
    df_ussic = df_ussic[['id', 'name', 'country', 'entity_type', 'naics12', 'wgt']]

    cw_nace2 = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'nace2_naics12.csv', dtype={'nace': str, 'naics12': str})
    df_nace = merge_df(df_nace, cw_nace2, left_on='NACEPCOD2', right_on='nace', how='left', keep_merge=True)
    assert (df_nace['_merge'] == 'left_only').sum() == 0
    df_nace['wgt'] = df_nace['wgt'] * df_nace['wgt_nace2']
    df_nace = df_nace[['id', 'name', 'country', 'entity_type', 'naics12', 'wgt']]

    df_res = pd.concat([df_naics, df_ussic, df_nace], ignore_index=True)
    print(len(df_sanctions['id'].unique()) - len(df_res['id'].unique()))

    cent = pd.read_csv(OUTPUT_PATH / 'measurement' / 'centrality' / 'centrality_naics.csv')
    cent = cent[['sector', 'C_M']]
    cent.columns = ['naics12', 'C_M']
    cent['rank_C_M'] = (cent['C_M'].rank() - 1) / cent.shape[0]
    df_res = merge_df(df_res, cent, on='naics12', how='left', keep_merge=False)
    df_res.to_csv(OUTPUT_PATH / 'measurement' / 'evaluation' / 'bis_res.csv', index=False)


def main():
    get_entities()
    list_orbis_format()
    count_orbis()
    lookup_orbis()
    merge_orbis()


if __name__ == '__main__':
    main()

