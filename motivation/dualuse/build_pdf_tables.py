""" Get tables from a pdf file """

import tabula
import pandas as pd

from settings import DATA_PATH, OUTPUT_PATH
from utilities import merge_df


def extract_critical_list():
    df_agg = []
    df_list = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'high_priority_feb_2024.pdf', pages='all', multiple_tables=True, lattice=True
    )
    df_new_list = []
    for i, df in enumerate(df_list):
        df = df_list[i].iloc[:, 0:2]
        if i == 1:
            df = df_list[1].iloc[:, [1, 3]]
        df.columns = ['hscode', 'local_desc']
        df['hscode'] = df['hscode'].map(lambda x: str(x).replace('.', '').ljust(6, '0'))
        if i < 2:
            df['tier'] = i + 1
            df['tier_full'] = str(i + 1)
        if i == 2:
            df['tier'] = i + 1
            df['tier_full'] = '3A'
        if i == 3:
            df['tier'] = i
            df['tier_full'] = '3B'
        if i == 4:
            df['tier'] = 4
            df['tier_full'] = '4A'
        if i == 5:
            df['tier'] = 4
            df['tier_full'] = '4B'
        df['date'] = '2024-02-01'
        df['date_text'] = '2024feb'
        df = df[['date', 'date_text', 'tier', 'tier_full', 'hscode', 'local_desc']]
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_agg.append(df_new.copy())

    df_list = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'high_priority_sep_2023.pdf', pages='all', multiple_tables=True, lattice=True
    )
    df_new_list = []
    for i, df in enumerate(df_list):
        df = df_list[i].iloc[:, 0:2]
        if i == 1:
            df = df_list[1].iloc[:, [1, 3]]
        if i == 3:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['hscode', 'local_desc']
        df['hscode'] = df['hscode'].map(lambda x: str(x).replace('.', '').ljust(6, '0'))
        if i < 2:
            df['tier'] = str(i + 1)
            df['tier_full'] = str(i + 1)
        if i == 2:
            df['tier'] = str(i + 1)
            df['tier_full'] = '3A'
        if i == 3:
            df['tier'] = str(i)
            df['tier_full'] = '3A'
        if i == 4:
            df['tier'] = '3'
            df['tier_full'] = '3B'
        if i == 5:
            df['tier'] = '4'
            df['tier_full'] = '4'
        df['date'] = '2023-09-01'
        df['date_text'] = '2023sep'
        df = df[['date', 'date_text', 'tier', 'tier_full', 'hscode', 'local_desc']]
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_agg.append(df_new.copy())

    df_list = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'high_priority_jun_2023.pdf', pages='all', multiple_tables=True, lattice=True
    )
    df_new_list = []
    for i, df in enumerate(df_list):
        df = df_list[i].iloc[:, 0:2]
        if i == 3:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        # print(i, df)
        df.columns = ['hscode', 'local_desc']
        df['hscode'] = df['hscode'].map(lambda x: str(x).replace('.', '').ljust(6, '0'))
        if i < 2:
            df['tier'] = str(i + 1)
            df['tier_full'] = str(i + 1)
        if i == 2:
            df['tier'] = str(i + 1)
            df['tier_full'] = '3'
        if i == 3:
            df['tier'] = str(3)
            df['tier_full'] = '3'
        if i == 4:
            df['tier'] = str(4)
            df['tier_full'] = '4'
        df['date'] = '2023-06-01'
        df['date_text'] = '2023jun'
        df = df[['date', 'date_text', 'tier', 'tier_full', 'hscode', 'local_desc']]
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_agg.append(df_new.copy())
    df_agg = pd.concat(df_agg, ignore_index=True)
    df_agg.to_csv(OUTPUT_PATH / 'motivation' / 'eu_commission' / 'critical_list.csv', index=False)

    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs22_hs12.csv', dtype={'hs22': str, 'hs12': str})
    df_agg = merge_df(df_agg, cw, how='left', left_on='hscode', right_on='hs22')
    df_agg['date_text_tier'] = df_agg['date_text'] + '_' + df_agg['tier'].map(str)
    df_agg = pd.get_dummies(df_agg, columns=['date_text'])
    df_agg = pd.get_dummies(df_agg, columns=['date_text_tier'])
    select_col = ['local_desc'] + [col for col in df_agg.columns if col.startswith('date_text')]
    df_agg = df_agg.groupby(['hs12'])[select_col].max().reset_index()

    def process_col(col):
        if col.startswith('date_text_tier'):
            return col.replace('date_text_tier', 'critical')
        if col.startswith('date_text'):
            return col.replace('date_text', 'critical')
        return col

    df_agg.columns = [process_col(col) for col in df_agg.columns]
    df_agg.to_csv(OUTPUT_PATH / 'motivation' / 'eu_commission' / 'critical_list_wide.csv', index=False)


def extract_chemicals():
    df_agg = []

    df_year = []
    df_list = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2023.pdf', pages='all', multiple_tables=True, lattice=True
    )
    df_new_list = []
    for i in range(19, 43):
        df = df_list[i]
        # print(df_list[i])
        if i != 19:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(
            lambda x: str(x).replace('.', '').replace('\r(subheading)', '00'))
        df['cn_code'] = df['cn_code'].map(
            lambda x: ['81092100', '81093100', '81099100'] if x == '81092100\r81093100\r81099100'
            else ['28441000', '28442000', '28443000', '28444100', '28444200', '28444300', '28444400', '28445000']
                if x == '2844 (heading)'
            else [x])
        df = df.explode('cn_code')
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'dualuse'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(44, 76):
        df = df_list[i]
        print(df_list[i])
        if i != 44:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', ''))
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'weapons'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(77, 105):
        df = df_list[i]
        print(df_list[i])
        if i != 77:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', ''))
        df = df[df['cn_code'] != 'Unnamed: 2'].copy()
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'military'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(106, 119):
        df = df_list[i]
        print(df_list[i])
        if i != 106:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', '').replace(
            '\r(subheading)', '00'))
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'hazardous'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    df1 = df_list[120].iloc[:, 1:5]
    df1.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df1['cn_code'] = df1['cn_code'].map(lambda x: str(x).replace('.', ''))
    df2 = pd.concat([
        pd.DataFrame([list(df_list[121].columns)], columns=df_list[121].columns),
        df_list[121].iloc[:, 0:5]
    ], ignore_index=True)
    df2.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df2['cn_code'] = df2['cn_code'].map(lambda x: str(x).replace('.', ''))
    df_new = pd.concat([df1, df2], ignore_index=True)
    df_new['year'] = 2023
    df_new['type'] = 'antitorture'
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(122, 125):
        df = df_list[i]
        print(df_list[i])
        if i != 122:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', '').replace(
            '\r(subheading)', '00'))
        df['cn_code'] = df['cn_code'].map(
            lambda x: ['81101000', '81102000', '81109000'] if x == '8110 (heading)'
            else [x]
        )
        df = df.explode('cn_code')
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'syria'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(126, 128):
        df = df_list[i]
        print(df_list[i])
        if i != 126:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', ''))
        df['cn_code'] = df['cn_code'].map(
            lambda x: ['81092100', '81093100', '81099100'] if x == '81092100\r81093100\r81099100'
            else [x]
        )
        df = df.explode('cn_code')
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'iran'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(129, 130):
        df = df_list[i]
        print(df_list[i])
        if i != 129:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', ''))
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'dprk'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(130, 145):
        df = df_list[i]
        print(df_list[i])
        if i != 130:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', ''))
        df = df[df['cn_code'] != 'N/A'].copy()
        df['cn_code'] = df['cn_code'].map(
            lambda x: ['81101000', '81102000', '81109000'] if x == 'Heading 8110'
            else [x]
        )
        df = df.explode('cn_code')
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'russia'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(130, 145):
        df = df_list[i]
        print(df_list[i])
        if i != 130:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', ''))
        df = df[df['cn_code'] != 'N/A'].copy()
        df['cn_code'] = df['cn_code'].map(
            lambda x: ['81101000', '81102000', '81109000'] if x == 'Heading 8110'
            else [x]
        )
        df = df.explode('cn_code')
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'russia'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(146, 147):
        df = df_list[i]
        print(df_list[i])
        if i != 146:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', '').replace(
            '310230 (subheading)', '31023000'))
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'explprec'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())

    df_new_list = []
    for i in range(148, 150):
        df = df_list[i]
        print(df_list[i])
        if i != 148:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
        df['cn_code'] = df['cn_code'].map(lambda x: str(x).replace('.', '').replace(
            '3102.30 (subheading)', '31023000'))
        assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
        df['year'] = 2023
        df['type'] = 'drugprec'
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_year.append(df_new.copy())
    df_year = pd.concat(df_year, ignore_index=True)
    df_agg.append(df_year.copy())

    df_year = []
    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='16-33', multiple_tables=False, lattice=False,
        guess=False, columns=[120.0, 400.0, 480.0]
    )[0].iloc[2:599, :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna()].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', '').replace(
        'Subhead 810600', '81060000').replace('Subhead 390720', '39072000').replace('Subhead 391190', '39119000'))
    df['cn_code'] = df['cn_code'].map(
        lambda x: ['28441000', '28442000', '28443000', '28444000', '28445000'] if x == 'Heading 2844' else
        ['39071000', '39072000', '39073000', '39074000', '39075000', '39076100', '39076900', '39077000',
         '39079100', '39079900'] if x == 'Heading 3907' else
        [x]
    )
    df = df.explode('cn_code')
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'dualuse'
    df_year.append(df.copy())

    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='35-61', multiple_tables=False, lattice=False,
        guess=False, columns=[100.0, 400.0, 450.0]
    )[0].iloc[3:949, :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna()].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', '').replace(
        'Subhead 390720', '39072000').replace('Subhead 391190', '39119000'))
    df = df.explode('cn_code')
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'weapons'
    df_year.append(df.copy())

    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='63-86', multiple_tables=False, lattice=False,
        guess=False, columns=[120.0, 400.0, 470.0]
    )[0].iloc[3:839, :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna() | (df['chemical_name'] == 'Poly(3-nitrato oxetane); PNO')].copy()
    df['cn_code'] = df['cn_code'].fillna('3907.20.00')
    df = df[df['cn_code'].map(lambda x: x != 'Subhead.' and x != '3907.20')].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', ''))
    df['cn_code'] = df['cn_code'].map(
        lambda x: ['39071000', '39072000', '39073000', '39074000', '39075000', '39076000', '39077000',
         '39079100', '39079900'] if x == 'Heading 3907' else [x]
    )
    df = df.explode('cn_code')
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'military'
    df_year.append(df.copy())

    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='89-99', multiple_tables=False, lattice=False,
        guess=False, columns=[150.0, 400.0, 450.0]
    )[0].iloc[3:359, :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna()].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', '').replace(
        'Subhead 280540', '28054000'))
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'hazardous'
    df_year.append(df.copy())

    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='100', multiple_tables=False, lattice=False,
        guess=False, area=[55, 0, 83, 100], relative_area=True, columns=[180.0, 320.0, 400.0]
    )[0].iloc[2:, :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna()].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', ''))
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'antitorture'
    df_year.append(df.copy())

    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='102-105', multiple_tables=False, lattice=False,
        guess=False, columns=[180.0, 340.0, 420.0]
    )[0].iloc[list(range(20, 27)) + list(range(51, 129)), :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna()].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', ''))
    df['cn_code'] = df['cn_code'].map(
        lambda x: ['81101000', '81102000', '81109000'] if x == 'Heading 8110' else
        [x]
    )
    df = df.explode('cn_code')
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'syria'
    df_year.append(df.copy())

    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='106-108', multiple_tables=False, lattice=False,
        guess=False, columns=[150.0, 350.0, 430.0]
    )[0].iloc[3:83, :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna()].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', ''))
    df['cn_code'] = df['cn_code'].map(
        lambda x: ['39071000', '39072000', '39073000', '39074000', '39075000', '39076000', '39077000',
         '39079100', '39079900'] if x == 'Heading 3907' else
        [x]
    )
    df = df.explode('cn_code')
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'iran'
    df_year.append(df.copy())

    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='109', multiple_tables=False, lattice=False,
        guess=False, columns=[160.0, 350.0, 430.0]
    )[0].iloc[1:11, :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna()].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', ''))
    df = df.explode('cn_code')
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'dprk'
    df_year.append(df.copy())

    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='110-111', multiple_tables=False, lattice=False,
        guess=False, columns=[160.0, 350.0, 430.0]
    )[0] .iloc[list(range(26, 32)) + list(range(47, 59)), :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna()].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', '').replace(
        'Subheading 310230', '31023000'))
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'explprec'
    df_year.append(df.copy())

    df = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'chemicals_2021.pdf', pages='112-114', multiple_tables=False, lattice=False,
        guess=False, columns=[180.0, 390.0, 470.0]
    )[0].iloc[list(range(30, 36)) + list(range(46, 77)), :]
    df.columns = ['dualuse_code', 'chemical_name', 'cas_number', 'cn_code']
    df = df[~df['cn_code'].isna()].copy()
    df['cn_code'] = df['cn_code'].map(lambda x: x.replace('.', ''))
    assert df['cn_code'].map(lambda x: len(x) != 8).sum() == 0
    df['year'] = 2021
    df['type'] = 'drugprec'
    df_year.append(df.copy())

    df_year = pd.concat(df_year, ignore_index=True)
    df_agg.append(df_year)

    df_agg = pd.concat(df_agg, ignore_index=True)
    df_agg['hscode'] = df_agg['cn_code'].map(lambda x: x[:6])
    df_agg.to_csv(OUTPUT_PATH / 'motivation' / 'eu_commission' / 'chemicals.csv', index=False)

    df_year = df_agg[df_agg['year'] == 2023].copy()
    df_year.loc[df_year['hscode'] == '284115', 'hscode'] = '284150'
    df_year.loc[df_year['hscode'] == '340213', 'hscode'] = '340242'
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs22_hs12.csv', dtype={'hs22': 'str', 'hs12': 'str'})
    df_year = merge_df(df_year, cw, left_on='hscode', right_on='hs22', how='left')
    df_year['chemical'] = df_year['year'].map(lambda x: str(int(x)))
    df_year = pd.get_dummies(df_year, columns=['chemical'])
    df_year['chemical'] = df_year['type'] + '_' + df_year['year'].map(lambda x: str(int(x)))
    df_year = pd.get_dummies(df_year, columns=['chemical'])
    cols = ['dualuse_code', 'cas_number'] + [col for col in df_year.columns if col.startswith('chemical_')]
    df_2023 = df_year.groupby(['hs12'])[cols].max().reset_index()

    df_year = df_agg[df_agg['year'] == 2021].copy()
    df_year.loc[df_year['hscode'] == '293998', 'hscode'] = '293971'
    df_year.loc[df_year['hscode'] == '390760', 'hscode'] = '390761'
    df_year.loc[df_year['hscode'] == '284115', 'hscode'] = '284150'
    df_year.loc[df_year['hscode'] == '290490', 'hscode'] = '290499'
    df_year.loc[df_year['hscode'] == '282470', 'hscode'] = '282490'
    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs17_hs12.csv', dtype={'hs17': 'str', 'hs12': 'str'})
    df_year = merge_df(df_year, cw, left_on='hscode', right_on='hs17', how='left')
    df_year['chemical'] = df_year['year'].map(lambda x: str(int(x)))
    df_year = pd.get_dummies(df_year, columns=['chemical'])
    df_year['chemical'] = df_year['type'] + '_' + df_year['year'].map(lambda x: str(int(x)))
    df_year = pd.get_dummies(df_year, columns=['chemical'])
    df_year['cas_number'] = df_year['cas_number'].fillna('')
    df_year['chemical_name'] = df_year['chemical_name'].fillna('')
    cols = ['dualuse_code', 'cas_number'] + [col for col in df_year.columns if col.startswith('chemical_')]
    df_2021 = df_year.groupby(['hs12'])[cols].max().reset_index()

    df = merge_df(df_2023, df_2021, on = 'hs12', suffixes = ('_2023', '_2021'), how = 'outer')
    for col in [col for col in df.columns if col.startswith('chemical_')]:
        df[col] = df[col].fillna(0)
    df.to_csv(OUTPUT_PATH / 'motivation' / 'eu_commission' / 'chemicals_wide.csv', index=False)


def extract_economic_list():
    df_agg = []
    df_list = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'critical_jun_2023.pdf', pages='all', multiple_tables=True, lattice=True
    )
    df_new_list = []
    for i, df in enumerate(df_list):
        if i > 0:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['hscode', 'ec_desc']
        df['hscode'] = df['hscode'].map(lambda x: str(x).replace('.', '').ljust(6, '0'))
        df['date'] = '2023-06-01'
        df['date_text'] = '2023jun'
        df = df[['date', 'date_text', 'hscode', 'ec_desc']]
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_new['hscode'] = df_new['hscode'].map(
        lambda x: '851713' if x == '851713\r852351' else '852351' if x == 'nan000' else x
    )
    df_agg.append(df_new.copy())

    df_list = tabula.read_pdf(
        DATA_PATH / 'motivation' / 'eu_commission' / 'critical_oct_2023.pdf', pages='all', multiple_tables=True, lattice=True
    )
    df_new_list = []
    for i, df in enumerate(df_list):
        if i > 0:
            df_extra = pd.DataFrame([list(df.columns)], columns=df.columns)
            df = pd.concat([df_extra, df], ignore_index=True)
        df.columns = ['hscode', 'ec_desc']
        df['hscode'] = df['hscode'].map(lambda x: str(x).replace('.', '').ljust(6, '0'))
        df['date'] = '2023-10-01'
        df['date_text'] = '2023oct'
        df = df[['date', 'date_text', 'hscode', 'ec_desc']]
        df_new_list.append(df.copy())
    df_new = pd.concat(df_new_list, ignore_index=True)
    df_agg.append(df_new.copy())
    df_agg = pd.concat(df_agg, ignore_index=True)
    df_agg.to_csv(OUTPUT_PATH / 'motivation' / 'eu_commission' / 'economic_list.csv', index=False)

    cw = pd.read_csv(OUTPUT_PATH / 'crosswalks' / 'hs22_hs12.csv', dtype={'hs22': str, 'hs12': str})
    df_agg = merge_df(df_agg, cw, how='left', left_on='hscode', right_on='hs22')
    df_agg = pd.get_dummies(df_agg, columns=['date_text'])
    select_col = ['ec_desc'] + [col for col in df_agg.columns if col.startswith('date_text')]
    df_agg = df_agg.groupby(['hs12'])[select_col].max().reset_index()

    def process_col(col):
        if col.startswith('date_text_tier'):
            return col.replace('date_text_tier', 'economic')
        if col.startswith('date_text'):
            return col.replace('date_text', 'economic')
        return col

    df_agg.columns = [process_col(col) for col in df_agg.columns]
    df_agg.to_csv(OUTPUT_PATH / 'motivation' / 'eu_commission' / 'economic_list_wide.csv', index=False)


def extract_lists():
    extract_critical_list()
    extract_chemicals()
    extract_economic_list()


if __name__ == '__main__':
    extract_lists()

