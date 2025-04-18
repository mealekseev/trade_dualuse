""" Build a crosswalk between PSC and NAICS """

import ibis
import numpy as np
import pandas as pd
import re

from settings import DATA_PATH, OUTPUT_PATH, output
from utilities import merge_df


def save_raw_files():
    # saving raw subawards
    conn_rpt = ibis.postgres.connect(
        database='data_store_api',
        host='127.0.0.1',
        user='postgres',
        password='postgres_max',
        schema='rpt'
    )
    subaward_search = conn_rpt.tables.subaward_search
    subaward_search = subaward_search.filter(subaward_search.awarding_agency_code == '097')
    subaward_search = subaward_search.filter(
        subaward_search.subaward_amount.cast('int') != subaward_search.sub_awardee_or_recipient_uniqu.cast('int'))
    subaward_search = subaward_search.filter(subaward_search.subaward_amount.cast('int')
                                             != subaward_search.sub_legal_entity_zip.replace('-', '').cast('int'))
    for col in ['broker_created_at', 'broker_updated_at', 'action_date', 'sub_action_date', 'date_submitted',
                'last_modified_date']:
        subaward_search = subaward_search.mutate(**{col: subaward_search[col].cast('date')})
    cols = [
        'broker_created_at', 'broker_updated_at', 'broker_subaward_id', 'unique_award_key', 'award_piid_fain',
        'parent_award_id', 'award_amount', 'action_date', 'fy', 'awarding_agency_code', 'awarding_agency_name',
        'awarding_sub_tier_agency_c', 'awarding_sub_tier_agency_n', 'awarding_office_code', 'awarding_office_name',
        'funding_agency_code', 'funding_agency_name', 'funding_sub_tier_agency_co', 'funding_sub_tier_agency_na',
        'funding_office_code', 'funding_office_name', 'awardee_or_recipient_uniqu', 'awardee_or_recipient_uei',
        'awardee_or_recipient_legal', 'dba_name', 'ultimate_parent_unique_ide', 'ultimate_parent_uei',
        'ultimate_parent_legal_enti', 'business_types', 'award_description', 'naics', 'naics_description',
        'cfda_numbers', 'cfda_titles', 'subaward_type', 'subaward_report_year', 'subaward_report_month',
        'subaward_number', 'subaward_amount', 'sub_action_date', 'sub_awardee_or_recipient_uniqu',
        'sub_awardee_or_recipient_uei', 'sub_awardee_or_recipient_legal_raw', 'sub_dba_name',
        'sub_ultimate_parent_unique_ide', 'sub_ultimate_parent_uei', 'sub_ultimate_parent_legal_enti_raw',
        'sub_business_types', 'subaward_description', 'prime_id', 'internal_id', 'date_submitted',
        'report_type', 'transaction_type', 'program_title', 'contract_agency_code', 'contract_idv_agency_code',
        'grant_funding_agency_id', 'grant_funding_agency_name', 'federal_agency_name', 'treasury_symbol', 'dunsplus4',
        'recovery_model_q1', 'recovery_model_q2', 'compensation_q1', 'compensation_q2',
        'sub_id', 'sub_parent_id', 'sub_federal_agency_id', 'sub_federal_agency_name', 'sub_funding_agency_id',
        'sub_funding_agency_name', 'sub_funding_office_id', 'sub_funding_office_name', 'sub_naics', 'sub_cfda_numbers',
        'sub_dunsplus4', 'sub_recovery_subcontract_amt', 'sub_recovery_model_q1', 'sub_recovery_model_q2',
        'sub_compensation_q1', 'sub_compensation_q2', 'prime_award_group', 'prime_award_type', 'piid', 'fain',
        'latest_transaction_id', 'last_modified_date', 'awarding_toptier_agency_name',
        'awarding_toptier_agency_abbreviation', 'awarding_subtier_agency_name', 'awarding_subtier_agency_abbreviation',
        'funding_toptier_agency_name', 'funding_toptier_agency_abbreviation', 'funding_subtier_agency_name',
        'funding_subtier_agency_abbreviation', 'cfda_number', 'cfda_title', 'sub_fiscal_year', 'sub_total_obl_bin',
        'sub_awardee_or_recipient_legal', 'sub_ultimate_parent_legal_enti', 'business_type_code', 'business_categories',
        'treasury_account_identifiers', 'pulled_from', 'type_of_contract_pricing', 'type_set_aside', 'extent_competed',
        'product_or_service_code', 'product_or_service_description', 'sub_legal_entity_country_code',
        'sub_legal_entity_country_name', 'sub_legal_entity_county_code', 'sub_legal_entity_county_name',
        'sub_legal_entity_zip5', 'sub_legal_entity_city_code', 'sub_legal_entity_congressional',
        'place_of_perform_scope', 'sub_place_of_perform_country_co', 'sub_place_of_perform_country_name',
        'sub_place_of_perform_county_code', 'sub_place_of_perform_county_name', 'sub_place_of_perform_zip5',
        'sub_place_of_perform_city_code', 'sub_place_of_perform_congressio', 'keyword_ts_vector', 'award_ts_vector',
        'recipient_name_ts_vector', 'award_id', 'awarding_agency_id', 'cfda_id', 'funding_agency_id'
    ]
    subaward_search = subaward_search[cols]
    output("Processing subawards...")
    df = subaward_search.execute(limit=None)
    output("Saving subawards...")
    df['fy'] = df['fy'].map(lambda x: int(x[2:6]) if x.startwith('FY') else None)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'raw_usaspending_subawards.csv', index=False)

    # saving raw awards
    award_search = conn_rpt.tables.award_search
    award_search = award_search.filter(award_search.is_fpds == True)
    award_search = award_search.filter(award_search.awarding_toptier_agency_code == '097')
    cols = [
        'treasury_account_identifiers', 'award_id', 'category', 'type', 'type_description', 'generated_unique_award_id',
        'display_award_id', 'update_date', 'piid', 'fain', 'uri', 'award_amount', 'total_obligation', 'description',
        'total_subsidy_cost', 'total_loan_value', 'total_obl_bin', 'recipient_hash', 'recipient_levels',
        'recipient_name', 'recipient_unique_id', 'parent_recipient_unique_id', 'recipient_uei', 'parent_uei',
        'business_categories', 'action_date', 'fiscal_year', 'last_modified_date', 'period_of_performance_start_date',
        'period_of_performance_current_end_date', 'date_signed', 'ordering_period_end_date',
        'original_loan_subsidy_cost', 'face_value_loan_guarantee', 'awarding_agency_id', 'funding_agency_id',
        'funding_toptier_agency_id', 'funding_subtier_agency_id', 'awarding_toptier_agency_name',
        'funding_toptier_agency_name', 'awarding_subtier_agency_name', 'funding_subtier_agency_name',
        'awarding_toptier_agency_code', 'funding_toptier_agency_code', 'awarding_subtier_agency_code',
        'funding_subtier_agency_code', 'cfda_program_title', 'cfda_number', 'cfdas', 'sai_number',
        'type_of_contract_pricing', 'extent_competed', 'type_set_aside', 'product_or_service_code',
        'product_or_service_description', 'naics_code', 'naics_description', 'tas_paths', 'tas_components',
        'base_and_all_options_value', 'base_exercised_options_val', 'certified_date', 'create_date',
        'earliest_transaction_id', 'fpds_agency_id', 'fpds_parent_agency_id', 'is_fpds', 'latest_transaction_id',
        'non_federal_funding_amount', 'parent_award_piid', 'raw_recipient_name', 'subaward_count',
        'total_funding_amount', 'total_indirect_federal_sharing', 'total_subaward_amount', 'transaction_unique_id',
        'awarding_subtier_agency_code_raw', 'awarding_subtier_agency_name_raw', 'awarding_toptier_agency_code_raw',
        'awarding_toptier_agency_name_raw', 'funding_subtier_agency_code_raw', 'funding_subtier_agency_name_raw',
        'funding_toptier_agency_code_raw', 'funding_toptier_agency_name_raw'
    ]
    for col in [
        'action_date', 'last_modified_date', 'period_of_performance_start_date',
        'period_of_performance_current_end_date',
        'date_signed', 'certified_date', 'create_date'
    ]:
        award_search = award_search.mutate(**{col: award_search[col].cast('date')})
    for year in range(2001, 2024):
        output(f"Processing year {year}...")
        award_subset = award_search.filter(award_search.fiscal_year == year)
        award_subset = award_subset[cols]
        df = award_subset.execute(limit=None)
        df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / f'raw_usaspending_awards_{year}.csv', index=False)


def save_usaspending_codes():
    conn_rpt = ibis.postgres.connect(
        database='data_store_api',
        host='127.0.0.1',
        user='postgres',
        password='postgres_max',
        schema='rpt'
    )
    award_search = conn_rpt.tables.award_search
    award_search = award_search.filter(award_search.is_fpds == True)
    award_search = award_search.filter(award_search.awarding_toptier_agency_code == '097')
    award_naics = award_search[[
        'award_id', 'fiscal_year', 'naics_code', 'naics_description', 'product_or_service_code'
    ]]
    df = award_naics.execute(limit=None)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_naics.csv', index=False)


def save_usaspending_awards():
    conn_rpt = ibis.postgres.connect(
        database='data_store_api',
        host='127.0.0.1',
        user='postgres',
        password='postgres_max',
        schema='rpt'
    )
    award_search = conn_rpt.tables.award_search
    award_search = award_search.filter(award_search.is_fpds == True)
    award_search = award_search.filter(award_search.awarding_toptier_agency_code == '097')
    award_amount = award_search[[
        'award_id', 'fiscal_year', 'award_amount', 'total_subaward_amount', 'awarding_subtier_agency_code'
    ]]
    df = award_amount.execute(limit=None)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_amount.csv', index=False)


def save_usaspending_codebook():
    conn_public = ibis.postgres.connect(
        database='data_store_api',
        host='127.0.0.1',
        user='postgres',
        password='postgres_max',
        schema='public'
    )
    subtier_agency = conn_public.tables.subtier_agency
    for col in ['create_date', 'update_date']:
        subtier_agency = subtier_agency.mutate(**{col: subtier_agency[col].cast('date')})
    subtier_agency = subtier_agency[['subtier_code', 'abbreviation', 'name']]
    df = subtier_agency.execute(limit=None)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'codebook_subtier_agency.csv', index=False)

    toptier_agency = conn_public.tables.toptier_agency
    for col in ['create_date', 'update_date']:
        toptier_agency = toptier_agency.mutate(**{col: toptier_agency[col].cast('date')})
    toptier_agency = toptier_agency[[
        'toptier_code', 'abbreviation', 'name', 'mission', 'website', 'icon_filename', 'justification',
        'about_agency_data'
    ]]
    df = toptier_agency.execute(limit=None)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'codebook_toptier_agency.csv', index=False)


def save_usaspending_subawards():
    conn_rpt = ibis.postgres.connect(
        database='data_store_api',
        host='127.0.0.1',
        user='postgres',
        password='postgres_max',
        schema='rpt'
    )
    subaward_search = conn_rpt.tables.subaward_search
    subaward_search = subaward_search.filter(subaward_search.awarding_agency_code == '097')
    subaward_search = subaward_search.filter(
        subaward_search.subaward_amount.cast('int') != subaward_search.sub_awardee_or_recipient_uniqu.cast('int'))
    subaward_search = subaward_search.filter(subaward_search.subaward_amount.cast('int')
                                             != subaward_search.sub_legal_entity_zip.replace('-', '').cast('int'))
    subaward_search = subaward_search[[
        'award_id', 'fy', 'broker_subaward_id', 'award_amount', 'subaward_amount', 'sub_naics'
    ]]
    df = subaward_search.execute(limit=None)

    df = subaward_search.execute(limit=1000)
    df['fy'] = df['fy'].map(lambda x: int(x[2:6]))
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_subaward_amount.csv', index=False)


def build_awards_naics():
    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_naics.csv', dtype={'naics_code': 'str'})
    df['naics_code'] = df['naics_code'].map(
        lambda x: str(int(x)).zfill(6) if bool(re.search('^[0-9.]+$', str(x))) else None)

    naics_cw = pd.read_pickle(OUTPUT_PATH / 'crosswalks' / 'naics_combined.pkl')
    df = merge_df(df, naics_cw, left_on='naics_code', right_on='naics_from', how='left')
    df = df[['award_id', 'naics']]
    df.to_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_naics.pkl')


def build_awards_pcs():
    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_naics.csv', nrows=10000)
    psc_cw = pd.read_pickle(OUTPUT_PATH / 'crosswalks' / 'psc_naics12.pkl')
    df = merge_df(df, psc_cw, left_on='product_or_service_code', right_on='psc', how='left')
    df = df[['award_id', 'naics']]
    df.to_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_psc.pkl')


def save_usaspending_companies():
    conn_rpt = ibis.postgres.connect(
        database='data_store_api',
        host='127.0.0.1',
        user='postgres',
        password='postgres_max',
        schema='rpt'
    )
    award_search = conn_rpt.tables.award_search
    award_search = award_search.filter(award_search.is_fpds == True)
    award_search = award_search.filter(award_search.awarding_toptier_agency_code == '097')
    award_recipient = award_search[[
        'award_id', 'fiscal_year', 'recipient_hash', 'recipient_name', 'recipient_unique_id',
        'parent_recipient_unique_id', 'recipient_uei'
    ]]
    df = award_recipient.execute(limit=None)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_recipient.csv', index=False)


def save_usaspending_subaward_companies():
    conn_rpt = ibis.postgres.connect(
        database='data_store_api',
        host='127.0.0.1',
        user='postgres',
        password='postgres_max',
        schema='rpt'
    )
    subaward_search = conn_rpt.tables.subaward_search
    subaward_search = subaward_search.filter(subaward_search.awarding_agency_code == '097')
    subaward_search = subaward_search.filter(
        subaward_search.subaward_amount.cast('int') != subaward_search.sub_awardee_or_recipient_uniqu.cast('int'))
    subaward_search = subaward_search.filter(subaward_search.subaward_amount.cast('int')
                                             != subaward_search.sub_legal_entity_zip.replace('-', '').cast('int'))
    subaward_search = subaward_search[[
        'award_id', 'fy', 'broker_subaward_id', 'sub_awardee_or_recipient_uniqu',
        'sub_awardee_or_recipient_uei', 'sub_ultimate_parent_unique_ide', 'sub_ultimate_parent_uei',
        'sub_awardee_or_recipient_legal_raw'
    ]]
    df = subaward_search.execute(limit=None)
    df['fy'] = df['fy'].map(lambda x: int(x[2:6]))
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_sub_recipient.csv', index=False)


def save_usaspending_duns():
    conn_rpt = ibis.postgres.connect(
        database='data_store_api',
        host='127.0.0.1',
        user='postgres',
        password='postgres_max',
        schema='rpt'
    )
    duns = conn_rpt.tables.recipient_lookup
    duns = duns[['recipient_hash', 'legal_business_name', 'duns', 'parent_duns', 'uei', 'parent_uei']]
    df = duns.execute(limit=None)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_company.csv', index=False)


def build_company_duns():
    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_recipient.csv')
    df = df.sort_values(['fiscal_year', 'recipient_hash', 'award_id'])
    for col in ['recipient_unique_id', 'parent_recipient_unique_id']:
        df[col] = df[col].map(lambda x: str(int(x)).zfill(9) if bool(re.search('^[0-9.]+$', str(x))) else None)
        output(df[col].map(lambda x: len(str(x))).value_counts())
        df[col] = df.groupby('recipient_hash')[col].ffill()
        df[col] = df.groupby('recipient_hash')[col].bfill()
        output(df[col].map(lambda x: len(str(x))).value_counts())
        df[col] = df[col].fillna('')

    firms = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_company.csv')
    firms = firms[['recipient_hash', 'duns', 'parent_duns']]
    for col in ['duns', 'parent_duns']:
        output(firms[col].isna().value_counts())
        firms[col] = firms[col].map(lambda x: str(int(x)).zfill(9) if bool(re.search('^[0-9.]+$', str(x))) else None)
        output(firms[col].map(lambda x: len(str(x))).value_counts())
        firms[col] = firms[col].fillna('')

    # merge the rest with firm identifiers
    df = merge_df(df, firms, how='left', on='recipient_hash', validate='m:1')
    df.loc[df['recipient_unique_id'] == '', 'recipient_unique_id'] = df.loc[
        df['recipient_unique_id'] == '', 'duns']
    df.loc[df['recipient_unique_id'] == '', 'parent_recipient_unique_id'] = df.loc[
        df['recipient_unique_id'] == '', 'parent_duns']
    df = df[['award_id', 'fiscal_year', 'recipient_hash', 'recipient_unique_id', 'parent_recipient_unique_id']]
    df.columns = ['award_id', 'fiscal_year', 'recipient_hash', 'duns', 'parent_duns']
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_company_duns.csv', index=False)


def build_sub_company_duns():
    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_sub_recipient.csv')
    for col in ['sub_awardee_or_recipient_uniqu', 'sub_ultimate_parent_unique_ide']:
        df[col] = df[col].map(lambda x: str(int(x)).zfill(9) if bool(re.search('^[0-9.]+$', str(x))) else None)
        output(df[col].map(lambda x: len(str(x))).value_counts())
    df.rename({
        'sub_awardee_or_recipient_uniqu': 'subawardee_duns',
        'sub_awardee_or_recipient_uei': 'subawardee_uei',
        'sub_ultimate_parent_unique_ide': 'subparent_duns',
        'sub_ultimate_parent_uei': 'subparent_uei'
    }, axis=1, inplace=True)
    output(df['subawardee_duns'].isna().value_counts())
    df = df.sort_values(['subawardee_uei', 'subawardee_duns'])
    df['subawardee_duns_copy'] = df['subawardee_duns']
    df['subawardee_duns_copy'] = df.groupby('subawardee_uei')['subawardee_duns_copy'].ffill()
    df['subawardee_duns_copy'] = df.groupby('subawardee_uei')['subawardee_duns_copy'].bfill()
    df.loc[df['subawardee_duns'].isna(), 'subawardee_duns'] = df.loc[
        df['subawardee_duns'].isna(), 'subawardee_duns_copy']
    df.drop('subawardee_duns_copy', axis=1, inplace=True)
    output(df['subawardee_duns'].isna().value_counts())

    firms = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_company.csv')
    firms = firms[['duns', 'parent_duns', 'uei', 'parent_uei']]
    for col in ['duns', 'parent_duns']:
        output(firms[col].isna().value_counts())
        firms[col] = firms[col].map(lambda x: str(int(x)).zfill(9) if bool(re.search('^[0-9.]+$', str(x))) else None)
        output(firms[col].map(lambda x: len(str(x))).value_counts())
        firms[col] = firms[col].fillna('')
    firms = firms[firms['duns'] != '']

    firm_uei = firms[~firms['uei'].isna()]
    df = merge_df(df, firm_uei, how='left', left_on='subawardee_uei', right_on='uei', validate='m:1')
    output(df['subawardee_duns'].isna().value_counts())
    df.loc[df['subawardee_duns'].isna(), 'subawardee_duns'] = df.loc[df['subawardee_duns'].isna(), 'duns']
    output(df['subawardee_duns'].isna().value_counts())
    df.drop(['duns', 'uei', 'parent_uei', 'parent_duns', '_merge'], axis=1, inplace=True)

    firm_parent_uei = firms[~firms['parent_uei'].isna()]
    firm_parent_uei = firm_parent_uei.groupby('parent_uei')['parent_duns'].first().reset_index()
    df = merge_df(df, firm_parent_uei, how='left', left_on='subawardee_uei', right_on='parent_uei', validate='m:1')
    output(df['subawardee_duns'].isna().value_counts())
    df.loc[df['subawardee_duns'].isna(), 'subawardee_duns'] = df.loc[df['subawardee_duns'].isna(), 'parent_duns']
    output(df['subawardee_duns'].isna().value_counts())
    df.drop(['parent_duns', 'parent_uei', '_merge'], axis=1, inplace=True)

    df = merge_df(df, firm_uei, how='left', left_on='subparent_uei', right_on='uei', validate='m:1')
    output(df['subawardee_duns'].isna().value_counts())
    df.loc[df['subawardee_duns'].isna(), 'subawardee_duns'] = df.loc[df['subawardee_duns'].isna(), 'duns']
    output(df['subawardee_duns'].isna().value_counts())
    df.drop(['duns', 'uei', 'parent_uei', 'parent_duns', '_merge'], axis=1, inplace=True)

    df = merge_df(df, firm_parent_uei, how='left', left_on='subparent_uei', right_on='parent_uei', validate='m:1')
    output(df['subawardee_duns'].isna().value_counts())
    df.loc[df['subawardee_duns'].isna(), 'subawardee_duns'] = df.loc[df['subawardee_duns'].isna(), 'parent_duns']
    output(df['subawardee_duns'].isna().value_counts())
    df.drop(['parent_uei', 'parent_duns', '_merge'], axis=1, inplace=True)

    df = df[['award_id', 'fy', 'broker_subaward_id', 'subawardee_duns', 'subparent_duns']]
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_sub_company_duns.csv', index=False)


def save_duns_rows():
    duns = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_company_duns.csv', dtype=str)
    sub_duns = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_sub_company_duns.csv', dtype=str)
    duns_set = set(duns.loc[~duns['duns'].isna(), 'duns'].to_list()
                   + duns.loc[~duns['parent_duns'].isna(), 'parent_duns'].to_list()
                   + sub_duns.loc[~sub_duns['subawardee_duns'].isna(), 'subawardee_duns'].to_list()
                   + sub_duns.loc[~sub_duns['subparent_duns'].isna(), 'subparent_duns'].to_list())
    conn = ibis.postgres.connect(
        database='dun_bradstreet',
        host='127.0.0.1',
        user='postgres',
        password='postgres_max'
    )
    listings = conn.tables.listings
    listings = listings[listings['dunsno'].isin(duns_set)]
    listings = listings[['year', 'dunsno', 'companyname', 'sic1', 'sic2', 'sic3', 'sic4', 'sic5', 'sic6', 'sls']]
    df = listings.execute(limit=None)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_duns_rows.csv', index=False)


def crosswalk_duns_sic_naics():
    duns = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_duns_rows.csv', dtype=str)

    sic_combined_naics12 = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'sic_combined_naics12.csv', dtype=str)
    sic_combined_naics12['wt_mappings'] = sic_combined_naics12['wt_mappings'].map(float)
    sic_combined_naics12 = sic_combined_naics12.groupby('sic')[['naics12', 'wt_mappings']].agg(list).reset_index()
    sic_combined_naics12['naics12_list'] = sic_combined_naics12[['naics12', 'wt_mappings']].apply(
        lambda row: list([(i, j) for i, j in zip(row[0], row[1])]), axis=1)
    sic_combined_naics12 = sic_combined_naics12[['sic', 'naics12_list']]

    df_agg = pd.DataFrame(columns=['dunsno', 'year'])
    for i in range(1, 7):
        df = duns[['dunsno', 'year', f'sic{i}']]
        df = merge_df(df, sic_combined_naics12, left_on=f'sic{i}', right_on='sic', how='left')
        df = df[df['_merge'] == 'both']
        df = df[['dunsno', 'year', f'sic{i}', 'naics12_list']]
        df.columns = ['dunsno', 'year', f'sic{i}', f'naics12_{i}']
        df[f'sic{i}'] = df[f'sic{i}'].map(lambda x: [(x, 1)])
        df_agg = merge_df(df_agg, df, on=['dunsno', 'year'], how='outer', keep_merge=False)
    df_agg['num'] = 6 - df_agg['sic1'].isna() - df_agg['sic2'].isna() - df_agg['sic3'].isna() \
                    - df_agg['sic4'].isna() - df_agg['sic5'].isna() - df_agg['sic6'].isna()
    df_agg['sic_list'] = [[] for i in range(len(df_agg))]
    df_agg['naics12_list'] = [[] for i in range(len(df_agg))]
    for i in range(1, 7):
        df_agg['sic_list'] = df_agg['sic_list'] + df_agg[f'sic{i}'].map(
            lambda x: [] if str(x) == 'nan' else x)
        df_agg['naics12_list'] = df_agg['naics12_list'] + df_agg[f'naics12_{i}'].map(
            lambda x: [] if str(x) == 'nan' else x)

    def adjust_weights(row):
        return [(e[0], e[1] / row[0]) for e in row[1]]

    df_agg['sic_list'] = df_agg[['num', 'sic_list']].apply(adjust_weights, axis=1)
    df_agg['naics12_list'] = df_agg[['num', 'naics12_list']].apply(adjust_weights, axis=1)
    df_agg = df_agg[['dunsno', 'year', 'sic_list', 'naics12_list']].sort_values(['dunsno', 'year'])
    df_agg.to_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_awards_sic_naics.pkl')


def build_awards_duns_naics():
    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_company_duns.csv',
                     usecols=['award_id', 'fiscal_year', 'duns', 'parent_duns'],
                     dtype={'duns': str, 'year': int, 'parent_duns': str})
    duns_naics = pd.read_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_awards_sic_naics.pkl')
    duns_naics['year'] = duns_naics['year'].astype(int)
    duns_naics = duns_naics.sort_values(['dunsno', 'year'])
    dn_unique = duns_naics.groupby('dunsno').last().reset_index()

    df = merge_df(df, duns_naics, left_on=['duns', 'fiscal_year'], right_on=['dunsno', 'year'], how='left')
    df_matched = df[df['_merge'] == 'both'][['award_id', 'fiscal_year', 'naics12_list']].copy()
    df_matched['wave'] = 0
    df_list = [df_matched.copy()]

    df = df[df['_merge'] == 'left_only'][['award_id', 'fiscal_year', 'duns', 'parent_duns']].copy()
    df = merge_df(df, dn_unique, left_on='duns', right_on='dunsno', how='left')
    df_matched = df[df['_merge'] == 'both'][['award_id', 'fiscal_year', 'naics12_list']].copy()
    df_matched['wave'] = 1
    df_list.append(df_matched.copy())

    df = df[df['_merge'] == 'left_only'][['award_id', 'fiscal_year', 'duns', 'parent_duns']].copy()
    df = merge_df(df, duns_naics, left_on=['parent_duns', 'fiscal_year'], right_on=['dunsno', 'year'], how='left')
    df_matched = df[df['_merge'] == 'both'][['award_id', 'fiscal_year', 'naics12_list']].copy()
    df_matched['wave'] = 2
    df_list.append(df_matched.copy())

    df = df[df['_merge'] == 'left_only'][['award_id', 'fiscal_year', 'duns', 'parent_duns']].copy()
    df = merge_df(df, dn_unique, left_on='parent_duns', right_on='dunsno', how='left')
    df_matched = df[df['_merge'] == 'both'][['award_id', 'fiscal_year', 'naics12_list']].copy()
    df_matched['wave'] = 3
    df_list.append(df_matched.copy())

    df = df[df['_merge'] == 'left_only'][['award_id', 'fiscal_year']].copy()
    df['wave'] = 4
    df_list.append(df.copy())

    df = pd.concat(df_list)
    output(df['wave'].value_counts())
    df.to_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_dunsnaics.pkl')


def build_subawards_duns_naics():
    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_sub_company_duns.csv',
                     dtype={'fy': int, 'subawardee_duns': str, 'subparent_duns': str})
    duns_naics = pd.read_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_awards_sic_naics.pkl')
    duns_naics['year'] = duns_naics['year'].astype(int)
    duns_naics = duns_naics.sort_values(['dunsno', 'year'])
    dn_unique = duns_naics.groupby('dunsno').last().reset_index()

    df = merge_df(df, duns_naics, left_on=['subawardee_duns', 'fy'], right_on=['dunsno', 'year'], how='left')
    df_matched = df[df['_merge'] == 'both'][['award_id', 'fy', 'broker_subaward_id', 'naics12_list']].copy()
    df_matched['wave'] = 0
    df_list = [df_matched.copy()]

    df = df[df['_merge'] == 'left_only'][[
        'award_id', 'fy', 'broker_subaward_id', 'subawardee_duns', 'subparent_duns']].copy()
    df = merge_df(df, dn_unique, left_on='subawardee_duns', right_on='dunsno', how='left')
    df_matched = df[df['_merge'] == 'both'][['award_id', 'fy', 'broker_subaward_id', 'naics12_list']].copy()
    df_matched['wave'] = 1
    df_list.append(df_matched.copy())

    df = df[df['_merge'] == 'left_only'][[
        'award_id', 'fy', 'broker_subaward_id', 'subawardee_duns', 'subparent_duns']].copy()
    df = merge_df(df, duns_naics, left_on=['subparent_duns', 'fy'], right_on=['dunsno', 'year'], how='left')
    df_matched = df[df['_merge'] == 'both'][['award_id', 'fy', 'broker_subaward_id', 'naics12_list']].copy()
    df_matched['wave'] = 2
    df_list.append(df_matched.copy())

    df = df[df['_merge'] == 'left_only'][[
        'award_id', 'fy', 'broker_subaward_id', 'subawardee_duns', 'subparent_duns']].copy()
    df = merge_df(df, dn_unique, left_on='subparent_duns', right_on='dunsno', how='left')
    df_matched = df[df['_merge'] == 'both'][['award_id', 'fy', 'broker_subaward_id', 'naics12_list']].copy()
    df_matched['wave'] = 3
    df_list.append(df_matched.copy())

    df = df[df['_merge'] == 'left_only'][['award_id', 'fy', 'broker_subaward_id']].copy()
    df['wave'] = 4
    df_list.append(df.copy())

    df = pd.concat(df_list)
    output(df['wave'].value_counts())
    df.to_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_subaward_dunsnaics.pkl')


def convert_cw_to_list():
    # naics collapse
    output("Collapsing naics...")
    naics = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_naics.csv', dtype={'naics': str})
    naics['naics'] = naics.apply(lambda x: (x['naics12'], x['wt_mappings']), axis=1)
    naics = naics.groupby('award_id')['naics'].apply(list).reset_index()
    naics['naics'] = naics['naics'].map(lambda x: None if len(x) == 1 and np.isnan(x[0][0]) else x)
    naics.to_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_naics.pkl')

    # psc collapse
    output("Collapsing psc...")
    psc = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_psc.csv')
    psc['psc_naics'] = psc.apply(lambda x: (x['naics12'], x['wt_mappings']), axis=1)
    psc = psc.groupby('award_id')['psc_naics'].apply(list).reset_index()
    psc['psc_naics'] = psc['psc_naics'].map(lambda x: None if len(x) == 1 and np.isnan(x[0][0]) else x)
    psc.to_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_psc.pkl')


def collect_awards():
    # read awards
    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_amount.csv', nrows=10000)
    cols = df.columns.tolist()
    naics = pd.read_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_naics.pkl')
    psc = pd.read_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_psc.pkl')
    psc.columns = ['award_id', 'psc_naics']
    dunsnaics = pd.read_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_dunsnaics.pkl')
    dunsnaics = dunsnaics[['award_id', 'naics12_list']]
    dunsnaics.columns = ['award_id', 'duns_naics']

    # merge frames
    df = merge_df(df, naics, on='award_id', how='left', keep_merge=False, validate='1:1')
    df = merge_df(df, psc, on='award_id', how='left', keep_merge=False, validate='1:1')
    df = merge_df(df, dunsnaics, on='award_id', how='left', keep_merge=False, validate='1:1')
    df['naics_combined'] = df.apply(
        lambda row: row['naics'] if type(row['naics']) == list
        else row['duns_naics'],
        axis=1
    )

    for col in ['naics', 'psc_naics', 'duns_naics', 'naics_combined']:
        df_sub = df[cols + [col]]
        df_sub = df_sub.explode(col)
        df_sub[f'{col}_wgt'] = df_sub[col].map(lambda x: x[1] if str(x) != 'nan' else None)
        df_sub[f'{col}'] = df_sub[col].map(lambda x: x[0] if str(x) != 'nan' else None)
        df_sub.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / f'usaspending_awards_naics_{col}.csv', index=False)

    for col in ['naics', 'psc_naics', 'duns_naics', 'naics_combined']:
        df[col] = df[col].map(lambda x: str(x))
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'usaspending_awards_naics.csv', index=False)


def collect_subawards():
    # read subawards
    df = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_subaward_amount.csv')
    dunsnaics = pd.read_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_subaward_dunsnaics.pkl')
    dunsnaics = dunsnaics[['award_id', 'broker_subaward_id', 'naics12_list']]
    dunsnaics.columns = ['award_id', 'broker_subaward_id', 'duns_naics']

    # merge frames
    df = merge_df(df, dunsnaics, on=['award_id', 'broker_subaward_id'], how='left', keep_merge=False, validate='1:1')
    df.to_pickle(OUTPUT_PATH / 'measurement' / 'contracts' / 'usaspending_subawards_naics.pkl')

    df = df.explode('duns_naics')
    df['duns_naics_wgt'] = df['duns_naics'].map(lambda x: x[1] if str(x) != 'nan' else None)
    df['duns_naics'] = df['duns_naics'].map(lambda x: x[0] if str(x) != 'nan' else None)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'usaspending_subawards_naics.csv', index=False)


def build_sales():
    awards = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_award_amount.csv', usecols=['award_id', 'award_amount'])
    duns_awards = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_company_duns.csv',
                              usecols=['award_id', 'fiscal_year', 'duns', 'parent_duns'],
                              dtype={'duns': str, 'year': int, 'parent_duns': str})
    df = merge_df(awards, duns_awards, on='award_id', how='left', keep_merge=False, validate='1:1')
    df = df.groupby(['duns', 'fiscal_year'])['award_amount'].sum().reset_index()

    subawards = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_subaward_amount.csv',
                            usecols=['award_id', 'fy', 'broker_subaward_id', 'subaward_amount'])
    duns_subawards = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_usaspending_sub_company_duns.csv',
                                 dtype={'subawardee_duns': str, 'fy': int, 'parent_duns': str})
    df_new = merge_df(subawards, duns_subawards, on=['award_id', 'fy', 'broker_subaward_id'],
                      how='left', keep_merge=False, validate='1:1')
    df_new = df_new.groupby(['subawardee_duns', 'fy'])['subaward_amount'].sum().reset_index()

    df = merge_df(
        df, df_new, left_on=['duns', 'fiscal_year'], right_on=['subawardee_duns', 'fy'], how='outer', keep_merge=False,
        validate='1:1'
    )
    df = df[['fiscal_year', 'duns', 'award_amount', 'subaward_amount']]
    df['award_amount'] = df['award_amount'].fillna(0)
    df['subaward_amount'] = df['subaward_amount'].fillna(0)
    df['total_amount'] = df['award_amount'] + df['subaward_amount']

    duns = pd.read_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'temp_duns_rows.csv', dtype=str)
    duns = duns[['year', 'dunsno', 'sls']]
    duns['year'] = duns['year'].map(float)
    df = merge_df(df, duns, left_on=['fiscal_year', 'duns'], right_on=['year', 'dunsno'], how='left')
    df.drop(['year', 'dunsno'], inplace=True, axis=1)
    df.to_csv(OUTPUT_PATH / 'measurement' / 'contracts' / 'usaspending_sales_db.csv', index=False)


def main():
    save_raw_files()
    save_usaspending_codes()
    save_usaspending_awards()
    save_usaspending_subawards()
    save_usaspending_codebook()
    save_usaspending_companies()
    save_usaspending_subaward_companies()
    save_usaspending_duns()
    build_awards_naics()
    build_awards_pcs()
    build_company_duns()
    build_sub_company_duns()
    save_duns_rows()
    crosswalk_duns_sic_naics()
    build_awards_duns_naics()
    build_subawards_duns_naics()
    collect_awards()
    collect_subawards()
    build_sales()


if __name__ == '__main__':
    main()

