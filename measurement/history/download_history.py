""" Downloads csvs from the national archive webiste """


import sys
import pandas as pd
from pathlib import Path

from settings import DATA_PATH, EXTERNAL_PATH


def download_csv():
    link_dict = [
        {'year': 1966, 'dt': 243},
        {'year': 1967, 'dt': 244},
        {'year': 1968, 'dt': 213},
        {'year': 1969, 'dt': 214},
        {'year': 1970, 'dt': 246},
        {'year': 1971, 'dt': 215},
        {'year': 1972, 'dt': 212},
        {'year': 1973, 'dt': 216},
        {'year': 1974, 'dt': 217},
        {'year': 1975, 'dt': 200},
        {'year': 1976, 'dt': 242},
        {'year': 19762, 'dt': 357},
        {'year': 1977, 'dt': 355},
        {'year': 1978, 'dt': 356},
        {'year': 1979, 'dt': 340},
        {'year': 1980, 'dt': 341},
        {'year': 1981, 'dt': 342},
        {'year': 1982, 'dt': 343},
        {'year': 1983, 'dt': 257},
        {'year': 1984, 'dt': 258},
        {'year': 1985, 'dt': 344},
        {'year': 1986, 'dt': 339},
        {'year': 1987, 'dt': 335},
        {'year': 1988, 'dt': 336},
        {'year': 1989, 'dt': 260},
        {'year': 1990, 'dt': 337},
        {'year': 1991, 'dt': 338},
        {'year': 1992, 'dt': 818},
        {'year': 1993, 'dt': 819},
        {'year': 1994, 'dt': 820},
        {'year': 1995, 'dt': 821},
        {'year': 1996, 'dt': 813},
        {'year': 1997, 'dt': 814},
        {'year': 1998, 'dt': 815},
        {'year': 1999, 'dt': 816},
        {'year': 2000, 'dt': 817},
        {'year': 2001, 'dt': 1461},
        {'year': 2002, 'dt': 1462},
        {'year': 2003, 'dt': 1722},
        {'year': 2004, 'dt': 2756},
        {'year': 2005, 'dt': 2797},
        {'year': 2006, 'dt': 2980}
    ]

    for d in link_dict:
        strike_count = 0
        for rid in range(1_500_000):
            print(f"Processing record {d['year']}-{rid}...")

            file_path = EXTERNAL_PATH / 'output' / 'motivation' / 'history' / str(d["year"]) / f"{d['year']}_{rid}.csv"
            if file_path.exists():
                continue

            url = f'https://aad.archives.gov/aad/record-detail.jsp?dt={d["dt"]}&rid={rid}'
            try:
                df = pd.read_html(url, flavor='bs4')[0]
            except:
                try:
                    df = pd.read_html(url, flavor='bs4')[0]
                except:
                    raise("Download error...")

            df.columns = ['field_title', 'value', 'meaning']
            if df['value'].isna().sum() == len(df):
                strike_count += 1
            else:
                strike_count = 0
                df.to_csv(file_path, index=False)
            if strike_count == 10:
                break


if __name__ == '__main__':
    download_csv()

