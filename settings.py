""" Common technical settings. """

from pathlib import Path
from utilities import Output
import datetime as dt


# define common paths
CODE_PATH = Path(__file__).resolve()
DIR_PATH = CODE_PATH.parents[1]


if DIR_PATH == Path('/Users/malekseev'):
    DIR_PATH = Path('/Users/malekseev/Dropbox (Personal)/research/trade_dualuse')
elif DIR_PATH == Path('/export/home/doctoral/malekseev'):
    DIR_PATH = Path('/export/home/doctoral/malekseev/trade_dualuse')
else:
    raise("No path is selected")


DATA_PATH = DIR_PATH / 'data'
OUTPUT_PATH = DIR_PATH / 'output'
STATS_PATH = DIR_PATH / 'stats'
LOGS_PATH = DIR_PATH / 'logs'
EXTERNAL_PATH = Path('/Volumes/Seagate/trade_dualuse')


timelog = dt.datetime.now().strftime("%Y-%m-%d_%H-%M-%S.txt")

def write_log(x):
    """Print to standard output and log to a file."""
    print(x)
    # with Path(LOGS_PATH / f'joblog_{timelog}').open('a') as handler:
    #     handler.write(f'{dt.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")}: \t {x}\n')

output = Output(pipe=write_log)

