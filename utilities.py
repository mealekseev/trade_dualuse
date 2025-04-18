"""Commonly used functions"""

import time
import contextlib


def prepare_matplotlib():
    import matplotlib.pyplot as plt

    plt.rcParams['font.sans-serif'] = 'Avenir'
    plt.rcParams['font.family'] = 'sans-serif'
    plt.rcParams['font.size'] = 16
    plt.rcParams['figure.figsize'] = (10, 6)
    plt.rcParams['savefig.dpi'] = 400
    color_dict = {}
    color_dict['emerald'] = (0 / 255, 94 / 255, 128 / 255)
    color_dict['magenta'] = (135 / 255, 0 / 255, 57 / 255)
    color_dict['granite'] = (94 / 255, 94 / 255, 94 / 255)
    color_dict['forest'] = (0 / 255, 60 / 255, 28 / 255)
    color_dict['purple'] = (81 / 255, 0 / 255, 76 / 255)
    plt.rcParams['axes.prop_cycle'] = plt.cycler(color=[
        color_dict['emerald'],
        color_dict['magenta'],
        color_dict['granite'],
        color_dict['purple'],
        color_dict['forest']
    ])
    return color_dict


def merge_df(
        df1, df2, how='outer', left_on='', right_on='', on='', keep_merge=True, throw_error='', silent=False, **kwargs
):
    import pandas as pd
    from settings import output

    if on != '':
        left_on = on
        right_on = on

    df = pd.merge(df1, df2, how='outer', left_on=left_on, right_on=right_on, indicator=True, **kwargs)
    if not silent:
        output(df['_merge'].value_counts())
    if (
        (throw_error == 'left' and 'left_only' in df['_merge'].unique())
        or (throw_error == 'right' and 'right_only' in df['_merge'].unique())
        or (throw_error == 'unmatched' and ('left_only' in df['_merge'].unique()
                                            or 'right_only' in df['_merge'].unique()))
        or (throw_error == 'matched' and 'both' in df['_merge'].unique())
    ):
        raise ValueError("Bad match")
    if how == 'inner':
        df = df[df['_merge'] == 'both']
    if how == 'left':
        df = df[df['_merge'].isin(['left_only', 'both'])]
    if how == 'right':
        df = df[df['_merge'].isin(['both', 'right_only'])]
    if keep_merge:
        return df
    df.drop('_merge', axis=1, inplace=True)
    return df


def format_seconds(seconds):
    """Prepare a number of seconds to be displayed as a string."""
    hours, remainder = divmod(int(round(seconds)), 60**2)
    minutes, seconds = divmod(remainder, 60)
    return f'{hours:02}:{minutes:02}:{seconds:02}'


class Output:
    """Configurable output pipeline."""

    def __init__(self, pipe=print):
        self.pipe = pipe

    def __call__(self, x=""):
        self.pipe(x)

    def set(self, pipe):
        """Set the output pipeline."""
        self.pipe = pipe

    @contextlib.contextmanager
    def clock(self, x1, x2=None):
        """Output information about how long something takes."""
        self(x1)
        start = time.time()
        yield lambda: time.time() - start
        end = time.time()
        if x2 is None:
            self(f"Finished after {format_seconds(end - start)}.")
        else:
            self(f"{x2} after {format_seconds(end - start)}.")


def divide_robust(v):
    w = v.copy()
    w[w == 0] = 1
    return w


def power_robust(a, b):
    v = a.copy()
    v[v == 0] = 1
    res = v ** b
    res[a == 0] = 0
    return res

