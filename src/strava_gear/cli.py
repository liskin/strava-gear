import pandas as pd  # type: ignore [import]
import click


def read_input_csv(inp):
    """
    Loads activities from CSV generated from this command:

        sqlite3 ~/.local/share/strava_offline/strava.sqlite \
            ".mode csv" \
            ".headers on" \
            "SELECT gear_id, start_date, moving_time, distance FROM activity" \
            >activities.csv
    """
    return pd.read_csv(
        inp,
        usecols=['gear_id', 'start_date', 'moving_time', 'distance'],
        dtype={'gear_id': 'string', 'moving_time': 'int64', 'distance': 'float64'},
        parse_dates=['start_date'],
    )


@click.command()
@click.argument('input', type=click.File('r'), default='-')
def main(input):
    i = read_input_csv(input)
    print(i)
