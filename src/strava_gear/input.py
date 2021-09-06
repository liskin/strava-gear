from pathlib import Path
import sqlite3
from typing import Dict
from typing import Tuple

import appdirs  # type: ignore [import]
import pandas as pd  # type: ignore [import]

from .data import BikeId
from .data import BikeName


def read_input_csv(inp) -> pd.DataFrame:
    """
    Load activities from CSV generated from this command:

        sqlite3 ~/.local/share/strava_offline/strava.sqlite \
            ".mode csv" \
            ".headers on" \
            "SELECT name, gear_id, start_date, moving_time, distance FROM activity" \
            >activities.csv
    """
    return pd.read_csv(
        inp,
        usecols=['name', 'gear_id', 'start_date', 'moving_time', 'distance'],
        parse_dates=['start_date'],
    )


def read_strava_offline() -> Tuple[Dict[BikeName, BikeId], pd.DataFrame]:
    """
    Load activities from strava-offline database.
    """
    db_filename = Path(appdirs.user_data_dir(appname='strava_offline')) / 'strava.sqlite'
    with sqlite3.connect(db_filename) as db:
        db.row_factory = sqlite3.Row

        aliases = {
            BikeName(r['name']): BikeId(r['id'])
            for r in db.execute('SELECT name, id FROM bike')
        }
        activities = pd.read_sql_query(
            'SELECT name, gear_id, start_date, moving_time, distance FROM activity',
            db,
            parse_dates=['start_date'],
        )
        return aliases, activities
