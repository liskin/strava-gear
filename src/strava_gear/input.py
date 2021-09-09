import csv
from datetime import date
from datetime import datetime
from pathlib import Path
import sqlite3
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple
from typing import Union

import appdirs  # type: ignore [import]
import yaml

from .data import BikeId
from .data import BikeName


def read_input_csv(inp) -> List[Dict]:
    """
    Load activities from CSV generated from this command:

        sqlite3 ~/.local/share/strava_offline/strava.sqlite \
            ".mode csv" \
            ".headers on" \
            "SELECT name, gear_id, start_date, moving_time, distance FROM activity" \
            >activities.csv
    """
    activities: List[Dict] = []
    for r in csv.DictReader(inp):
        assert r.keys() <= {'name', 'gear_id', 'start_date', 'moving_time', 'distance'}
        activities.append({
            **r,
            'moving_time': int(r['moving_time']),
            'distance': float(r['distance']),
            'start_date': parse_datetime(r['start_date']),
        })

    return activities


def read_strava_offline() -> Tuple[Dict[BikeName, BikeId], List[Dict]]:
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

        activities: List[Dict] = []
        for r in db.execute('SELECT name, gear_id, start_date, moving_time, distance FROM activity'):
            assert set(r.keys()) <= {'name', 'gear_id', 'start_date', 'moving_time', 'distance'}
            activities.append({**r, 'start_date': parse_datetime(r['start_date'])})

        return aliases, activities


def parse_datetime(d: Optional[Union[str, date, datetime]]) -> Optional[datetime]:
    if isinstance(d, str):
        d = yaml.safe_load(d)  # yaml parses enough of ISO8601 for us

    if d is None:
        return d

    if isinstance(d, date):
        if not isinstance(d, datetime):
            d = datetime(d.year, d.month, d.day)
        return d.astimezone()
    else:
        raise ValueError
