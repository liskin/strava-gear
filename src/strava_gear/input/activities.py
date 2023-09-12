import csv
from os import PathLike
import sqlite3
from typing import Dict
from typing import List
from typing import Tuple
from typing import Union

from ..data import BikeId
from ..data import BikeName
from .date import parse_datetime

essential_columns = {
    'name',
    'gear_id',
    'start_date',
    'moving_time',
    'distance',
    'total_elevation_gain',
}


def read_input_csv(inp) -> Tuple[Dict[BikeName, BikeId], List[Dict]]:
    """
    Load activities from CSV generated from this command:

        sqlite3 ~/.local/share/strava_offline/strava.sqlite \
            ".mode csv" \
            ".headers on" \
            "SELECT name, gear_id, start_date, moving_time, distance, total_elevation_gain FROM activity" \
            >activities.csv
    """
    activities: List[Dict] = []
    for r in csv.DictReader(inp):
        assert essential_columns <= r.keys()
        activities.append({
            **r,
            'moving_time': int(r['moving_time']),
            'distance': float(r['distance']),
            'total_elevation_gain': float(r['total_elevation_gain']),
            'start_date': parse_datetime(r['start_date']),
        })

    return {}, activities


def read_strava_offline(db_filename: Union[str, PathLike]) -> Tuple[Dict[BikeName, BikeId], List[Dict]]:
    """
    Load activities from strava-offline database.
    """
    with sqlite3.connect(db_filename) as db:
        db.row_factory = sqlite3.Row

        aliases = {
            BikeName(r['name']): BikeId(r['id'])
            for r in db.execute('SELECT name, id FROM bike')
        }

        activities: List[Dict] = []
        for r in db.execute('SELECT * FROM activity'):
            assert essential_columns <= set(r.keys())
            activities.append({**r, 'start_date': parse_datetime(r['start_date'])})

        return aliases, activities
