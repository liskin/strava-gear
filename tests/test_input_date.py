from datetime import datetime
from datetime import timezone

import pytest  # type: ignore [import]

from strava_gear.input.date import parse_datetime


def test_parse_datetime():
    assert parse_datetime('') is None

    with pytest.raises(ValueError):
        parse_datetime('x')

    with pytest.raises(ValueError):
        parse_datetime('123')

    assert parse_datetime('2021-01-01') == datetime(2021, 1, 1).astimezone()
    assert parse_datetime('2021-01-01T08:00:00') == datetime(2021, 1, 1, 8).astimezone()
    assert parse_datetime('2021-01-01T08:00:00Z') == datetime(2021, 1, 1, 8, tzinfo=timezone.utc).astimezone()
    assert parse_datetime('2021-01-01T08:00:00+02:00') == datetime(2021, 1, 1, 6, tzinfo=timezone.utc).astimezone()
