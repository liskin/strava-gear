from datetime import date
from datetime import datetime
from typing import Optional
from typing import Union

from dateutil.parser import isoparse


def parse_datetime(d: Optional[Union[str, date, datetime]]) -> Optional[datetime]:
    if d is None or d == '':
        return None

    if isinstance(d, str):
        d = isoparse(d)

    if isinstance(d, date):
        if not isinstance(d, datetime):
            d = datetime(d.year, d.month, d.day)
        return d.astimezone()
    else:
        raise ValueError
