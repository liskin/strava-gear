from collections import defaultdict
import csv
from enum import Enum
from enum import auto
from functools import partial
from typing import Dict
from typing import Final
from typing import Iterator

from tabulate import tabulate

from .data import BikeId
from .data import Component
from .data import FirstLast
from .data import Result

MILE_IN_METERS: Final[float] = 1_609.344
FOOT_IN_METERS: Final[float] = 0.3048


class Units(Enum):
    METRIC = auto()
    IMPERIAL = auto()


def report(
    f,
    res: Result,
    output,
    tablefmt: str,
    show_name: bool,
    show_first_last: bool,
    show_vert: bool,
    units: Units,
):
    def cols(d: Dict) -> Dict:
        if not show_name:
            del d["name"]
        if not show_first_last:
            del d["first … last"]
        if units == Units.IMPERIAL:
            del d["km"]
            del d["vert m"]
            if not show_vert:
                del d["vert ft"]
        else:
            del d["mi"]
            del d["vert ft"]
            if not show_vert:
                del d["vert m"]
        return d

    table = [cols(d) for d in f(res)]
    if not table:
        return

    if tablefmt == 'csv':
        writer = csv.DictWriter(output, fieldnames=list(table[0].keys()), lineterminator='\n')
        writer.writeheader()
        writer.writerows(table)
    else:
        print(tabulate(table, headers="keys", floatfmt=".1f", tablefmt=tablefmt), file=output)


def report_components(res: Result) -> Iterator[Dict]:
    for c in sorted(res.components, key=lambda c: (c.firstlast, c.ident,)):
        yield {
            "id": c.ident,
            "name": c.name,
            "km": c.distance / 1000,
            "mi": c.distance / MILE_IN_METERS,
            "vert m": c.elevation_gain,
            "vert ft": c.elevation_gain / FOOT_IN_METERS,
            "hour": c.time / 3600,
            "first … last": c.firstlast,
        }


def report_bikes(res: Result) -> Iterator[Dict]:
    bikes_firstlasts = bikes_firstlast(res)

    def sort_key(c: Component):
        assert c.assignment
        b, t = c.assignment

        return bikes_firstlasts[b], b, t

    for c in sorted((c for c in res.components if c.assignment), key=sort_key):
        assert c.assignment
        b, t = c.assignment
        yield {
            "bike": res.bike_names.get(b, b),
            "role": t,
            "id": c.ident,
            "name": c.name,
            "km": c.distance / 1000,
            "mi": c.distance / MILE_IN_METERS,
            "vert m": c.elevation_gain,
            "vert ft": c.elevation_gain / FOOT_IN_METERS,
            "hour": c.time / 3600,
            "first … last": c.firstlast,
        }


def bikes_firstlast(res: Result) -> Dict[BikeId, FirstLast]:
    fl: Dict[BikeId, FirstLast] = defaultdict(FirstLast)
    for c in res.components:
        if c.assignment:
            b, _ = c.assignment
            fl[b] += c.firstlast
    return fl


reports = {
    'components': partial(report, report_components),
    'bikes': partial(report, report_bikes),
}
