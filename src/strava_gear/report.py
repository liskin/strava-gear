from collections import defaultdict
import csv
from functools import partial
from typing import Dict
from typing import Iterator

from tabulate import tabulate

from .data import BikeId
from .data import Component
from .data import FirstLast
from .data import Result


def report(f, res: Result, output, tablefmt: str, show_name: bool, show_first_last: bool):
    def cols(d: Dict) -> Dict:
        if not show_name:
            del d["name"]
        if not show_first_last:
            del d["first … last"]
        return d

    table = [cols(d) for d in f(res)]
    if not table:
        return

    if tablefmt == 'csv':
        writer = csv.DictWriter(output, fieldnames=list(table[0].keys()))
        writer.writeheader()
        writer.writerows(table)
    else:
        print(tabulate(table, headers="keys", floatfmt=".1f", tablefmt=tablefmt), file=output)


def report_components(res: Result) -> Iterator[Dict]:
    for c in sorted(res.components, key=lambda c: c.firstlast):
        yield {
            "id": c.ident,
            "name": c.name,
            "km": c.distance / 1000,
            "hour": c.time / 3600,
            "first … last": c.firstlast,
        }


def report_bikes(res: Result, show_names: bool = True, show_firstlasts: bool = True) -> Iterator[Dict]:
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
