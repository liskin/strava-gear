from collections import defaultdict
from functools import partial
from typing import Dict
from typing import Iterable

from tabulate import tabulate

from .data import BikeId
from .data import Component
from .data import FirstLast
from .data import Result


def report_components(res: Result, only_assigned: bool) -> str:
    components: Iterable[Component] = sorted(res.components, key=lambda c: c.firstlast)
    if only_assigned:
        components = (c for c in components if c.assignment)
    return tabulate(
        [[c.ident, c.name, c.distance / 1000, c.time / 3600, c.firstlast] for c in components],
        headers=["id", "name", "km", "hour", "first … last"],
        floatfmt=".1f",
    )


def report_bikes(res: Result) -> str:
    bikes_firstlasts = bikes_firstlast(res)

    def sort_key(c: Component):
        assert c.assignment
        b, t = c.assignment

        return bikes_firstlasts[b], b, t

    def cols(c: Component):
        assert c.assignment
        b, t = c.assignment
        return [
            res.bike_names.get(b, b),
            t,
            c.ident,
            c.name,
            c.distance / 1000,
            c.time / 3600,
            c.firstlast
        ]

    return tabulate(
        [cols(c) for c in sorted((c for c in res.components if c.assignment), key=sort_key)],
        headers=["bike", "role", "id", "name", "km", "hour", "first … last"],
        floatfmt=".1f",
    )


def bikes_firstlast(res: Result) -> Dict[BikeId, FirstLast]:
    fl: Dict[BikeId, FirstLast] = defaultdict(FirstLast)
    for c in res.components:
        if c.assignment:
            b, _ = c.assignment
            fl[b] += c.firstlast
    return fl


reports = {
    'components': partial(report_components, only_assigned=True),
    'components-all': partial(report_components, only_assigned=False),
    'bikes': report_bikes,
}
