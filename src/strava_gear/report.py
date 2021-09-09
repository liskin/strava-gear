from functools import partial
from typing import Iterable

from tabulate import tabulate

from .data import Component
from .data import Result


def report_components(res: Result, only_assigned: bool) -> str:
    components: Iterable[Component] = sorted(res.components, key=lambda c: c.firstlast)
    if only_assigned:
        components = (c for c in components if c.assignment is not None)
    return tabulate(
        [[c.ident, c.name, c.distance / 1000, c.time / 3600, c.firstlast] for c in components],
        headers=["id", "name", "distance (km)", "time (hour)", "first … last"],
        floatfmt=".1f",
    )


reports = {
    'components': partial(report_components, only_assigned=True),
    'components-all': partial(report_components, only_assigned=False),
}
