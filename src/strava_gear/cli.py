from datetime import datetime
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import TextIO

import click
import platformdirs

from .core import apply_rules
from .core import warn_unknown_bikes
from .input.activities import essential_columns
from .input.activities import read_input_csv
from .input.activities import read_strava_offline
from .input.date import parse_datetime
from .input.rules import read_rules
from .report import Units
from .report import reports


class DateTimeParam(click.ParamType):
    name = "datetime"

    def convert(self, value, _param, _ctx):
        try:
            return parse_datetime(value)
        except ValueError as e:
            self.fail(str(e) or "Could not parse datetime")

    def get_metavar(self, _param):
        return "ISO8601"


@click.command(context_settings={'max_content_width': 120})
@click.option(
    '--rules', 'rules_input', type=click.File('r'),
    default=platformdirs.user_config_path(appname=__package__) / 'rules.yaml',
    show_default=True,
    help="Rules configuration (bikes, components, ...)")
@click.option(
    '--csv', type=click.File('r'),
    help=f"""
    Load activities from CSV instead of the strava-offline database
    (columns: {", ".join(sorted(essential_columns))})
    """)
@click.option(
    '--strava-database', type=click.Path(path_type=Path),  # type: ignore [type-var] # debian typeshed compat
    default=platformdirs.user_data_path(appname='strava_offline') / 'strava.sqlite',
    show_default=True,
    help="Location of the strava-offline database")
@click.option(
    '-o', '--output', type=click.File('w'), default='-', show_default=True,
    help="Output file")
@click.option(
    '-r', '--report', type=click.Choice(list(reports.keys())),
    default='bikes', show_default=True,
    help="Type of report")
@click.option(
    '-f', '--tablefmt', type=str, default='simple', show_default=True,
    help="""
    Table format, see <https://github.com/astanin/python-tabulate#table-format>.
    Additionally, "csv" is supported for CSV output.
    """)
@click.option(
    '--show-name/--hide-name', default=True, show_default=True,
    help="Show long component names")
@click.option(
    '--show-first-last/--hide-first-last', default=True, show_default=True,
    help="Show first/last usage of components")
@click.option(
    '--show-vert/--hide-vert', default=False, show_default=True,
    help="Show vertical (elevation gain)")
@click.option(
    '--show-retired/--hide-retired', default=False, show_default=True,
    help="Show retired bikes (on Strava)")
@click.option(
    '--units', type=click.Choice([u.name.lower() for u in Units]), default=Units.METRIC.name.lower(), show_default=True,
    callback=lambda _ctx, _param, v: Units[v.upper()],  # TODO: drop when Python 3.11 is the oldest supported
    help="Show data in metric or imperial")
@click.option(
    '--date-start', type=DateTimeParam(),
    help="Filter activities: start at or after the specified date(time)")
@click.option(
    '--date-end', type=DateTimeParam(),
    help="Filter activities: start before the specified date(time)")
def cli(
    rules_input: TextIO,
    csv: Optional[TextIO],
    strava_database: Path,
    output: TextIO,
    report: str,
    tablefmt: str,
    show_name: bool,
    show_first_last: bool,
    show_vert: bool,
    show_retired: bool,
    units: Units,
    date_start: Optional[datetime],
    date_end: Optional[datetime]
):
    if csv:
        input = read_input_csv(csv)
    else:
        input = read_strava_offline(strava_database)
    activities = activities_in_range(input.activities, date_start=date_start, date_end=date_end)
    rules = read_rules(rules_input, aliases=input.aliases)
    res = apply_rules(rules, activities)
    reports[report](
        res,
        output=output, tablefmt=tablefmt,
        show_name=show_name,
        show_first_last=show_first_last,
        show_vert=show_vert,
        show_bike=lambda b: show_retired or not input.bike_retired(b),
        units=units,
    )
    warn_unknown_bikes(rules, activities)


def activities_in_range(
    activities: List[Dict],
    date_start: Optional[datetime],
    date_end: Optional[datetime]
) -> List[Dict]:
    return [
        a
        for a in activities
        if not date_start or a['start_date'] >= date_start
        if not date_end or a['start_date'] < date_end
    ]
