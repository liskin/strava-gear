import os

import appdirs  # type: ignore [import]
import click

from .core import apply_rules
from .input import read_input_csv
from .input import read_strava_offline
from .report import reports
from .rules_yaml import read_rules


@click.command(context_settings={'max_content_width': 120})
@click.option(
    '--rules', type=click.File('r'),
    default=os.path.join(appdirs.user_data_dir(appname=__package__), 'rules.yaml'),
    show_default=True,
    help="Rules configuration (bikes, components, …)")
@click.option(
    '--csv', type=click.File('r'),
    help="""
    Load activities from CSV instead of the strava-offline database
    (columns: name, gear_id, start_date, moving_time, distance)
    """)
@click.option(
    '--strava-database', type=click.Path(),
    default=os.path.join(appdirs.user_data_dir(appname='strava_offline'), 'strava.sqlite'),
    show_default=True,
    help="Location of the strava-offline database")
@click.option(
    '-o', '--output', type=click.File('w'), default='-', show_default=True,
    help="Output file")
@click.option(
    '-r', '--report', type=click.Choice(reports.keys()),
    default='bikes', show_default=True,
    help="Type of report")
@click.option(
    '-f', '--tablefmt', type=str, default='simple', show_default=True,
    help="Table format, see https://github.com/astanin/python-tabulate#table-format")
@click.option(
    '--show-name/--hide-name', default=True, show_default=True,
    help="Show long component names")
@click.option(
    '--show-first-last/--hide-first-last', default=True, show_default=True,
    help="Show first/last usage of components")
def main(rules, csv, strava_database, output, report, tablefmt, show_name, show_first_last):
    if csv:
        aliases, activities = {}, read_input_csv(csv)
    else:
        aliases, activities = read_strava_offline()
    rules = read_rules(rules, aliases=aliases)
    res = apply_rules(rules, activities)
    print(reports[report](res, tablefmt=tablefmt, show_name=show_name, show_first_last=show_first_last), file=output)


if __name__ == "__main__":
    main()
