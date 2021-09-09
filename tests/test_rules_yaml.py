from datetime import datetime
from datetime import timezone
from io import StringIO
import textwrap

from strava_gear.data import Rule
from strava_gear.data import Rules
from strava_gear.rules_yaml import read_rules

epoch = datetime.fromtimestamp(0, timezone.utc)


def test_read_rules():
    def rd(yaml, **kwargs):
        return read_rules(StringIO(textwrap.dedent(yaml.strip("\n"))), **kwargs)

    # minimal config
    assert rd(
        """
        rules:
        - b1: {}
        """
    ) == Rules(
        bike_names={}, components=[],
        rules=[Rule(bikes={'b1': {}}, hashtags={}, since=epoch)]
    )

    # aliases
    assert rd(
        """
        aliases:
          city: b1
        rules:
        - city: {}
          b2: {}
        """
    ) == Rules(
        bike_names={'b1': "city"}, components=[],
        rules=[Rule(bikes={'b1': {}, 'b2': {}}, hashtags={}, since=epoch)]
    )
    assert rd(
        """
        aliases:
          city: b1
        rules:
        - town: {}
        """,
        aliases={'town': 'b1'}
    ) == Rules(
        # aliases from rules config override aliases from strava
        bike_names={'b1': "city"}, components=[],
        rules=[Rule(bikes={'b1': {}}, hashtags={}, since=epoch)]
    )

    # TODO: tests for validation
