from io import StringIO
import textwrap

import pandas as pd  # type: ignore [import]

from strava_gear.data import Rule
from strava_gear.data import Rules
from strava_gear.rules_yaml import read_rules


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
        bike_names={}, components={},
        rules=[Rule(bikes={}, hashtags={}, since=pd.to_datetime(0, utc=True))]
    )
    assert rd(
        """
        rules:
        - b1:
            chain: hg53
        """
    ) == Rules(
        bike_names={}, components={},
        rules=[Rule(bikes={'b1': {'chain': 'hg53'}}, hashtags={}, since=pd.to_datetime(0, utc=True))]
    )

    # aliases
    assert rd(
        """
        aliases:
          city: b1
        rules:
        - city:
            chain: hg53
          b2:
            chain: hg73
        """
    ) == Rules(
        bike_names={'b1': "city"}, components={},
        rules=[Rule(
            bikes={'b1': {'chain': 'hg53'}, 'b2': {'chain': 'hg73'}},
            hashtags={}, since=pd.to_datetime(0, utc=True))]
    )
    assert rd(
        """
        aliases:
          city: b1
        rules:
        - town:
            chain: hg53
        """,
        aliases={'town': 'b1'}
    ) == Rules(
        # aliases from rules config override aliases from strava
        bike_names={'b1': "city"}, components={},
        rules=[Rule(bikes={'b1': {'chain': 'hg53'}}, hashtags={}, since=pd.to_datetime(0, utc=True))]
    )

    # TODO: tests for validation
