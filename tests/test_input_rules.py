from io import StringIO
import textwrap

from strava_gear.data import Component
from strava_gear.data import Rule
from strava_gear.data import Rules
from strava_gear.input.rules import read_rules


def rd(yaml, **kwargs):
    return read_rules(StringIO(textwrap.dedent(yaml.strip("\n"))), **kwargs)


def test_minimal():
    assert rd(
        """
        rules:
        - b1: {}
        """
    ) == Rules(
        bike_names={}, components=[],
        rules=[Rule(bikes={'b1': {}})]
    )


def test_aliases():
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
        rules=[Rule(bikes={'b1': {}, 'b2': {}})]
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
        rules=[Rule(bikes={'b1': {}})]
    )


def test_undeclared_components():
    assert rd(
        """
        rules:
        - b1:
            role: c1
            role2: c2
        """
    ) == Rules(
        bike_names={},
        components=[Component('c1', 'c1'), Component('c2', 'c2')],
        rules=[Rule(bikes={'b1': {'role': 'c1', 'role2': 'c2'}})]
    )

    assert rd(
        """
        rules:
        - b1:
            role: c1
        - b2:
            role: c1
        """
    ) == Rules(
        bike_names={},
        components=[Component('c1', 'c1')],
        rules=[Rule(bikes={'b1': {'role': 'c1'}}), Rule(bikes={'b2': {'role': 'c1'}})]
    )


# TODO: tests for validation
# TODO: exclusive components in bikes in one rule
