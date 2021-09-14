from io import StringIO
import textwrap

from jsonschema.exceptions import ValidationError  # type: ignore [import]
import pytest  # type: ignore [import]

from strava_gear.data import Component
from strava_gear.data import Rule
from strava_gear.data import Rules
from strava_gear.input.rules import check_component_duplicities
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


def test_validation():
    with pytest.raises(ValidationError) as e:
        rd("rules: []")
    assert "[] is too short" in str(e.value)
    assert list(e.value.absolute_path) == ['rules']

    with pytest.raises(ValidationError) as e:
        rd("xxx:")
    assert "unexpected" in str(e.value)
    assert list(e.value.absolute_path) == []

    with pytest.raises(Exception) as e:
        rd("rules: [{b1: {r1: c1, r2: c1}}]")
    assert "Duplicate components" in str(e.value)

    # TODO: more tests for validation


def test_check_component_duplicities():
    assert check_component_duplicities() == set()

    assert check_component_duplicities(hashtags={'#a': {'r1': 'c1'}}) == set([])
    assert check_component_duplicities(hashtags={'#a': {'r1': 'c1'}, '#b': {'r1': 'c1'}}) == set([])
    assert check_component_duplicities(hashtags={'#a': {'r1': 'c1', 'r2': 'c1'}}) == set(['c1'])

    assert check_component_duplicities(bikes={'b1': {'r1': 'c1'}}) == set([])
    assert check_component_duplicities(bikes={'b1': {'r1': 'c1', 'r2': 'c1'}}) == set(['c1'])
    assert check_component_duplicities(bikes={'b1': {'r1': 'c1'}, 'b2': {'r1': 'c1'}}) == set(['c1'])
