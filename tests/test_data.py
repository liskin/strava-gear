import pytest  # type: ignore [import]

from strava_gear.data import Rule
from strava_gear.input.date import parse_datetime


def test_rule():
    assert Rule() + Rule() == Rule()

    # since taken from right
    assert Rule(since=parse_datetime('2020-01-01')) + Rule(since=parse_datetime('2020-01-02T01:00:00')) == \
        Rule(since=parse_datetime('2020-01-02T01:00:00'))
    with pytest.raises(TypeError):
        assert Rule(since=parse_datetime('2020-01-02')) + Rule(since=parse_datetime('2020-01-01T01:00:00')) == \
            Rule(since=parse_datetime('2020-01-01T01:00:00'))

    # neutral value on either side
    assert Rule(bikes={'b1': {'c': 'c1'}}) + Rule() == Rule(bikes={'b1': {'c': 'c1'}})
    assert Rule() + Rule(bikes={'b1': {'c': 'c1'}}) == Rule(bikes={'b1': {'c': 'c1'}})
    assert Rule(hashtags={'#b1': {'c': 'c1'}}) + Rule() == Rule(hashtags={'#b1': {'c': 'c1'}})
    assert Rule() + Rule(hashtags={'#b1': {'c': 'c1'}}) == Rule(hashtags={'#b1': {'c': 'c1'}})

    # pruning
    assert Rule(bikes={'b1': {}}) + Rule(bikes={}) == Rule(bikes={})
    assert Rule(bikes={}) + Rule(bikes={'b1': {}}) == Rule(bikes={})
    assert Rule(bikes={'b1': {'c': 'c1'}}) + Rule(bikes={'b1': {'c': None}}) == Rule(bikes={})
    assert Rule(hashtags={'#b1': {}}) + Rule(hashtags={}) == Rule(hashtags={})
    assert Rule(hashtags={}) + Rule(hashtags={'#b1': {}}) == Rule(hashtags={})
    assert Rule(hashtags={'#b1': {'c': 'c1'}}) + Rule(hashtags={'#b1': {'c': None}}) == Rule(hashtags={})

    # merging
    assert Rule(bikes={'b1': {'c': 'c1'}}) + Rule(bikes={'b1': {'d': 'd1'}}) == \
        Rule(bikes={'b1': {'c': 'c1', 'd': 'd1'}})
    assert Rule(bikes={'b1': {'c': 'c1'}}) + Rule(bikes={'b2': {'d': 'c2'}}) == \
        Rule(bikes={'b1': {'c': 'c1'}, 'b2': {'d': 'c2'}})
    assert Rule(hashtags={'#b1': {'c': 'c1'}}) + Rule(hashtags={'#b1': {'d': 'd1'}}) == \
        Rule(hashtags={'#b1': {'c': 'c1', 'd': 'd1'}})
    assert Rule(hashtags={'#b1': {'c': 'c1'}}) + Rule(hashtags={'#b2': {'d': 'c2'}}) == \
        Rule(hashtags={'#b1': {'c': 'c1'}, '#b2': {'d': 'c2'}})

    # overriding
    assert Rule(bikes={'b1': {'c': 'c1'}}) + Rule(bikes={'b1': {'c': 'c2'}}) == \
        Rule(bikes={'b1': {'c': 'c2'}})
    assert Rule(hashtags={'#b1': {'c': 'c1'}}) + Rule(hashtags={'#b1': {'c': 'c2'}}) == \
        Rule(hashtags={'#b1': {'c': 'c2'}})

    # moving a component from one bike to another…
    assert Rule(bikes={'b1': {'c': 'c1', 'd': 'd1'}}) + Rule(bikes={'b2': {'c': 'c1'}}) == \
        Rule(bikes={'b1': {'d': 'd1'}, 'b2': {'c': 'c1'}})

    # … hashtags aren't exclusive though
    assert Rule(hashtags={'#b1': {'c': 'c1', 'd': 'd1'}}) + Rule(hashtags={'#b2': {'c': 'c1'}}) == \
        Rule(hashtags={'#b1': {'c': 'c1', 'd': 'd1'}, '#b2': {'c': 'c1'}})

    # explicit component removal
    assert Rule(bikes={'b1': {'c': 'c1', 'd': 'd1'}}) + Rule(bikes={'b1': {'c': None}}) == \
        Rule(bikes={'b1': {'d': 'd1'}})
    assert Rule(hashtags={'#b1': {'c': 'c1', 'd': 'd1'}}) + Rule(hashtags={'#b1': {'c': None}}) == \
        Rule(hashtags={'#b1': {'d': 'd1'}})
