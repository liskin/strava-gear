from dataclasses import dataclass
from dataclasses import field
from dataclasses import replace
import datetime
from typing import Dict
from typing import List
from typing import Set

import jsonschema  # type: ignore [import]
import pandas as pd  # type: ignore [import]
import yaml

config_format_checker = jsonschema.FormatChecker()
config_schema = {
    'type': 'object',
    'additionalProperties': False,
    'properties': {
        'aliases': {
            'type': 'object',
            'additionalProperties': {'type': 'string'},
        },
        'components': {
            'type': 'object',
            'additionalProperties': {
                'oneOf': [
                    {'type': 'null'},
                    {'type': 'string'},
                    {
                        'type': 'object',
                        'additionalProperties': False,
                        'properties': {
                            'name': {'type': 'string'},
                            'distance': {'type': 'string'},
                            'hours': {'type': 'string'},
                        },
                    },
                ],
            }
        },
        'rules': {
            'type': 'array',
            'items': {
                'type': 'object',
                'properties': {
                    'since': {'format': 'datetime'},
                },
                'additionalProperties': {
                    'type': 'object',
                    'additionalProperties': {'type': 'string'},
                },
            },
        },
    },
}


@config_format_checker.checks('datetime')
def format_checker_datetime(d):
    return isinstance(d, (datetime.datetime, datetime.date,))


@dataclass(frozen=True)
class Component:
    ident: str  # TODO: do we need this?
    name: str
    distance: int = 0  # TODO: pint
    hours: int = 0


def process_component(k: str, v) -> Component:
    if v is None:
        return Component(ident=k, name=k)
    elif isinstance(v, str):
        return Component(ident=k, name=v)
    else:
        name = v.pop('name', k)
        return Component(ident=k, name=name, **v)  # TODO: parse distance/hours


@dataclass(frozen=True)
class Rule:
    bikes: Dict[str, Dict[str, str]] = field(default_factory=dict)
    hashtags: Dict[str, Dict[str, str]] = field(default_factory=dict)
    since: pd.Timestamp = pd.to_datetime(0, utc=True)

    def __add__(self, other):
        """
        Combine two rules. The second rule's since must be later than the first. Component mappings
        in the second rule then override those in the first. Additionally, components newly assigned
        to another bike are automatically removed from the old one.
        """
        if not isinstance(other, Rule):
            return NotImplemented
        if other.since < self.since:
            return NotImplemented

        other_components = {c for m in other.bikes.values() for c in m.values()}
        bikes = prune_mapping(update_mappings(filter_mapping(self.bikes, other_components), other.bikes))
        hashtags = prune_mapping(update_mappings(self.hashtags, other.hashtags))
        return replace(other, bikes=bikes, hashtags=hashtags)


def prune_mapping(m: Dict[str, Dict[str, str]]) -> Dict[str, Dict[str, str]]:
    """Prune mapping—drop null/None components and empty dicts."""
    return {a: b for a, b in ((a, {c: d for c, d in b.items() if d}) for a, b in m.items()) if b}


def update_mappings(
    m1: Dict[str, Dict[str, str]],
    m2: Dict[str, Dict[str, str]]
) -> Dict[str, Dict[str, str]]:
    """Override component mappings in m1 by those in m2."""
    return {a: {**m1.get(a, {}), **m2.get(a, {})} for a in m1.keys() | m2.keys()}


def filter_mapping(m: Dict[str, Dict[str, str]], f: Set[str]) -> Dict[str, Dict[str, str]]:
    """Remove components from a mapping."""
    return {a: {c: d for c, d in b.items() if d not in f} for a, b in m.items()}


def process_rule(r: Dict) -> Rule:
    bikes = {}
    hashtags = {}
    kwargs = {}
    for k, v in r.items():
        if k == 'since':
            kwargs[k] = pd.to_datetime(v, utc=True)
        elif k.startswith('#'):
            hashtags[k] = v
        else:
            bikes[k] = v

    return Rule(bikes=bikes, hashtags=hashtags, **kwargs)


@dataclass(frozen=True)
class Config:
    aliases: Dict[str, str]
    components: Dict[str, Component]
    rules: List[Rule]


def process_config(c: Dict) -> Config:
    return Config(
        aliases=c.get('aliases', []),
        components={k: process_component(k, v) for k, v in c.get('components', {}).items()},
        rules=[process_rule(r) for r in c.get('rules', [])],
    )


def read_config(inp) -> Config:
    c = yaml.safe_load(inp)
    jsonschema.validate(c, config_schema, format_checker=config_format_checker)
    return process_config(c)
