from dataclasses import dataclass
import datetime
from typing import Dict
from typing import List
from typing import Optional

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
                    'hashtag': {'type': 'string', 'format': 'hashtag'},
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


@config_format_checker.checks('hashtag')
def format_checker_hashtag(s: str):
    return s.startswith('#')


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
    bikes: Dict[str, Dict[str, str]]
    since: pd.Timestamp = pd.to_datetime(0, utc=True)
    hashtag: Optional[str] = None


def process_rule(r: Dict) -> Rule:
    kwargs = {}
    if 'since' in r:
        kwargs['since'] = pd.to_datetime(r.pop('since'), utc=True)
    if 'hashtag' in r:
        kwargs['hashtag'] = r.pop('hashtag')
    return Rule(bikes=r, **kwargs)


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
