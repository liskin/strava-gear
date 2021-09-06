import datetime
from typing import Dict

import jsonschema  # type: ignore [import]
import pandas as pd  # type: ignore [import]
import yaml

from .data import BikeId
from .data import BikeName
from .data import Component
from .data import ComponentId
from .data import ComponentName
from .data import Rule
from .data import Rules
from .data import prune_mapping

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
            'minItems': 1,
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


def process_component(k: str, v) -> Component:
    if v is None:
        return Component(ident=ComponentId(k), name=ComponentName(k))
    elif isinstance(v, str):
        return Component(ident=ComponentId(k), name=ComponentName(v))
    else:
        name = v.pop('name', k)
        return Component(ident=ComponentId(k), name=ComponentName(name), **v)  # TODO: parse distance/hours


def process_rule(r: Dict, aliases: Dict[BikeName, BikeId]) -> Rule:
    bikes = {}
    hashtags = {}
    kwargs = {}
    for k, v in r.items():
        if k == 'since':
            kwargs[k] = pd.to_datetime(v, utc=True)
        elif k.startswith('#'):
            hashtags[k] = v
        else:
            bikes[aliases.get(k, k)] = v

    return Rule(bikes=prune_mapping(bikes), hashtags=prune_mapping(hashtags), **kwargs)


def process_rules(c: Dict, aliases: Dict[BikeName, BikeId]) -> Rules:
    aliases = {**aliases, **c.get('aliases', {})}
    return Rules(
        bike_names={v: k for k, v in aliases.items()},
        components={k: process_component(k, v) for k, v in c.get('components', {}).items()},
        rules=[process_rule(r, aliases=aliases) for r in c.get('rules', [])],
    )


def read_rules(inp, aliases: Dict[BikeName, BikeId] = {}) -> Rules:
    c = yaml.safe_load(inp)
    jsonschema.validate(c, config_schema, format_checker=config_format_checker)
    return process_rules(c, aliases)
