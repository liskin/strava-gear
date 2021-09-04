import datetime

import jsonschema  # type: ignore [import]
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


def read_config(inp):
    c = yaml.safe_load(inp)
    jsonschema.validate(c, config_schema, format_checker=config_format_checker)
    return c
