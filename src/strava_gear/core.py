from itertools import accumulate
from itertools import chain
from typing import Dict
from typing import Iterable
from typing import List
from warnings import warn

from .data import Result
from .data import Rule
from .data import Rules
from .data import Usage


def apply_rules(rules: Rules, activities: List[Dict]) -> Result:
    rules_sorted = sorted(rules.rules, key=lambda r: r.since)
    activities_sorted = sorted(activities, key=lambda a: a['start_date'])

    # Determine the effective combinations of rules for all time points where rules change.
    # Rules effective at time T are obtained by combining all rules up to and including time T.
    effective_rules = list(accumulate(chain([Rule()], rules_sorted)))

    # Determine the effective rules for each activity by merging the two sorted series,
    # and tally up usage.
    usage = Usage()
    for activity, rule in merge_asof(activities_sorted, effective_rules):
        usage += usage_for_activity(activity, rule)

    # Fill computed data (usage, current assignment) into components.
    component_assignments = effective_rules[-1].component_assignments()
    components = [
        c.add_usage(usage).assign(component_assignments.get(c.ident, None))
        for c in rules.components
    ]

    return Result(bike_names=rules.bike_names, components=components)


def usage_for_activity(activity: Dict, rule: Rule) -> Usage:
    component_map = rule.bikes.get(activity['gear_id'], {})

    for hashtag in (s for s in activity['name'].split() if s.startswith('#')):
        component_map_ht = rule.hashtags.get(hashtag)
        if component_map_ht:
            component_map = {**component_map, **component_map_ht}

    return Usage.from_activity(
        components=component_map.values(),
        distance=activity['distance'],
        time=activity['moving_time'],
        ts=activity['start_date'])


def merge_asof(activities: Iterable[Dict], rules: Iterable[Rule]):
    rules = iter(rules)
    rule_cur = next(rules)
    rule_next = next(rules, None)
    for activity in activities:
        assert rule_cur.since <= activity['start_date']
        while rule_next is not None and rule_next.since <= activity['start_date']:
            rule_cur, rule_next = rule_next, next(rules, None)

        yield activity, rule_cur


def warn_unknown_bikes(rules: Rules, activities: List[Dict]) -> None:
    known_bikes = set(rules.bike_names.keys())
    used_gear = set(activity['gear_id'] for activity in activities if activity['gear_id'])

    unknown_bikes = rules.all_rule_bike_ids() - known_bikes - used_gear
    if unknown_bikes:
        warn(f"Unknown bikes in rules, possibly a typo: {unknown_bikes}")
