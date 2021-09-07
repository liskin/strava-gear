from itertools import accumulate
from itertools import chain

import pandas as pd  # type: ignore [import]

from .data import Result
from .data import Rule
from .data import Rules
from .data import Usage


def apply_rules(rules: Rules, activities) -> Result:
    rules_sorted = sorted(rules.rules, key=lambda r: r.since)
    activities_sorted = activities.sort_values('start_date')

    # Determine the effective combinations of rules for all time points where rules change.
    # Rules effective at time T are obtained by combining all rules up to and including time T.
    effective_rules = pd.DataFrame(accumulate(chain([Rule()], rules_sorted)))

    # Determine the effective rules for each activity by merging the two sorted series.
    activities_rules = pd.merge_asof(activities_sorted, effective_rules, left_on='start_date', right_on='since')

    # Tally up usage.
    usage = activities_rules.apply(usage_for_activity, axis=1).sum()

    return Result(
        bike_names=rules.bike_names,
        bikes=effective_rules['bikes'].iat[-1],
        components=[c.add_usage(usage) for c in rules.components])


def usage_for_activity(activity) -> Usage:
    component_map = activity['bikes'].get(activity['gear_id'], {})

    for hashtag in (s for s in activity['name'].split() if s.startswith('#')):
        component_map_ht = activity['hashtags'].get(hashtag)
        if component_map_ht:
            component_map = {**component_map, **component_map_ht}

    return Usage.from_activity(
        components=component_map.values(),
        distance=activity['distance'],
        time=activity['moving_time'],
        ts=activity['start_date'])
