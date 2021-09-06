from dataclasses import dataclass
from dataclasses import field
from dataclasses import replace
from typing import Dict
from typing import List
from typing import NewType
from typing import Set

import pandas as pd  # type: ignore [import]

ComponentType = NewType('ComponentType', str)
ComponentId = NewType('ComponentId', str)
ComponentName = NewType('ComponentName', str)
BikeId = NewType('BikeId', str)
BikeName = NewType('BikeName', str)
HashTag = NewType('HashTag', str)

ComponentMap = Dict[ComponentType, ComponentId]


@dataclass(frozen=True)
class Component:
    ident: ComponentId  # TODO: do we need this?
    name: ComponentName
    distance: int = 0  # TODO: pint
    hours: int = 0


@dataclass(frozen=True)
class Rule:
    bikes: Dict[BikeId, ComponentMap] = field(default_factory=dict)
    hashtags: Dict[HashTag, ComponentMap] = field(default_factory=dict)
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


def prune_mapping(m: Dict[str, ComponentMap]) -> Dict[str, ComponentMap]:
    """Prune mapping—drop null/None components and empty dicts."""
    return {a: b for a, b in ((a, {c: d for c, d in b.items() if d}) for a, b in m.items()) if b}


def update_mappings(
    m1: Dict[str, ComponentMap],
    m2: Dict[str, ComponentMap]
) -> Dict[str, ComponentMap]:
    """Override component mappings in m1 by those in m2."""
    return {a: {**m1.get(a, {}), **m2.get(a, {})} for a in m1.keys() | m2.keys()}


def filter_mapping(m: Dict[str, ComponentMap], f: Set[str]) -> Dict[str, ComponentMap]:
    """Remove components from a mapping."""
    return {a: {c: d for c, d in b.items() if d not in f} for a, b in m.items()}


@dataclass(frozen=True)
class Rules:
    bike_names: Dict[BikeId, BikeName]
    components: Dict[ComponentId, Component]
    rules: List[Rule]
