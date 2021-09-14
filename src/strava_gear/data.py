from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass
from dataclasses import field
from dataclasses import replace
from datetime import datetime
from datetime import timezone
from functools import total_ordering
from itertools import chain
from typing import Dict
from typing import Iterable
from typing import Iterator
from typing import List
from typing import NewType
from typing import Optional
from typing import Set
from typing import Tuple
from typing import TypeVar

T = TypeVar('T')

ComponentRole = NewType('ComponentRole', str)
ComponentId = NewType('ComponentId', str)
ComponentName = NewType('ComponentName', str)
BikeId = NewType('BikeId', str)
BikeName = NewType('BikeName', str)
HashTag = NewType('HashTag', str)

Meters = NewType('Meters', float)
Seconds = NewType('Seconds', float)

ComponentMap = Dict[ComponentRole, ComponentId]
Mapping = Dict[T, ComponentMap]
ComponentAssignment = Tuple[BikeId, ComponentRole]


@total_ordering
@dataclass(frozen=True)
class FirstLast:
    _fl: Optional[Tuple[datetime, datetime]] = None

    @staticmethod
    def from_ts(ts: datetime):
        return FirstLast(_fl=(ts, ts,))

    @property
    def first(self):
        return self._fl[0] if self._fl is not None else None

    @property
    def last(self):
        return self._fl[1] if self._fl is not None else None

    def __add__(self, other) -> FirstLast:
        if not isinstance(other, FirstLast):
            return NotImplemented

        if self._fl is None:
            return other
        elif other._fl is None:
            return self
        else:
            return FirstLast(_fl=(min(self.first, other.first), max(self.last, other.last)))

    def __lt__(self, other) -> bool:
        if not isinstance(other, FirstLast):
            return NotImplemented

        if self._fl is None:
            return True
        elif other._fl is None:
            return False
        else:
            return self.last < other.last \
                or (self.last == other.last and self.first < other.first)

    def __str__(self) -> str:
        if self._fl is None:
            return "never"
        else:
            return f"{self.first.date()} … {self.last.date()}"


@dataclass(frozen=True)
class Component:
    ident: ComponentId
    name: ComponentName
    distance: Meters = Meters(0.0)
    time: Seconds = Seconds(0.0)
    firstlast: FirstLast = FirstLast()
    assignment: Optional[ComponentAssignment] = None

    def add_usage(self, usage: Usage) -> Component:
        return replace(
            self,
            distance=self.distance + usage.distances.get(self.ident, 0),
            time=self.time + usage.times.get(self.ident, 0),
            firstlast=self.firstlast + usage.firstlasts.get(self.ident, FirstLast()))

    def assign(self, assignment: Optional[ComponentAssignment]) -> Component:
        return replace(self, assignment=assignment)


@dataclass(frozen=True)
class Rule:
    bikes: Mapping[BikeId] = field(default_factory=dict)
    hashtags: Mapping[HashTag] = field(default_factory=dict)
    since: datetime = datetime.fromtimestamp(0, timezone.utc)

    def __add__(self, other) -> Rule:
        """
        Combine two rules. The second rule's since must be equal or later than the first.
        Component mappings in the second rule then override those in the first.
        Additionally, components newly assigned to another bike are automatically removed
        from the old one.
        """
        if not isinstance(other, Rule):
            return NotImplemented
        if other.since < self.since:
            return NotImplemented

        other_components = {c for m in other.bikes.values() for c in m.values()}
        bikes = prune_mapping(update_mappings(filter_mapping(self.bikes, other_components), other.bikes))
        hashtags = prune_mapping(update_mappings(self.hashtags, other.hashtags))
        return replace(other, bikes=bikes, hashtags=hashtags)

    def component_assignments(self) -> Dict[ComponentId, ComponentAssignment]:
        return {c: (b, t) for b, m in self.bikes.items() for t, c in m.items()}

    def all_component_ids(self) -> Iterator[ComponentId]:
        """Return all component ids mentioned in this rule."""
        for m in chain(self.bikes.values(), self.hashtags.values()):
            for c in m.values():
                if c is not None:
                    yield c


def prune_mapping(m: Mapping[T]) -> Mapping[T]:
    """Prune mapping—drop null/None components and empty dicts."""
    return {a: b for a, b in ((a, {c: d for c, d in b.items() if d}) for a, b in m.items()) if b}


def update_mappings(m1: Mapping[T], m2: Mapping[T]) -> Mapping[T]:
    """Override component mappings in m1 by those in m2."""
    return {a: {**m1.get(a, {}), **m2.get(a, {})} for a in m1.keys() | m2.keys()}


def filter_mapping(m: Mapping[T], f: Set[T]) -> Mapping[T]:
    """Remove components from a mapping."""
    return {a: {c: d for c, d in b.items() if d not in f} for a, b in m.items()}


@dataclass(frozen=True)
class Rules:
    bike_names: Dict[BikeId, BikeName]
    components: List[Component]
    rules: List[Rule]

    def all_rule_bike_ids(self) -> Set[BikeId]:
        return set(b for r in self.rules for b in r.bikes.keys())


@dataclass
class Usage:
    distances: Dict[ComponentId, float] = field(default_factory=lambda: defaultdict(float))
    times: Dict[ComponentId, float] = field(default_factory=lambda: defaultdict(float))
    firstlasts: Dict[ComponentId, FirstLast] = field(default_factory=lambda: defaultdict(FirstLast))

    @staticmethod
    def from_activity(components: Iterable[ComponentId], distance: float, time: float, ts: datetime):
        fl = FirstLast.from_ts(ts)
        return Usage(
            distances={c: distance for c in components},
            times={c: time for c in components},
            firstlasts={c: fl for c in components})

    def __iadd__(self, other) -> Usage:
        if not isinstance(other, Usage):
            return NotImplemented
        for k, d in other.distances.items():
            self.distances[k] += d
        for k, t in other.times.items():
            self.times[k] += t
        for k, fl in other.firstlasts.items():
            self.firstlasts[k] += fl
        return self


@dataclass(frozen=True)
class Result:
    bike_names: Dict[BikeId, BikeName]
    components: List[Component]
