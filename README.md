# strava-gear

**Rule based tracker of gear and component wear primarily for [Strava][]**

[![PyPI Python Version badge](https://img.shields.io/pypi/pyversions/strava-gear)](https://pypi.org/project/strava-gear/)
[![PyPI Version badge](https://img.shields.io/pypi/v/strava-gear)](https://pypi.org/project/strava-gear/)
![License badge](https://img.shields.io/github/license/liskin/strava-gear)

Simple, yet powerful, local and open-source gear tracker for Strava. Uses
[strava-offline][] to keep a local database of activities and then reads
rules such as when a chain was replaced or that a certain hashtag means a
different set of wheels was used, and computes the wear (distance ridden and
time used) of all components.

Compared to [Strava My Gear][]:

* unlimited number of components
* tracks wear of components swapped from one bike to another, or taken off
  temporarily
* hashtags for temporary component changes (race wheels, bikepacking bags, …)
* easy and reversible editing of maintenance history (it's just YAML, and you
  can put it under [version control][git])

<!-- * TODO: alerts -->

[strava-offline]: https://github.com/liskin/strava-offline
[Strava]: https://strava.com/
[Strava My Gear]: https://support.strava.com/hc/en-us/articles/216918727-Adding-Gear-to-your-activities-on-Strava
[git]: https://git-scm.com/

## Example

```yaml
aliases:
  # optional, uses bike/shoes names from Strava by default
  city: "b123456"
  gravel: "b234567"
  road: "b345678"

components:
  # frames
  "Specialized Tarmac":                  # name same as id
  agang_sin_city: "Agang Sin City 2010"  # name specified
  specialized_diverge:                   # name specified as a property
    name: "Specialized Diverge Comp Carbon 2015"
    distance: 2000 km     # usage not tracked in the data (2nd hand components)
    hours: 100 h

  # tyres
  …

rules:
- since: 2010-01-01
  city:
    frame: agang_sin_city
    cranks: alfine_cranks
    derailleur_rear: deore_rd
- hashtag: "#graveldiverge"

# since/hashtag optional
# since default to epoch
# hashtag defaults to undefined
```
