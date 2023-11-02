Common invocation flags:

    $ function strava-gear {
    >   command strava-gear --rules rules.yaml --csv - --tablefmt csv "$@"
    > }

Rules:

    $ cat >rules.yaml <<END
    > rules:
    >   - road:
    >       chain: c1
    >   - since: 2023-01-01
    >     road:
    >       chain: c2
    >   - since: 2024-01-01
    >     road:
    >       chain: c3
    > END

Bikes report:

    $ strava-gear <<END
    > name,gear_id,start_date,moving_time,distance
    > Ride 1,road,2022-01-01,3600,1000
    > Ride 2,road,2023-01-01,3600,1000
    > END
    bike,role,id,name,km,hour,first … last
    road,chain,c3,c3,0.0,0.0,never

Components report:

    $ strava-gear --report components <<END
    > name,gear_id,start_date,moving_time,distance
    > Ride 1,road,2022-01-01,3600,1000
    > Ride 2,road,2023-01-01,3600,1000
    > END
    id,name,km,hour,first … last
    c3,c3,0.0,0.0,never
    c1,c1,1.0,1.0,2022-01-01 … 2022-01-01
    c2,c2,1.0,1.0,2023-01-01 … 2023-01-01
